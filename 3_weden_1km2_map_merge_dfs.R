library(tidyr)
library(dplyr)
library(terra)

df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df2.Rds")

# Define parameters
#max_agb = 60   # maximum agb value
bgw = 14       # threshold of grassland:wooded land
err = 3        # error margin

df_filtered <- df %>%
  group_by(grid) %>%
  filter(n() >= 8) %>%
  ungroup()

# Define change classes
df <- df %>% mutate(we = case_when(t1 <= bgw & t2 >= bgw & (t2-t1) > err ~ 1, # woody encroachment
                                   t1 > bgw & t2 < bgw & (t2-t1) < -err ~ -1, # anti-woody encroachment
                                   abs(t2-t1) <= err ~ 0,
                                   t1 >= bgw & t2 >= bgw ~ 0,
                                   t1 <= bgw & t2 <= bgw ~ 0),
                    den = case_when(t1 > bgw & slope > 0 & p_slope <= 0.05 ~ 1,  # densification
                                    t1 > bgw & slope <= 0 & p_slope <= 0.05 ~ -1, # anti-densification
                                    t1 > bgw & p_slope > 0.05 ~ 0,
                                    ),              # anti-densification
                    encble = case_when(t1 < bgw ~ TRUE,#########################
                                       t1 > bgw ~ FALSE),                       # encroachable
                    densifble = case_when(t1 > bgw ~ TRUE,
                                          t1 < bgw ~ FALSE))                    # densifiable                                
# Aggregate to 1km2 grid
df2 <- df %>%
  group_by(grid) %>%
  summarise(
    total_count = n(),                    # Total number of pixels in each grid
    na_count_we = sum(is.na(we)),         # Number of NAs in each grid
    encble_count = sum(encble),
    we_1km = mean(we, na.rm = TRUE),      # Mean of 'we', ignoring NAs
  )  %>%
  mutate(encble_fac = case_when(encble_count >= 1 ~ TRUE)) # Identifying grids with 1 or more "encroachable pixels as encroachable

df3 <- df %>%
  group_by(grid) %>%
  summarise(
    total_count = n(),                    # Total number of pixels in each ####### CHECK DOESNT REMOVE NAs
    na_count_den = sum(is.na(den)),          # Number of NAs in each grid
    densifble_count = sum(densifble),
    den_1km = mean(den, na.rm = TRUE)     # Mean of 'we', ignoring NAs
  ) %>%
  mutate(densifble_fac = case_when(densifble_count >= 1 ~ TRUE))

# # Initialise dataframe
# summary_df <- data.frame(
#   process_1 = factor(c("WE", "WE","DEN", "DEN"), levels = c("WE", "DEN")),
#   process_2 = factor(c("WE", "AWE", "DEN", "ADEN"), levels = c("WE", "AWE", "DEN", "ADEN"))
# )
# 
# # Areal extent (1km2) - Define WE, AWE, DEN, ADEN at 1km2
# we_1 <- df2 %>% 
#   filter(we_1km > 0)
# awe_1 <- df2 %>%
#   filter(we_1km < 0)
# den_1 <- df3 %>%
#   filter(den_1km > 0)
# aden_1 <- df3 %>%
#   filter(den_1km < 0)
# summary_df$area_1km2 <- c(length(we_1$we_1km), length(awe_1$we_1km), length(den_1$den_1km), length(aden_1$den_1km))
# 
# # Change as a proportion of encroachable/ densifiable land
# we_prop <- (length(we_1$we_1km)/sum(df2$encble_fac, na.rm = TRUE))*100 # woody encroachment as a proportion of encroachable land, depends on value on line 40
# awe_prop <- (length(awe_1$we_1km)/sum(df2$encble_fac, na.rm = TRUE))*100
# den_prop <- (length(den_1$den_1km)/sum(df3$densifble_fac, na.rm = TRUE))*100
# aden_prop <- (length(aden_1$den_1km)/sum(df3$densifble_fac, na.rm = TRUE))*100
# summary_df$prop <- c(we_prop, awe_prop, den_prop, aden_prop)


###### Join WE and D dataframes to df1 with covars ######
# Attach df2 vars to df
df4 <- df2 %>%
  full_join(df3, by = "grid")
df5 <- df %>%
  full_join(df4, by = "grid")

# Attach xy from df1
dat <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df1.Rds") %>%
  select(id, x, y,lc_2015, agric, map, precip_trend, tpi, urban, markets, roads, buildings, wedge_in, wedge_out, ff, doy, fire_trend)
df6 <- df5 %>%
  full_join(dat, by = "id")

# Save df3
saveRDS(df6, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df3.Rds")

# ##### Dataframe to rasters ######
# we_tib <- df6 %>% select(x,y,we_1km) %>%
#   as_tibble()
# 
# den_tib <- df6 %>% select(x,y,den_1km) %>%
#   as_tibble()
# 
# # Write rasters
# r <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/2018_resampled.tif")
# 
# we_r <- rast(we_tib, type="xyz", crs = crs(r))
# writeRaster(we_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/we_1km.tif", overwrite = TRUE)
# 
# den_r <- rast(den_tib, type="xyz", crs = crs(r))
# writeRaster(den_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/den_1km.tif", overwrite = TRUE)
