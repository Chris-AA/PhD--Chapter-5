library(tidyr)
library(dplyr)
library(terra)

df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df2.Rds")

# Define parameters
#max_agb = 60   # maximum agb value
bgw = 14       # threshold of grassland:wooded land
err = 3        # error margin

# Format df
#df <- df %>%
  #filter(t1 < max_agb & t2 < max_agb) %>% # Remove high agb values
  #filter(p_slope < 0.06) %>% # filter to significant slope and intercepts
  #rename(px_id = id)
 # add var which counts number of rows for each grid value###########################
 # filter to complete or near complete grids #######################

df_filtered <- df %>%
  group_by(grid) %>%
  filter(n() >= 8) %>%
  ungroup()

# Define change classes
df <- df %>% mutate(we = case_when(t1 < bgw & t2 > bgw & (t2-t1) > err ~ 1,     # woody encroachment
                    t1 > bgw & t2 < bgw & (t2-t1) < -err ~ -1),                 # anti-woody encroachment
                    den = case_when(t1 > bgw & slope > 0 & p_slope <= 0.05 ~ 1,  # densification
                    t1 > bgw & slope < 0 & p_slope <= 0.05 ~ -1),              # anti-densification
                    encble = case_when(t1 < bgw ~ TRUE,
                                       t1 > bgw ~ FALSE),                       # encroachable
                    densifble = case_when(t1 > bgw ~ TRUE,
                                          t1 < bgw ~ FALSE))                    # densifiable                                

# Summarise densification and woody encroachment by grid cell id
# df2 <- df %>% group_by(grid) %>%
#   summarise(we_1km = mean(we), # need to count 1ha pix
#             den_1km = mean(den),
#             agric_1km = sum(agric))

# df2 <- df %>%
#   group_by(grid) %>%
#   summarise(
#     # Calculate the proportion of TRUE and NA in encrchble
#     encble_1km = case_when(
#       mean(encble, na.rm = TRUE) > 0.5 ~ TRUE,  # if >50% of rows are TRUE
#       mean(is.na(encble)) > 0.5 ~ FALSE,           # if >50% of rows are NA
#      # TRUE ~ NA                                   # default case
#     ),
#     densifble_1km = case_when(
#       mean(densifble, na.rm = TRUE) > 0.5 ~ TRUE,  # if >50% of rows are TRUE
#       mean(is.na(densifble)) > 0.5 ~ FALSE,           # if >50% of rows are NA
#       # TRUE ~ NA                                   # default case
#     ),
#     # Calculate the proportion of 1's in we, if encr_1km is TRUE
#     we_1km = ifelse(encble_1km == TRUE, 
#                     mean(we == 1) > 0.5,  # if >50% of we values are 1
#                     FALSE),                   # else FALSE
#     den_1km = ifelse(densifble_1km == TRUE, 
#                     mean(den == 1) > 0.5,  # if >50% of we values are 1
#                     FALSE)                   # else FALSE
#   )
df2 <- df %>%
  group_by(grid) %>%
  summarise(
    total_count = n(),                    # Total number of pixels in each grid
    na_count_we = sum(is.na(we)),         # Number of NAs in each grid
    we_1km = mean(we, na.rm = TRUE),      # Mean of 'we', ignoring NAs
  ) 
  #filter(na_count_we < 0.5 * total_count) # Keep only grid cells with less than 50% NAs


df3 <- df %>%
  group_by(grid) %>%
  summarise(
    total_count = n(),                    # Total number of pixels in each ####### CHECK DOESNT REMOVE NAs
    na_count_den = sum(is.na(den)),           # Number of NAs in each grid
    den_1km = mean(den, na.rm = TRUE)     # Mean of 'we', ignoring NAs
  ) 
  #filter(na_count_den < 0.5 * total_count)    # Keep only grid cells with less than 50% NAs

# Attach df2 vars to df
df4 <- df2 %>%
  full_join(df3, by = "grid")
df5 <- df %>%
  full_join(df4, by = "grid")

# Save df3
#saveRDS(df5, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df3.Rds")

# Dataframe to rasters
we <- df5 %>% select(x,y,we_1km) %>%
  as_tibble()
# we_grow <- df3 %>% filter(we_1km > 0)
# we_decl <- df3 %>% filter(we_1km < 0)

den <- df5 %>% select(x,y,den_1km) %>%
  as_tibble()
# Write rasters
r <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/2015_resampled.tif")

we_r <- rast(we, type="xyz", crs = crs(r))
writeRaster(we_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/we.tif", overwrite = TRUE)

den_r <- rast(den, type="xyz", crs = crs(r))
writeRaster(den_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/den.tif", overwrite = TRUE)

 
