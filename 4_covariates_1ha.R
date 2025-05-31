library(tidyr)
library(dplyr)
# Import data
df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df3.Rds") %>%
  select(id, x, y, t1, t2, we, den, we_1km, den_1km,encble, densifble, pred_2018, lc_2015.x, agric, map, precip_trend,
         tpi, urban, markets, roads, buildings, wedge_in, wedge_out, ff, doy, fire_trend, grid, "2018", "2023", error) %>%
  rename(lc_2015 = lc_2015.x)

# Format data
df$buildings2 <- df$buildings
df$buildings2[df$buildings2==0] <- min(df$buildings2[df$buildings2!=0])

df2 <- df %>%
  filter(ff >= 0 & ff <= 23 & doy >= 100 & doy <= 350 & buildings < 7200) %>%
  mutate(
    buildings2 = buildings2/9, # Format buildings to per km2
    buildings2_log = log10(buildings2),
    roads = roads/1000,
    markets = markets/1000,
    urban = urban/1000,
    wedge_in = wedge_in/1000,
    wedge_out = wedge_out/1000,
    lc_2015 = as.factor(lc_2015),
    flam_lc = case_when(lc_2015 == 4 ~ 1,
                        lc_2015 == 5 ~ 1,
                        lc_2015 == 6 ~ 1),
    flam_lc = replace_na(flam_lc, 0),
    ff = ff/23,
    fire = case_when(ff > 0 ~ 1,
                     ff == 0 ~ 0)
  )

# Calc proportion of agric per grid
df3 <- df2 %>%
  group_by(grid) %>%
  summarise(
    total_count = n(),
    agric_count = sum(agric),
    agric_p = agric_count/total_count,
    flam_lc_count = sum(flam_lc),
    flam_lc_p = flam_lc_count/total_count)

df4 <- df2 %>%
  full_join(df3, by = "grid") %>%
  select(id, x, y, t1, t2, we, den, encble, densifble, agric_p, flam_lc_p, map, precip_trend,
         tpi, urban, markets, roads, buildings2_log, wedge_in, wedge_out, ff, doy, fire_trend, fire, "2018", "2023")


saveRDS(df4, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df4_analysis_ready.Rds")

# # Create dataframes for bivariate plots and RF models
# we_df <- df %>%
#   filter(encble == TRUE) %>%
#   mutate(we = replace_na(we, 0)) # NA values from df should be encroachable that weren't encroached, set to 0
# 
# den_df <- df %>% filter(densifble == TRUE) %>%
#   mutate(den = replace_na(den, 0))
