library(terra)
library(dplyr)

setwd("/exports/geos.ed.ac.uk/landteam/N/chris_WE")

# Import rasters
L <- list() #load data here
#L$agb_2015 <- rast("0_data/1_resamp_reproj/2015_resampled.tif")
#L$agb_2016 <- rast("0_data/1_resamp_reproj/2016_resampled.tif")
#L$agb_2017 <- rast("0_data/1_resamp_reproj/2017_resampled.tif")
L$agb_2018 <- rast("0_data/1_resamp_reproj/2018_resampled.tif")
L$agb_2019 <- rast("0_data/1_resamp_reproj/2019_resampled.tif")
L$agb_2020 <- rast("0_data/1_resamp_reproj/2020_resampled.tif")
L$agb_2021 <- rast("0_data/1_resamp_reproj/2021_resampled.tif")
L$agb_2022 <- rast("0_data/1_resamp_reproj/2022_resampled.tif")
L$agb_2023 <- rast("0_data/1_resamp_reproj/2023_resampled.tif")
# L$agb_2018 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2018_resampled.tif")
# L$agb_2019 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2019_resampled.tif")
# L$agb_2020 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2020_resampled.tif")
# L$agb_2021 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2021_resampled.tif")
# L$agb_2022 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2022_resampled.tif")
# L$agb_2023 <- rast("0_data/1_resamp_reproj/AGB_merged_minscan2023_resampled.tif")
L$scan_2018 <- rast("0_data/1_resamp_reproj/countmask_year_2018_resampled.tif")
L$scan_2019 <- rast("0_data/1_resamp_reproj/countmask_year_2019_resampled.tif")
L$scan_2020 <- rast("0_data/1_resamp_reproj/countmask_year_2020_resampled.tif")
L$scan_2021 <- rast("0_data/1_resamp_reproj/countmask_year_2021_resampled.tif")
L$scan_2022 <- rast("0_data/1_resamp_reproj/countmask_year_2022_resampled.tif")
L$scan_2023 <- rast("0_data/1_resamp_reproj/countmask_year_2023_resampled.tif")
L$grid <- rast("0_data/1_resamp_reproj/grid_1km_resampled.tif")

# Covars
L$lc_2015 <- rast("0_data/1_resamp_reproj/LC2015_resampled.tif")
L$agric <- rast("0_data/1_resamp_reproj/def_binary_resampled.tif")
L$map <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/bioclim_precip_resampled.tif")
L$precip_trend <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/Precipitation_Trend_1982_2023_resampled.tif")
L$tpi <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/SRTM_mTPI_resampled.tif")
L$urban <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/dist_urbanUInt32_resampled.tif")
L$markets <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/markets_UInt32_resampled.tif")
L$roads <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/roads_UInt32_resampled.tif")
L$buildings <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/pop_3km_UInt16_resampled.tif")
L$wedge_in <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/wedge_in_dist_resampled.tif")
L$wedge_out <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/wedge_out_dist_resampled.tif")
L$ff <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/ff_mean_resampled.tif")
L$doy <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/doy_mean_resampled.tif")
L$fire_trend <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/avg_number_fires_per_year_5yrs_resampled.tif", lyrs=1)

RSt <- rast(L)
print("Rasters in")

a <- Sys.time()
df <- terra::as.data.frame(RSt, xy= TRUE)
df <- df %>% filter(agb_2018 < 70 &
                      agb_2019 < 70 &
                      agb_2020 < 70 &
                      agb_2021 < 70 &
                      agb_2022 < 70 &
                      agb_2023 < 70 &
                      scan_2018 > 4 &
                      scan_2019 > 4 &
                      scan_2020 > 4 &
                      scan_2021 > 4 &
                      scan_2022 > 4 &
                      scan_2023 > 4 &
                        lc_2015 %in% c(1,2,3,4,5,6,9)) %>%
  mutate(id = row_number())
print("dataframe done")
saveRDS(df, "0_data/2_dataframes/df1.Rds")
print("done")
Sys.time()-a