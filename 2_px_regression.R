library(tidyr)
library(dplyr)
library(parallel)
library(tibble)
library(data.table)

dat <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df1.Rds")
#dat <- dat %>% sample_frac(0.1)# For testing
dat <- dat %>% select(id, agb_2018, agb_2019, agb_2020, agb_2021, agb_2022, agb_2023,
                      grid, lc_2015) %>%
  filter(lc_2015 %in% c(1,2,3,4,5,9))

# Calculate t1 and t2
dat$t1 <- (dat$agb_2018+dat$agb_2019)/2
dat$t2 <- (dat$agb_2022+dat$agb_2023)/2
names(dat) <- sub('^agb_', '', names(dat)) # remove "agb" from column names
dat <- as.tibble(dat)

# Regression of agb against time
# Define the number of splits
n_splits <- 10 # also denotes number of cores used for parallel processing

# Split the dataframe into n parts
dat_split <- split(dat, rep(1:n_splits, length.out = nrow(dat)))
# Elongate data
format_fn <- function(df) {
  df <- df %>%
    pivot_longer(starts_with("20"))
  df <- df %>%
    mutate(name = as.integer(name))
  return(df)
}

# Define a function to perform regression and extract statistics
# regression_fn <- function(df) {
#   df <- as.data.table(df)
#   results <- df[, {
#     model <- lm(value ~ name)
#     summary_mod <- summary(model)
#     coef_mod <- coef(summary_mod)
#     list(intercept = coef_mod[1, 1],
#          slope = coef_mod[2, 1],
#          rsq = summary_mod$r.squared,
#          error = coef_mod[2, 2],
#          error_intercept = coef_mod[1, 2],
#          p_slope = coef_mod[2, 4],
#          p_intercept = coef_mod[1, 4])
#   }, by = id]
#   return(results)
# }

regression_fn <- function(df) {
  df <- as.data.table(df)
  results <- df[, {
    model <- lm(value ~ name)
    summary_mod <- summary(model)
    coef_mod <- coef(summary_mod)
    
    intercept <- coef_mod[1, 1]
    slope <- coef_mod[2, 1]
    
    list(
      intercept = intercept,
      slope = slope,
      rsq = summary_mod$r.squared,
      error = coef_mod[2, 2],
      error_intercept = coef_mod[1, 2],
      p_slope = coef_mod[2, 4],
      p_intercept = coef_mod[1, 4],
      pred_2018 = intercept + slope * 2018 # New column for prediction at 2018
    )
  }, by = id]
  
  return(results)
}


# Use parallel processing with mclapply
n_cores <- n_splits
dat_split <- mclapply(dat_split, format_fn, mc.cores = n_cores)
results <- mclapply(dat_split, regression_fn, mc.cores = n_cores)

# Combine all the results into one dataframe
final_result <- bind_rows(results)

# Merge coeffs with agb into one dataframe
df2 <- final_result %>%
  select(id, slope, intercept, rsq, error, error_intercept, p_slope, p_intercept, pred_2018) %>%
  inner_join(dat, by = "id")

rm(dat);rm(dat_split);rm(final_result);rm(results);gc()

#df2 <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df2.Rds") # Import df2, use after df2 has been exported

# Define densification
# den <- df2 %>% filter(t1 > 10 & slope > 1) %>%  # Add land cover map filtering...  lc_2015 %in% c(1,2,3) &
#   mutate(den = TRUE) %>%
#   select(x,y,den) %>%
#   as_tibble()
# 
# aden <- df2 %>% filter(t1 > 10 & slope < -1) %>% # anti- densificaiton
#   mutate(aden = TRUE) %>%
#   select(x,y,aden) %>%
#   as_tibble()

# Export df2
saveRDS(df2, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df2.Rds")

# Export rasters
# library(terra)
# r <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/2015_resampled.tif")
# 
# den_r <- rast(den, type="xyz", crs = r, extent = r)
# writeRaster(den_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/den.tif")
# 
# aden_r <- rast(aden, type="xyz", crs = r, extent = r)
# writeRaster(aden_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/aden.tif")
