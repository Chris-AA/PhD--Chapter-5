# Bivariate plots of all variables

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
err = 3
# Load dataframe
df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df4_analysis_ready.Rds")

## Woody encroachment
# Filter to encroachable cells
we_df <- df %>%
  filter(encble == TRUE & abs(t1-t2) > err) %>%
  mutate(we_fac = if_else(we == 1, 1, 0),
         we_fac = as.logical(we_fac))

# make a function to find def rate for bins of x
rate_by_x <- function(x_name, dat, nbins = 10){
  x <- dat[[x_name]]
  # find good breaks in x simple would be cut_number(), but that fails for pop. Use BAMMtools::getJenksBreaks for more sophiticated version
  brk <- BAMMtools::getJenksBreaks(x, k = nbins, subset = 10000)
  brk <- brk[!duplicated(brk)] # even jenks breaks can fail if the data are very unbalanced
  dat$bins <- cut(x, breaks = brk, dig.lab = 1)
  #dat$bins <- forcats::fct_explicit_na(dat$bins)
  
  # Fire rate
  dat_sum1 <- dat %>%
    group_by(bins) %>%
    summarise(
      n_encble = sum(encble),
      n_we = sum(we_fac),
      we = n_we/n_encble*100
      #sd = sd(we_1km)
    )
  
  dat_sum1 %>%
    filter(!is.na(dat_sum1$bins)) %>%
    ggplot(aes(x = bins, y = we)) +
    geom_point() +
    geom_line(group = 1) + # group?
    #geom_errorbar(aes(ymin = mean_we-sd, ymax = mean_we+sd), color = "#d7191c")+
    #geom_text(aes(label = round(), y = 0.1, x = bins), colour = "red", nudge_y = 1, size = 3) +
    theme_classic()
}

# run function for one var e.g.:
rate_by_x("pred_2018", we_df)

# run for a set of vars (subset the vars you want)
var_names <- c("t1", "agric_p", "flam_lc_p","map", "precip_trend", "tpi", "urban", "markets", "roads",
               "buildings2_log", "wedge_in", "wedge_out", "ff", "doy", "fire_trend")

plts1 <- lapply(var_names, rate_by_x, dat = we_df, nbins = 10)
cowplot::plot_grid(plotlist = plts1, ncol = 3, labels= var_names)

#bivar1 <- cowplot::plot_grid(plotlist = plts1, ncol = 2, labels= var_names)
#bivar1
#ggsave("", bivar1, width = 20, height = 20, units = "cm", dpi = 300)

## Densification
# Filter to 'densifiable' cells
den_df <- df %>%
  filter(densifble == TRUE) %>%
  mutate(den_reclass = if_else(den == 1, 1, 0),
         den_reclass = as.factor(den_reclass))

# make a function to find def rate for bins of x
rate_by_x <- function(x_name, dat, nbins = 10){
  x <- dat[[x_name]]
  # find good breaks in x simple would be cut_number(), but that fails for pop. Use BAMMtools::getJenksBreaks for more sophiticated version
  brk <- BAMMtools::getJenksBreaks(x, k = nbins, subset = 10000)
  brk <- brk[!duplicated(brk)] # even jenks breaks can fail if the data are very unbalanced
  dat$bins <- cut(x, breaks = brk, dig.lab = 1)
  #dat$bins <- forcats::fct_explicit_na(dat$bins)
  
  # Fire rate
  dat_sum1 <- dat %>%
    group_by(bins) %>%
    summarise(
      n_densifble = sum(densifble),
      n_den = sum(den_fac),
      den = n_den/n_densifble*100
      #sd = sd(we_1km)
    )
  
  dat_sum1 %>%
    filter(!is.na(dat_sum1$bins)) %>%
    ggplot(aes(x = bins, y = den)) +
    geom_point() +
    geom_line(group = 1) + # group?
    #geom_errorbar(aes(ymin = mean_we-sd, ymax = mean_we+sd), color = "#d7191c")+
    #geom_text(aes(label = round(), y = 0.1, x = bins), colour = "red", nudge_y = 1, size = 3) +
    theme_classic()
}

# run function for one var e.g.:
rate_by_x("pred_2018", den_df)

# run for a set of vars (subset the vars you want)
var_names <- c("pred_2018", "agric_p", "flam_lc_p","map", "precip_trend", "tpi", "urban", "markets", "roads",
               "buildings2", "wedge_in", "wedge_out", "ff", "doy", "fire_trend")

plts2 <- lapply(var_names, rate_by_x, dat = den_df, nbins = 10)
cowplot::plot_grid(plotlist = plts2, ncol = 3, labels= var_names)

#bivar1 <- cowplot::plot_grid(plotlist = plts1, ncol = 2, labels= var_names)
#bivar1
#ggsave("", bivar1, width = 20, height = 20, units = "cm", dpi = 300)