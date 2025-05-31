library(tidyr)
library(dplyr)
library(terra)
library(ggplot2)
library(gridExtra)
df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df4_analysis_ready.Rds")
err = 3

## Filter data
we_df <- df %>%
  filter(encble == TRUE) #& abs(t1-t2) > err)
awe_df <- df %>%
  filter(we == -1)

den_df <- df %>%
  filter(densifble == TRUE)
aden_df <- df %>%
  filter(den == -1)

## Summarise area of WE and DEN at 1ha level
# Initialise dataframe
df2 <- data.frame(
  process_1 = factor(c("Woody Encroachment", "Woody Encroachment", "Densification", "Densification"), levels = c("Woody Encroachment", "Densification")),
  process_2 = factor(c("Gain", "Anti", "Gain", "Anti"), levels = c("Gain", "Anti")))


# Change as a proportion of encroachable/ densifiable land (Rate of change)
we <- (sum(we_df$we)/length(we_df$we))*100 # woody encroachment as a proportion of encroachable land
awe <- (length(awe_df$we)/sum(!df$encble, na.rm = TRUE))*100
den<- (sum(den_df$den)/length(den_df$den))*100 # densification as a proportion of woodland
aden <- (length(aden_df$we)/length(den_df$den))*100
df2$rate <- c(we, awe, den, aden)

we_se <- sqrt((we/100)*(1-(we/100))/length(we_df$we))
awe_se <- sqrt((awe/100)*(1-(awe/100))/length(awe_df$we))
den_se <- sqrt((den/100)*(1-(den/100))/length(den_df$den))
aden_se <- sqrt((aden/100)*(1-(aden/100))/length(aden_df$den))
df2$prop_se <- c(we_se*100, awe_se*100, den_se*100, aden_se*100)

# Areal extent (ha)
we_a <- sum(we_df$we)
awe_a <- length(awe_df$we)
den_a <- sum(den_df$den)
aden_a <- length(aden_df$we)
df2$area_1ha <- c(we_a, awe_a, den_a, aden_a)

# AGB difference
we_1 <- we_df %>%
  filter(we == 1 )

den_1 <- den_df %>%
  filter(den == 1 )

we_agb <- sum(we_1$"2023") - sum(we_1$"2018")
awe_agb <- sum(awe_df$"2018") - sum(awe_df$"2023") 
den_agb <- sum(den_1$"2023") - sum(den_1$"2018")
aden_agb <- sum(aden_df$"2018") - sum(aden_df$"2023")
df2$agb_diff <- c(we_agb, awe_agb, den_agb, aden_agb)
df2$agb_diff <- df2$agb_diff/1000000 # Mg to Tg

# C per hectare
we_agbha <- df2$agb_diff[1]/length(we_df$we)
awe_agbha <- df2$agb_diff[2]/length(awe_df$we)
den_agbha <- df2$agb_diff[3]/length(den_df$den)
aden_agbha <- df2$agb_diff[4]/length(aden_df$den)
df2$agb_ha <- c(we_agbha, awe_agbha, den_agbha, aden_agbha)

## Plot proportional area and C per hectare
p_area <- ggplot(df2, aes(x = as.factor(process_1), y = rate, fill = process_2)) + 
  geom_bar(stat = "identity", position = position_dodge()) +  # Plot the bars
  #geom_errorbar(aes(ymin = rate - prop_se, ymax = rate + prop_se), width = 0.2, position = position_dodge(0.9)) +  # Add error bars
  scale_y_continuous(limits = c(0, 20)) +  # Set y-axis starting from 10, NA lets ggplot automatically handle the upper limit
  scale_fill_manual(values = c("#abd9e9", "#fdae61")) +
  xlab("Process") + 
  ylab("Proportional rate of change 2018- 2023 (%)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14))

AGB <- ggplot(df2, aes(x = as.factor(process_1), y = agb_diff, fill = process_2)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +  # Plot the bars
  xlab("Process") + 
  ylab(expression("Gain or Loss in Above-Ground Biomass (Tg)")) +
  theme_classic() +
  scale_fill_manual(values = c("#abd9e9", "#fdae61")) +
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      axis.title.x  = element_text(size = 14),
      axis.title.y  = element_text(size = 14))


g_legend <- function(plot){
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # function to extract legend

mylegend <- g_legend(AGB) # extract legend

AGB <- ggplot(df2, aes(x = as.factor(process_1), y = agb_diff, fill = process_2)) + 
  geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +  # Plot the bars
  xlab("Process") + 
  ylab(expression("Gain or Loss in Above-Ground Biomass (Tg)")) +
  theme_classic() +
  scale_fill_manual(values = c("#abd9e9", "#fdae61")) +
  theme(
        legend.position = "none",
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14))

grid <- grid.arrange(arrangeGrob(p_area, AGB, nrow=1), mylegend, nrow = 2, heights=c(10,1))
ggsave("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/5_figures/rq1.png", grid, width = 40, height = 20, units = "cm", dpi = 300)

##### Dataframe to rasters ######
we_tib <- we_df %>% select(x,y,we) %>%
  as_tibble()

den_tib <- den_df %>% select(x,y,den) %>%
  as_tibble()

# Write rasters
r <- rast("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/1_resamp_reproj/2018_resampled.tif")

we_r <- rast(we_tib, type="xyz", crs = crs(r))
writeRaster(we_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/we_fac.tif", overwrite = TRUE)

den_r <- rast(den_tib, type="xyz", crs = crs(r))
writeRaster(den_r, "/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/3_rasters/den_fac.tif", overwrite = TRUE)




####################################################################################

# # Identify 1km2 grids with positive signal (flag1) and grids with signal over 0.3 (flag2)
# we_df <- df %>%
#   filter(encble == TRUE) %>%
#   mutate(we = replace_na(we, 0),
#          we_flag1 = if_else(we_1km > 0, 1, if_else(we_1km == 0, 0, -1)),
#          we_flag2 = if_else(we_1km > 0.3, 1,
#                             if_else(we_1km >= -0.3 & we_1km <= 0.3, 0, -1)), # WE in grids with positive signal over 0.3
#          we_fac = case_when(we_flag2 == 1 & we == 1 ~ 1,
#                             we == 0 ~ 0)) %>% # we_flag1 <= 0 here to control for noise? Results in small amount of 0s
#   filter(!is.na(we_fac))
# 
# den_df <- df %>%
#   filter(densifble == TRUE) %>%
#   mutate(den = replace_na(den, 0),
#          den_flag1 = if_else(den_1km > 0, 1, if_else(den_1km == 0, 0, -1)),
#          den_flag2 = if_else(den_1km > 0.3, 1,
#                             if_else(den_1km >= -0.3 & den_1km <= 0.3, 0, -1)),  #den in grids with positive signal over 0.3
#          den_fac = case_when(den_flag2 == 1 & den == 1 ~ 1,
#                              den == 0 ~ 0)) %>% ## NOT MANY 0s ##
#   filter(!is.na(den_fac))