library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Load dataframe
df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df4_analysis_ready.Rds")

## Woody encroachment

# Cut number
# Function to generate grouped summaries and plots ########ENCROACHABLE PX ONLY NO ANTI WE
we_plot <- function(data, variable, n_bins, x_label) {
  # Create a temporary column for the binned variable
  data1 <- data %>%
    filter(encble) %>%
    mutate(binned_var = cut_number({{ variable }}, n_bins))
  
  data2 <- data %>%
    mutate(binned_var = cut_number({{ variable }}, n_bins))
  
  # Group by the binned variable and summarize
  summary_data1 <- data1 %>%
    group_by(binned_var) %>%
    summarise(
      r.we = sum(we == 1) / n(),
      r.a.we = sum(we == -1) / n(),
      r.no.change = sum(we == 0) / n(),
      n.NA = sum(is.na(we)),
      n = sum(!is.na(we)),
      se = sqrt(r.we*(1-r.we)/n),
      .groups = "drop"
    )
  summary_data2 <- data2 %>%
    group_by(binned_var) %>%
    summarise(
      r.we = sum(we == 1) / n(),
      r.a.we = sum(we == -1) / n(),
      r.no.change = sum(we == 0) / n(),
      n.NA = sum(is.na(we)),
      n = sum(!is.na(we)),
      a.se = sqrt(r.a.we*(1-r.a.we)/n),
      .groups = "drop"
    )
  
  # Create the plot
  ggplot(summary_data1, aes(x = binned_var, y = r.we)) +
    geom_point() +
    geom_errorbar(aes(ymin = r.we-se, ymax = r.we+se), color = "#d7191c", width = 0.2)+
    #geom_text(aes(label = round(n), y = 0.05, x = binned_var), colour = "red", size = 3) +
    labs(x = x_label, y = "Rate of WE (%)") +
    #xlim(min(binned_var), max(binned_var)) +
    #geom_point(data = summary_data2, aes(y = r.a.we), colour = "brown") +
    #geom_errorbar(data = summary_data2, aes(ymin = r.a.we-a.se, ymax = r.a.we+a.se), color = "#d7191c", width = 0.2)+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}


# Generate plots for all variables
t1 <- we_plot(df, t1, 10, expression("AGB at t1 (Mg ha"^-1))
#fire <- we_plot(df, fire, 2, "Fire Presence (False/ True)")
doy <- we_plot(df, doy, 10, "Mean Day of Year of Burning (2001-2023)")
ff <- we_plot(df, ff, 10, "Fire Frequency (2001-2023)")
fire_trend <- we_plot(df, fire_trend, 10, "Fire Trend")
map <- we_plot(df, map, 10, "Annual Precipitation (mm)")
precip_trend <- we_plot(df, precip_trend, 10, "Rainfall Trend 1982-2023 (mm/year)")
urban <- we_plot(df, urban, 10, "Distance to Urban (km)")
roads <- we_plot(df, roads, 10, "Distance to Roads (km)")
#agric_p <- we_plot(df, agric_p, 2, "Proportion of Agriculture in 1km2 grid")


# Display
we_grid <- grid.arrange(arrangeGrob(t1, fire, doy, ff, fire_trend, map,
                                    precip_trend, urban, roads, agric_p,
                                    nrow = 3))

ggsave("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/5_figures/we_bivars.png", we_grid, width = 40, height = 22.5, units = "cm", dpi = 300)

## Densification
# Function to generate grouped summaries and plots
den_plot <- function(data, variable, n_bins, x_label) {
  # Create a temporary column for the binned variable
  data <- data %>%
    filter(densifble) %>%
    mutate(binned_var = cut_number({{ variable }}, n_bins))
  
  # Group by the binned variable and summarize
  summary_data <- data %>%
    group_by(binned_var) %>%
    summarise(
      r.den = sum(den == 1) / n(),
      r.a.den = sum(den == -1) / n(),
      r.no.change = sum(den == 0) / n(),
      n.NA = sum(is.na(den)),
      n = sum(!is.na(den)),
      se = sqrt(r.den*(1-r.den)/n),
      .groups = "drop"
    )
  
  # Create the plot
  ggplot(summary_data, aes(x = binned_var, y = r.den)) +
    geom_point() +
    geom_errorbar(aes(ymin = r.den-se, ymax = r.den+se), color = "#d7191c", width = 0.2)+
    #geom_text(aes(label = round(n), y = 0.05, x = binned_var), colour = "red", size = 3, angle = 90)+
    #geom_point(aes(y = r.a.den), colour = "burlywood3") +
    labs(x = x_label, y = "Rate of Densification (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}


# Generate plots for all variables
t1 <- den_plot(df, t1, 10, expression("AGB at t1 (Mg ha"^-1))
#fire <- den_plot(df, fire, 2, "Fire Presence (False/ True)")
doy <- den_plot(df, doy, 10, "Mean Day of Year of Burning (2001-2023)")
ff <- den_plot(df, ff, 10, "Fire Frequency (2001-2023)")
fire_trend <- den_plot(df, fire_trend, 10, "Fire Trend (2001-2023)")
map <- den_plot(df, map, 10, "Annual Precipitation (mm)")
precip_trend <- den_plot(df, precip_trend, 10, "Rainfall Trend 1982-2023 (mm/year)")
urban <- den_plot(df, urban, 10, "Distance to Urban (km)")
roads <- den_plot(df, roads, 10, "Distance to Roads (km)")
#agric_p <- den_plot(df, agric_p, 2, "Proportion of Agriculture in 1km2 grid")

# Display
den_grid <- grid.arrange(arrangeGrob(t1, fire, doy, ff, fire_trend, map,
                                     precip_trend, urban, roads, agric_p,
                                     nrow = 3))

ggsave("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/5_figures/den_bivars.png", den_grid, width = 40, height = 24.5, units = "cm", dpi = 300)
