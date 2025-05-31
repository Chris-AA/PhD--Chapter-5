library(dplyr)
library(tidyr)
library(ggplot2)

# Load dataframe
df <- readRDS("/exports/geos.ed.ac.uk/landteam/N/chris_WE/0_data/2_dataframes/df4_analysis_ready.Rds")

# t1 WE with anti
df %>%
  group_by(cut_width(t1, 2)) %>% summarise(
  r.we = sum(we==1)/length(we),
  r.a.we = sum(we==-1)/length(we),
  r.no.change = sum(we==0)/length(we),
  n.NA = sum(is.na(we)),
  n = sum(!is.na(we))
) %>% ggplot(
  aes(x = `cut_width(t1, 2)`, y = r.we))+
  geom_point() +
  geom_point(aes(y = r.a.we), colour = "brown")

# Fire prevalence
df %>% filter(encble) %>%
  group_by(cut_width(fire, 1)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(fire, 1)`, y = r.we))+
  geom_point()

# DOY
df %>% filter(encble) %>%
  group_by(cut_number(doy, 10)) %>% summarise(
    r.we = sum(we==1, na.rm= TRUE)/length(we),
    r.a.we = sum(we==-1, na.rm= TRUE)/length(we),
    r.no.change = sum(we==0, na.rm= TRUE)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we)),
    se = sqrt(r.we*(1-r.we)/n)
  ) %>% ggplot(
    aes(x = `cut_number(doy, 10)`, y = r.we))+
  geom_point() +
  geom_errorbar(aes(ymin = r.we-se, ymax = r.we+se), color = "#d7191c", width = 0.2)+
  geom_text(aes(label = round(n), y = 0.05, x = `cut_number(doy, 10)`), colour = "red", size = 3) +
  geom_point(aes(y = r.a.we), colour = "brown")

# FF
df %>% filter(encble) %>%
  group_by(cut_width(ff, 0.1)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(ff, 0.1)`, y = r.we))+
  geom_point()
  #geom_point(aes(y = r.a.we), colour = "brown")

# Fire trend
df %>% filter(encble) %>%
  group_by(cut_width(fire_trend, 0.1)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(fire_trend, 0.1)`, y = r.we))+
  geom_point()

# Annual precip
df %>% filter(encble) %>%
  group_by(cut_width(map, 50)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(map, 50)`, y = r.we))+
  geom_point()

# Rainfall trend
df %>% filter(encble) %>%
  group_by(cut_width(precip_trend, 0.015)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(precip_trend, 0.015)`, y = r.we))+
  geom_point()

# Urban
df %>% filter(encble) %>%
  group_by(cut_width(urban, 15)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(urban, 15)`, y = r.we))+
  geom_point()

# Roads
df %>% filter(encble) %>%
  group_by(cut_width(roads, 15)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(roads, 15)`, y = r.we))+
  geom_point()

# Proportion of agric
df %>% filter(encble) %>%
  group_by(cut_width(agric_p, 0.1)) %>% summarise(
    r.we = sum(we==1)/length(we),
    r.a.we = sum(we==-1)/length(we),
    r.no.change = sum(we==0)/length(we),
    n.NA = sum(is.na(we)),
    n = sum(!is.na(we))
  ) %>% ggplot(
    aes(x = `cut_width(agric_p, 0.1)`, y = r.we))+
  geom_point()

# t1 Densification with anti
df %>% filter(densifble)%>%
  group_by(cut_width(t1, 2)) %>% summarise(
    r.den = sum(den==1)/length(den),
    r.a.den = sum(den==-1)/length(den),
    r.no.change = sum(den==0)/length(den),
    n.NA = sum(is.na(den)),
    n = sum(!is.na(den))
  ) %>% ggplot(
    aes(x = `cut_width(t1, 2)`, y = r.den))+
  geom_point() +
  geom_point(aes(y = r.a.den), colour = "brown")

