## Plotting ##

## Packages ----

library(tidyverse)
library(Hmisc)

## Load & clean data ----

# load dataset
coord_data <- read_csv("raw_data/HW_Provisioning_Data.csv")

# clean dataset
coord_data$NestID <- as.factor(coord_data$NestID)
coord_data$site <- as.factor(coord_data$site)
coord_data$trt <- as.factor(coord_data$trt)
coord_data$stage <- as.factor(coord_data$stage)
coord_data$year <- as.numeric(coord_data$year)

## Section 1: Total Visits ----

# Plot 1: tot_visits ~ site

coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = site, y = tot_visits, col = site, fill = site)) +
  geom_jitter(width = 0.15, size = 4, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black",
               linewidth = 1.5, size = 1.5, pch = 21) +
  ylab("Total number of visits") +
  xlab("Site") +
  theme_minimal() +
  theme(legend.position = c(0.9,0.9))

# Plot 2: tot_visits ~ site + trt + stage, excluding rural site

coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = site, y = tot_visits, col = trt, fill = trt, group = trt)) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black",
               linewidth = 1.5, size = 1.5, pch = 21,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(stage)) +
  ylab("Total number of visits") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.1,0.86))

# Plot 3: tot_visits ~ site + trt + stage

coord_data %>%
  #filter(site != "R") %>% # remove if you want to see rural
  ggplot(aes(x = site, y = tot_visits, col = trt, fill = trt, group = trt)) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black",
               linewidth = 1.5, size = 1.5, pch = 21,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(stage)) +
  ylab("Total number of visits") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.1,0.86))


# Plot 4: tot_visits ~ year + trt + site

coord_data %>%
  filter(site != "R") %>% # remove if you want to see rural
  ggplot(aes(x = year, y = tot_visits, col = trt, fill = trt, group = trt)) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black",
               linewidth = 1.5, size = 1.5, pch = 21,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(site)) +
  ylab("Total number of visits") +
  xlab("Year") +
  theme_bw() +
  theme(legend.position = c(0.1,0.86))

# Plot 5: tot_visits ~ clutch_size + trt + site

coord_data %>%
  ggplot(aes(x = clutch_size, y = tot_visits, col = trt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(site)) +
  ylab("Total number of visits") +
  xlab("Clutch Size") +
  theme_bw() +
  theme(legend.position = c(0.1,0.86))

# Plot 6: tot_visits ~ julian_data + trt + size

coord_data %>%
  ggplot(aes(x = julian_date, y = tot_visits, col = trt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(site)) +
  ylab("Total number of visits") +
  xlab("Julian Date") +
  theme_bw() +
  theme(legend.position = c(0.1,0.86))

## Section 2: Alternation Score ----

# Plot 7: Alt_score ~ site + trt + stage

coord_data %>%
  #filter(site != "R") %>% # remove if you want to see rural
  ggplot(aes(x = site, y = Alt_score, col = trt, fill = trt, group = trt)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black",
               linewidth = 1.5, size = 1.5, pch = 21,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(stage)) +
  ylab("Alternation score") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.9,0.86))

## Section 3: Abandonment ----

# Plot 8: ab ~ site + trt + stage

# create abandonment column
coord_data$ab <- ifelse(coord_data$m_visit_rate * coord_data$f_visit_rate == 0, 1, 0)

# plot
coord_data %>%
  filter(site != "R") %>% # remove if you want to see rural
  ggplot(aes(x = site, y = ab, col = trt, fill = trt)) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 8,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(stage)) +
  ylab("Nest abandonment probability") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8))

# Plot 9: ab ~ clutch size + trt + site

coord_data %>%
  ggplot(aes(x = clutch_size, y = ab, col = trt, fill = trt)) +
  geom_point() +
  geom_smooth(method = "glm",  method.args=list(family="binomial")) +
  facet_grid(cols = vars(site))

# Plot 10: Female and male abandonment probability

# create female abadonment column
coord_data$bye_mom <- ifelse(coord_data$f_visit_rate == 0, 1, 0)
coord_data$bye_dad <- ifelse(coord_data$m_visit_rate == 0, 1, 0)

# plot
coord_data %>%
  #filter(site != "R") %>% # remove if you want to see rural
  ggplot(aes(x = site, y = bye_dad, col = trt, fill = trt)) +
  geom_point(position = position_jitterdodge(), size = 3, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 8,
               position = position_dodge(width = 0.75)) +
  scale_fill_discrete(name = "Treatment") +
  scale_color_discrete(name = "Treatment") +
  facet_grid(cols = vars(stage)) +
  ylab("Female abandonment probability") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8))

# Plot 11: Female vs male abandonment probability

# processing data
fvm_abandonment <- coord_data %>%
  dplyr::select(site, trt, stage, bye_dad, bye_mom) %>%
  group_by(site, trt, stage) %>%
  summarise(bye_dad = mean(bye_dad), bye_mom = mean(bye_mom)) %>%
  ungroup() %>%
  pivot_longer(cols = c(bye_dad, bye_mom), names_to = "who", values_to = "prob")

# Nest abandonment probability by site, trt and sex
fvm_abandonment %>%
  #filter(site != "R") %>%
  ggplot(aes(x = site, y = prob, col = trt)) +
  geom_point(aes(shape = who), position = position_jitterdodge(), size = 6) +
  facet_grid(cols = vars(stage)) +
  ylab("Abandonment probability") +
  xlab("Site") +
  theme_bw() +
  theme(legend.position = c(0.1,0.8))

# PLot 12: Female vs Male number of visits





