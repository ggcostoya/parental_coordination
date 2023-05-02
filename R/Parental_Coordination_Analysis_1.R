## Parental coordination analysis ##

## Packages ----
library(tidyverse)
library(lme4)

## Load & clean data ----

# load dataset
coord_data <- read_csv("raw_data/HW_Provisioning_Data.csv")

# clean dataset
coord_data$NestID <- as.factor(coord_data$NestID)
coord_data$site <- as.factor(coord_data$site)
coord_data$trt <- as.factor(coord_data$trt)
coord_data$stage <- as.factor(coord_data$stage)
coord_data$year <- as.factor(coord_data$year)

## Preliminary data viz ----

# tot_visits ~ site + trt
coord_data %>%
  ggplot(aes(x = site, y = tot_visits, col = trt)) +
  geom_boxplot() +
  geom_jitter(width = 0.25)

# tot_visits ~ trt + site
coord_data %>%
  ggplot(aes(x = trt, y = tot_visits, col = site)) +
  geom_boxplot() +
  geom_jitter(width = 0.25)

# tot_visits ~ stage
coord_data %>%
  ggplot(aes(x = stage, y = tot_visits)) +
  geom_boxplot() +
  geom_jitter(width = 0.25)

# tot_visits ~ site + stage
coord_data %>%
  ggplot(aes(x = site, y = tot_visits, col = stage, group = stage)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun = mean, geom = "point", size = 4, col = "black",
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               position = position_dodge(width = 0.75))

# tot_visits ~ site + trt + stage
coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = site, y = tot_visits, col = trt, group = trt)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun = mean, geom = "point", size = 4, col = "black",
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               position = position_dodge(width = 0.75)) +
  facet_grid(cols = vars(stage))

# tot_visits ~ year + site
coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = year, y = tot_visits, col = site, group = site)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun = mean, geom = "point", size = 4, col = "black",
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               position = position_dodge(width = 0.75))


## Test models ----

# model 1: tot_visits ~ site * trt + Random(NestID)
model1 <- glmer(tot_visits ~ site * trt + (1|NestID), data = coord_data,
                family = poisson(link = log))

summary(model1)
anova(model1)

# model 2: tot_visits ~ site * stage + Random(NestID)
model2 <- glmer(tot_visits ~ site * stage + (1|NestID), data = coord_data,
                family = poisson(link = log))

summary(model2)

# model 3: tot_visits ~ site * trt * stage + Random(NestID)
model3 <- glmer(tot_visits ~ site * trt * stage + (1|NestID), data = coord_data,
                family = poisson(link = log))

summary(model3)

# model 4: tot_visits ~ site * trt * stage + Random(NestID), Excluding R site
model4 <- glmer(tot_visits ~ site * trt * stage + (1|NestID),
                data = coord_data %>% filter(site != "R"),
                family = poisson(link = log))

summary(model4)

# model 5: tot_visits ~ site * trt * stage * year + Random(NestID), Excluding R, year as numeric
model5 <- glmer(tot_visits ~ site * trt * stage * as.numeric(year) + (1|NestID),
                data = coord_data %>% filter(site != "R"),
                family = poisson(link = log))

summary(model5)


test <- glmer(tot_visits ~ site * trt * stage * as.numeric(year) + (1|NestID),
                data = coord_data %>% filter(site != "R"),
                family = poisson(link = log))
summary(test)

# model 6:
model6 <- glmer(tot_visits ~ site + trt + stage + as.numeric(year) + (1|NestID) +
                  site * trt + site * stage + site * as.numeric(year) +
                  trt * stage + trt * as.numeric(year) + stage * as.numeric(year) +
                  site * trt * stage,
                data = coord_data %>% filter(site != "R"),
                family = poisson(link = log))

summary(model6)

AIC(model1, model2, model3, model4, model5)
