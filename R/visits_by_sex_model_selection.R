## Visits by sex model selection ##

## Packages ----

library(tidyverse)
library(lme4)
library(lubridate)

## Load & process dataset ----

# load dataset
coord_data <- read_csv("raw_data/HW_Provisioning_Data.csv")

# clean dataset
coord_data$NestID <- as.factor(coord_data$NestID)
coord_data$site <- as.factor(coord_data$site)
coord_data$trt <- as.factor(coord_data$trt)
coord_data$stage <- as.factor(coord_data$stage)
coord_data$year <- as.factor(coord_data$year)

# transfoming date variable
coord_data$date <- mdy(coord_data$date)

# add empty julian date column
coord_data$julian_date <- rep(NA, nrow(coord_data))

# loop to assign Julian dates
for(i in 1:nrow(coord_data)){

  coord_data$julian_date[i] <- julian(coord_data$date[i], origin = as.Date(paste(coord_data$year[i],"-01-01", sep = "")))

}

# select columns of interest for analysis
coord_data <- coord_data %>%
  dplyr::select(NestID, site, trt, stage, year, julian_date, clutch_size,
                m_visit_rate, f_visit_rate)

# rename male and female visit rate columns
coord_data <- coord_data %>% rename(male = m_visit_rate, female = f_visit_rate)

# re-structure visits column
coord_data <- coord_data %>% pivot_longer(c(male, female), names_to = "sex", values_to = "visit_rate")

# remake sex column
coord_data$sex <- as.factor(coord_data$sex)

# Visualize data ----

coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = site, y = visit_rate, col = trt, shape = sex, group = trt:sex)) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", size = 1,
               position = position_dodge(width = 0.75)) +
  ylab("Visits / h") + xlab("Site") +
  facet_grid(cols = vars(stage)) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.85),
        legend.direction = "horizontal")

## Running models ----

# Model 1, full model --> AIC: 1730.5


model_1 <- glmer(visit_rate ~ site * trt * stage * sex * clutch_size + julian_date + (1|NestID) + (1|year),
                data = coord_data %>% filter(site != "R"),
                family = poisson(link = log))

summary(model_1)

# Model 2, removing year as random effect since variance = 0 --> AIC = 1728.5 # Best model
# Significant effects: julian_date, site * trt * stage, site * trt * sex,
# site * stage * sex, site * trt * clutch_size, site * trt * stage * sex,
# all 4 and 5 way interactions except trt * stage * sex * clutch_size

model_2 <- glmer(visit_rate ~ site * trt * stage * sex * clutch_size + julian_date + (1|NestID),
                 data = coord_data %>% filter(site != "R"),
                 family = poisson(link = log))

summary(model_2)

# Model 3, removing all 3 and 4 way interactions --> AIC = 1821.3

model_3 <- glmer(visit_rate ~ site + trt + stage + sex + clutch_size + julian_date + (1|NestID) +
                   site * trt + site * stage + site * sex + site * clutch_size +
                   trt * stage + trt * sex + trt * clutch_size +
                   stage * sex + stage * clutch_size,
                 data = coord_data %>% filter(site != "R"),
                 family = poisson(link = log))


summary(model_3)



