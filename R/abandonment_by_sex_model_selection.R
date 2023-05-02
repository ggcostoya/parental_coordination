## Abandonment by sex model selection ##

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

# generate abandonment columns
coord_data$female <- ifelse(coord_data$f_visit_rate == 0, 1, 0)
coord_data$male <- ifelse(coord_data$m_visit_rate == 0, 1, 0)

# select columns of interest for analysis & pivot longer
coord_data <- coord_data %>%
  dplyr::select(NestID, site, trt, stage, year, julian_date, clutch_size, female, male) %>%
  pivot_longer(c(female, male), names_to = "sex", values_to = "ab" )

# reformat sex column
coord_data$sex <- as.factor(coord_data$sex)

## Visualize data ---

coord_data %>%
  filter(site != "R") %>%
  ggplot(aes(x = site, y = ab, col = trt, shape = sex, group = trt:sex)) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 4,
               position = position_dodge(width = 0.75)) +
  ylab("Abandonment probability") + xlab("Site") +
  facet_grid(cols = vars(stage))

## Model building considering both early and late stage ----

# Model 1, full model --> AIC: 211.5
# No significant effects

model_1 <- glmer(ab ~ site * trt * stage * sex * clutch_size + julian_date + (1|NestID) + (1|year),
                 data = coord_data %>% filter(site != "R"),
                 family = "binomial")

summary(model_1)

# Model 2, remove year as random effect <- AIC: 209.5 **Best Model**
# No significant effects, julian date almost has an effect

model_2 <- glmer(ab ~ site * trt * stage * sex * clutch_size + julian_date + (1|NestID),
                 data = coord_data %>% filter(site != "R"),
                 family = "binomial")

summary(model_2)


# Model 3, remove 4 way interactions --> AIC: 211.5
# Julian date significant

model_3 <- glmer(ab ~ site + trt + stage + sex + clutch_size + julian_date + (1|NestID) +
                   site * trt + site * stage + site * sex + site * clutch_size +
                   trt * stage + trt * sex + trt * clutch_size +
                   stage * sex + stage * clutch_size + sex * clutch_size +
                   site * trt * stage + site * trt * sex + site * trt * clutch_size +
                   trt * stage * sex + trt * stage * clutch_size +
                   stage * sex * clutch_size,
                 data = coord_data %>% filter(site != "R"),
                 family = "binomial")

summary(model_3)

## Model building considering only late stage ----

# Model 1, full model --> AIC: 140.6
# Nothing significant

model_1 <- glmer(ab ~ site * trt * sex * clutch_size + julian_date + (1|NestID) + (1|year),
                 data = coord_data %>% filter(site != "R") %>% filter(stage == "L"),
                 family = "binomial")

summary(model_1)

# Model 2, removing year as random effect --> AIC: 138.5
# Nothing significant

model_2 <- glmer(ab ~ site * trt * sex * clutch_size + julian_date + (1|NestID),
                 data = coord_data %>% filter(site != "R") %>% filter(stage == "L"),
                 family = "binomial")

summary(model_2)

# Model 3, removing 4 way interactions --> AIC: 137.4 ## Best Model

model_3 <- glmer(ab ~ site + trt + sex + clutch_size + julian_date + (1|NestID) +
                   site * trt + site * sex + site * clutch_size +
                   trt * sex + trt * clutch_size + sex * clutch_size +
                   site * trt * sex + site * sex * clutch_size +
                   trt * sex * clutch_size,
                 data = coord_data %>% filter(site != "R") %>% filter(stage == "L"),
                 family = "binomial")

summary(model_3)

# Model 4, removing 3 way site * sex * clutch and site * trt * sex interactions --> AIC: 145.6

model_4 <- glmer(ab ~ site + trt + sex + clutch_size + julian_date + (1|NestID) +
                   site * trt + site * sex + site * clutch_size +
                   trt * sex + trt * clutch_size + sex * clutch_size +
                   trt * sex * clutch_size,
                 data = coord_data %>% filter(site != "R") %>% filter(stage == "L"),
                 family = "binomial")

summary(model_4)













