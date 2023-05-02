## Alternation score Model selection ##

## Packages ----

library(tidyverse)
library(lme4)
library(lmerTest)
library(MASS)
library(buildmer)
library(optimx)
library(lubridate)

## Load & clean data ----

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

## Model selection ----

# write largest model --> AIC 140.2 / AIC 127.5 without R
full_model <- lmer(Alt_score ~ site * trt * stage * year + clutch_size + julian_date + (1|NestID),
                    data = coord_data
                    #data = coord_data  %>% filter(site != "R"), # if R site is excluded
)

summary(full_model)
AIC(full_model)

# Model 1: remove all 3 and 4-way interactions --> AIC 138.5 / 126.02 without R
model1 <- lmer(Alt_score ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                  site * trt + site * stage + site * year + trt * stage + trt * year,
                #data = coord_data
                data = coord_data  %>% filter(site != "R") # if R site is excluded
               )

summary(model1)
AIC(model1)

# Model 2: remove trt * stage --> AIC 136.34 / 123.8516 without R
model2 <- lmer(Alt_score ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                 site * trt + site * stage + site * year + trt * year,
               #data = coord_data
               data = coord_data  %>% filter(site != "R") # if R site is excluded
)

summary(model2)
AIC(model2)

# Model 3: remove site * stage --> AIC 134.2279 / 121.6 without R
model3 <- lmer(Alt_score ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                 site * trt + site * year + trt * year,
               #data = coord_data
               data = coord_data  %>% filter(site != "R") # if R site is excluded
)

summary(model3)
AIC(model3)

# Model 4: remove all interactions --> AIC 125.104 / 115.0032 ** BEST MODEL **
model4 <- lmer(Alt_score ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID),
               data = coord_data
               #data = coord_data  %>% filter(site != "R") # if R site is excluded
               #data = coord_data  %>% filter(site != "N") # if N site is excluded
)

summary(model4)
AIC(model4)

