## Total Number of Visits Model Selection ##

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

# running full model <- AIC = 759.6 / site AIC = 678.1 without R
full_model <- glmer(tot_visits ~ site * trt * stage * year + clutch_size + julian_date + (1|NestID),
                    #data = coord_data ,
                    data = coord_data  %>% filter(site != "R"), # if R site is excluded
                    family = poisson(link = log))

summary(full_model)

# model 1: removing 4 time interaction site * trt * stage * year <- AIC = 758.9 / AIC = 677.4 without R
model1 <- glmer(tot_visits ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                 site * trt + site * stage + site * year + trt * stage + trt * year +
                 site * trt * stage + site * trt * year + site * stage * year + trt * stage * year,
                #data = coord_data ,
               data = coord_data  %>% filter(site != "R"), # if R site is excluded
               family = poisson(link = log))

summary(model1)

# model 2: remove 3 time interaction site * stage * year --> AIC = 756.9 / AIC = 676.0 without R
model2 <- glmer(tot_visits ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                  site * trt + site * stage + site * year + trt * stage + trt * year +
                  site * trt * stage + site * trt * year + trt * stage * year,
                #data = coord_data ,
                data = coord_data  %>% filter(site != "R"), # if R site is excluded
                family = poisson(link = log))

summary(model2)

# model 3: remove 2 way trt * year interaction -> AIC = 756.9 ** BEST MODEL **
model3 <- glmer(tot_visits ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                  site * trt + site * stage + site * year + trt * stage +
                  site * trt * stage + site * trt * year + trt * stage * year,
                data = coord_data ,
                #data = coord_data  %>% filter(site != "R"), # if R site is excluded
                #data = coord_data  %>% filter(site != "N"),
                family = poisson(link = log))

summary(model3)












