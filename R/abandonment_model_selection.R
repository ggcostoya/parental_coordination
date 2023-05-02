## Abandonment probability ##

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

# create abandonment column
coord_data$ab <- ifelse(coord_data$m_visit_rate * coord_data$f_visit_rate == 0, 1, 0)

## Model selection ----

# running full model
full_model <- glmer(ab ~ site * trt * stage * year + clutch_size + julian_date + (1|NestID),
                    #data = coord_data ,
                    data = coord_data  %>% filter(site != "R"), # if R site is excluded
                    family = "binomial")

summary(full_model)

# Model 1: removing 4 & 3 way interactions ** BEST MODEL **
model1 <- glmer(ab ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                 site * trt + site * stage + site * year + trt * stage + trt * year,
                family = "binomial",
               #data = coord_data
               #data = coord_data  %>% filter(site != "R") # if R site is excluded
               data = coord_data  %>% filter(site != "N") # if R site is excluded
)

summary(model1)

# Model 2: removing trt * year interaction
model2 <- glmer(ab ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID) +
                  site * trt + site * stage + site * year + trt * stage,
                family = "binomial",
                #data = coord_data
                data = coord_data  %>% filter(site != "R") # if R site is excluded
)

summary(model2)

# Model 3: removing all interactions
model3 <- glmer(ab ~ site + trt + stage + year + clutch_size + julian_date + (1|NestID),
                family = "binomial",
                #data = coord_data
                data = coord_data  %>% filter(site != "R") # if R site is excluded
)

summary(model3)
AIC(model2, model3)


## One sex abandonment ----

coord_data$bye_mom <- ifelse(coord_data$f_visit_rate == 0, 1, 0)


















