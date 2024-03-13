# set working directory
setwd("C:/Users/Conor/Documents")

# import packages
library(data.table)
library(tmap)
library(tidyverse)
library(sf)
library(gtools)
library(dplyr)
library(mosaic)

# import data with data.table package
df <- fread("msuCapstone_premiumEstimation.csv")
df <- df %>%
  mutate(stfips = factor(stfips),
         ctyfips = factor(ctyfips),
         crop = factor(crop),
         insurancePlan = factor(insurancePlan),
         coverageLevel = factor(coverageLevel),
         coverageTypeCodeDummy = factor(coverageTypeCodeDummy),
         mapAreaDummy = factor(mapAreaDummy),
         enterpriseUnitDummy = factor(enterpriseUnitDummy))

#Create Train and Test data for each combination
dftrain = df %>% filter(cropYear != 2017)
dftest = df %>% filter(cropYear == 2017)
remove(df)

# create unique dataframes for each state/crop
# state Federal Information Processing Standards (fips) 17 is the code for Illinois while state fips 18 is Indiana
INtrain = dftrain %>% filter(stfips == 18)
INtest = dftest %>% filter(stfips == 18)

# corn crop codes are 41
IN_corntrain  = INtrain %>% filter(crop == 41)
IN_corntest = INtest %>% filter(crop == 41)

# soy bean crop codes are 41
IN_soybeantrain = INtrain %>% filter(crop == 81)
IN_soybeantest = INtest %>% filter(crop == 81)

##IN MLR w/ ctyfips (some ctyfips are not significant)
IN <- lm(premium ~ ctyfips + crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = INtrain)
summary(IN)
#Adjusted R squared = 0.8319 (best IN soy)

## IN MLR w/o ctyfips 
IN_no_cty <- lm(premium ~ crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = INtrain)
summary(IN_no_cty)
#Adjusted R squared = 0.8136

## IN Corn MLR w/ ctyfips (some ctyfips are not significant)
IN_corn <- lm(premium ~ ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
summary(IN_corn)
#Adjusted R squared = 0.8423

## IN Corn MLR w/o avgBias
IN_corn_no_avgbias <- lm(premium ~ ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
summary(IN_corn_no_avgbias)
#Adjusted R squared = 0.8423 (best IN corn)

## IN Corn MLR w/o avgBias or ctyfips
IN_corn_no_avgbias_no_ctyfips <- lm(premium ~ insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
summary(IN_corn_no_avgbias_no_ctyfips)
#Adjusted R squared = 0.8218

## IN soy MLR w/ cty (some ctyfips are not significant)
IN_soy <- lm(premium ~ ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)
summary(IN_soy)
#Adjusted R squared = 0.8278

## IN soy w/o cty
IN_soy_no_cty <- lm(premium ~ insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)
summary(IN_soy_no_cty)
#Adjusted R squared = 0.8035