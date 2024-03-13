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

#summary(df)
#head(df)

#Create Train and Test data for each combination
dftrain = df %>% filter(cropYear != 2017)
dftest = df %>% filter(cropYear == 2017)
remove(df)

# create unique dataframes for each state/crop
# state Federal Information Processing Standards (fips) 17 is the code for Illinois while state fips 18 is Indiana
ILtrain = dftrain %>% filter(stfips == 17)
ILtest = dftest %>% filter(stfips == 17)

# corn crop codes are 41
Corntrain  = dftrain %>% filter(crop == 41)
Soybeantrain = dftrain %>% filter(crop == 81)
Corntest = dftest %>% filter(crop == 41)
Soybeantest = dftest %>% filter(crop == 81)

remove(dftest)
remove(dftrain)

# corn crop codes are 41
IL_corntrain  = ILtrain %>% filter(crop == 41)
IL_corntest  = ILtest %>% filter(crop == 41)

# soy bean crop codes are 41
IL_soybeantrain = ILtrain %>% filter(crop == 81)
IL_soybeantest = ILtest %>% filter(crop == 81)

####
#All ln(premium) and ln(liability)

##Overall MLR with ctyfips
overall <- lm(log(premium) ~ stfips + ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = dftrain)
summary(overall)
#Adj R squared = 0.949

##Overall MLR w/o ctyfips
overall_no_cty <- lm(log(premium) ~ stfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = dftrain)
summary(overall_no_cty)
#Adj R squared = 0.916

##IL MLR w/ ctyfips
IL <- lm(log(premium) ~ ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = ILtrain)
summary(IL)
#Adjusted R squared = 0.9641 (best IL soy)

## Corn MLR w/ ctyfips
corn <- lm(log(premium) ~ stfips + ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Corntrain)
summary(corn)
#Adjusted R squared = 0.9559

## Soy MLR w/ ctyfips (some ctyfips are not significant)
soy <- lm(log(premium) ~ stfips + ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Soybeantrain)
summary(soy)
#Adjusted R squared = 0.9453

## Soy MLR w/o ctyfips
soy_no_cty <- lm(log(premium) ~ stfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Soybeantrain)
summary(soy_no_cty)
#Adjusted R squared = 0.9079

## IL Corn MLR w/ ctyfips
IL_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
summary(IL_corn)
#Adjusted R squared = 0.972 (best IL corn)

## IL soy MLR w/ cty
IL_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
summary(IL_soy)
#Adjusted R squared = 0.9637


# IL Corn Test

# In sample

# Predict log premiums using best model
IL_corntrain$predicted_lnpremiums <- predict.lm(IL_corn, IL_corntrain)

# Convert log premiums to premiums
IL_corntrain$predicted_premiums <- exp(IL_corntrain$predicted_lnpremiums)

# Calculate predicted loss ratios
IL_corntrain$predicted_loss_ratios = IL_corntrain$indemnity / IL_corntrain$predicted_premiums

# Put observations into groups based on predicted loss ratios
IL_corntrain$group <- cut(IL_corntrain$predicted_loss_ratios, breaks = c(-10000, 0.5, 0.8, 1.1, 1.4,400), labels = c('1', '2', '3', '4', '5'))

# Calculate actua loss ratio
IL_corntrain$actual_loss_ratio = IL_corntrain$indemnity / IL_corntrain$premium

# Plot Predicted loss ratios and actual loss ratios by group
boxplot(predicted_loss_ratios ~ group, data = IL_corntrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Corn Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IL_corntrain, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (1998 - 2016)")


#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IL_corntrain, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Corn Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IL_corntrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (1998 - 2016)")

# Out of Sample

# Repeat in-sample process using test data set
IL_corntest$predicted_lnpremiums <- predict.lm(IL_corn, IL_corntest)
IL_corntest$predicted_premiums <- exp(IL_corntest$predicted_lnpremiums)
IL_corntest$predicted_loss_ratios = IL_corntest$indemnity / IL_corntest$predicted_premiums
IL_corntest$group <- cut(IL_corntest$predicted_loss_ratios, breaks = c(-6000, 0.5, 0.8,1.1,1.4, 222), labels = c('1','2', '3', '4', '5'))
IL_corntest$actual_loss_ratio = IL_corntest$indemnity / IL_corntest$premium

boxplot(predicted_loss_ratios ~ group, data = IL_corntest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Corn Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IL_corntest, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (2017)")

#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IL_corntest, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Corn Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IL_corntest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (2017)")




# IL Soy Test

# In sample
IL_soybeantrain$predicted_lnpremiums <- predict.lm(IL_soy, IL_soybeantrain)
IL_soybeantrain$predicted_premiums <- exp(IL_soybeantrain$predicted_lnpremiums)
IL_soybeantrain$predicted_loss_ratios = IL_soybeantrain$indemnity / IL_soybeantrain$predicted_premiums
IL_soybeantrain$group <- cut(IL_soybeantrain$predicted_loss_ratios, breaks = c(-10000, 0.5, 0.8, 1.1, 1.4,500), labels = c('1', '2', '3', '4', '5'))
IL_soybeantrain$actual_loss_ratio = IL_soybeantrain$indemnity / IL_soybeantrain$premium

boxplot(predicted_loss_ratios ~ group, data = IL_soybeantrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Soy Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IL_soybeantrain, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soy Model (1998 - 2016)")


#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IL_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Soy Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IL_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soy Model (1998 - 2016)")

# Out of Sample

IL_soybeantest$predicted_lnpremiums <- predict.lm(IL, IL_soybeantest)
IL_soybeantest$predicted_premiums <- exp(IL_soybeantest$predicted_lnpremiums)
IL_soybeantest$predicted_loss_ratios = IL_soybeantest$indemnity / IL_soybeantest$predicted_premiums
IL_soybeantest$group <- cut(IL_soybeantest$predicted_loss_ratios, breaks = c(-6000, 0.5, 0.8,1.1,1.4, 226), labels = c('1','2', '3', '4', '5'))
IL_soybeantest$actual_loss_ratio = IL_soybeantest$indemnity / IL_soybeantest$premium

boxplot(predicted_loss_ratios ~ group, data = IL_soybeantest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Soy Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IL_soybeantest, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soy Model (2017)")

#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IL_soybeantest, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Soy Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IL_soybeantest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soy Model (2017)")
