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
dftrain$lossratio = dftrain$indemnity / dftrain$premium


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

##Overall MLR with ctyfips (some ctyfips are not significant)
overall <- lm(premium ~ stfips + ctyfips + crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = dftrain)
summary(overall)
#Adj R squared = 0.8032

##Overall MLR w/o ctyfips
overall_no_cty <- lm(premium ~ stfips + crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = dftrain)
summary(overall_no_cty)
#Adj R squared = 0.7908

##IL MLR w/ ctyfips (some ctyfips are not significant)
IL <- lm(premium ~ ctyfips + crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = ILtrain)
summary(IL)
#Adjusted R squared = 0.8081 (best for IL soy)

## IL MLR w/o ctyfips 
IL_no_cty <- lm(premium ~ crop + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = ILtrain)
summary(IL_no_cty)
#Adjusted R squared = 0.7864


## Corn MLR w/ ctyfips (some ctyfips are not significant)
corn <- lm(premium ~ stfips + ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Corntrain)
summary(corn)
#Adjusted R squared = 0.8237

## Corn MLR w/o ctyfips
corn_no_cty <- lm(premium ~ stfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Corntrain)
summary(corn_no_cty)
#Adjusted R squared = 0.8078

## Soy MLR w/ ctyfips (some ctyfips are not significant)
soy <- lm(premium ~ stfips + ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Soybeantrain)
summary(soy)
#Adjusted R squared = 0.7372

## Soy MLR w/o ctyfips
soy_no_cty <- lm(premium ~ stfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Soybeantrain)
summary(soy_no_cty)
#Adjusted R squared = 0.7218

## IL Corn MLR w/ ctyfips (some ctyfips are not significant)
IL_corn <- lm(premium ~ ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
summary(IL_corn)
#Adjusted R squared = 0.8347 (best IL Corn)

## IL Corn MLR w/o ctyfips
IL_corn_no_cty <- lm(premium ~ insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
summary(IL_corn_no_cty)
#Adjusted R squared = 0.8082


## IL soy MLR w/ cty (some ctyfips are not significant)
IL_soy <- lm(premium ~ ctyfips + insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
summary(IL_soy)
#Adjusted R squared = 0.7116

## IL soy w/o cty
IL_soy_no_cty <- lm(premium ~ insurancePlan + coverageLevel + liability + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
summary(IL_soy_no_cty)
#Adjusted R squared = 0.6792

#ln premium IL corn
IL_corntrain$lnpremium <- log(IL_corntrain$premium)
IL_corn_ln <- lm(lnpremium ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
summary(IL_corn_ln)
#Adjusted R squared = 0.7944 (best IL Corn)


## IL test

# In Sample testing
ILtrain$predicted_premiums <- predict.lm(IL, ILtrain)
ILtrain$predicted_loss_ratios = ILtrain$indemnity / ILtrain$predicted_premiums
ILtrain$group <- cut(ILtrain$predicted_loss_ratios, breaks = c(-160000,0.5,0.8,1.1,1.4,63000), labels = c('1','2','3','4','5'))

boxplot(predicted_loss_ratios ~ group, data = ILtrain, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Base Model")

sub_ILtrain = subset(ILtrain, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_ILtrain$group <- cut(sub_ILtrain$predicted_loss_ratios, breaks = c(-100, 0.5,0.8,1.1,1.4,200), labels = c('1','2','3','4','5'))
sub_ILtrain$actual_loss_ratio = sub_ILtrain$indemnity / sub_ILtrain$premium
# sub_ILtrain = subset(sub_ILtrain, actual_loss_ratio < 5)

boxplot(actual_loss_ratio ~ group, data = sub_ILtrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Base Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_ILtrain, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Base Model (Subset)")

# Out of sample testing

# State
ILtest$predicted_premiums <- predict.lm(IL, ILtest)
ILtest$predicted_loss_ratios = ILtest$indemnity / ILtest$predicted_premiums 
ILtest$group <- cut(ILtest$predicted_loss_ratios, breaks = c(-1875,0.5,0.8,1.1,1.4,3075), labels = c('1','2','3','4','5'))

boxplot(predicted_loss_ratios ~ group, data = ILtest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Base Model")

sub_ILtest = subset(ILtest, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_ILtest$group <- cut(sub_ILtest$predicted_loss_ratios, breaks = c(-100, 0.5,0.8,1.1,1.4,200), labels = c('1','2','3','4','5'))
sub_ILtest$actual_loss_ratio = sub_ILtest$indemnity / sub_ILtest$premium
# sub_ILtest = subset(sub_ILtest, actual_loss_ratio < 5)

boxplot(actual_loss_ratio ~ group, data = sub_ILtest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Base Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_ILtest, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Base Model (Subset)")

# IL Corn Test

# In sample
IL_corntrain$predicted_premiums <- predict.lm(IL_corn)
IL_corntrain$predicted_loss_ratios = IL_corntrain$indemnity / IL_corntrain$predicted_premiums
IL_corntrain$group <- cut(IL_corntrain$predicted_loss_ratios, breaks = c(-10000, 0.5, 0.8, 1.1, 1.4,43000), labels = c('1', '2', '3', '4', '5'))

boxplot(predicted_loss_ratios ~ group, data = IL_corntrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Corn Model")

sub_IL_corntrain = subset(IL_corntrain, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_IL_corntrain$group <- cut(sub_IL_corntrain$predicted_loss_ratios, breaks = c(-100, 0.5, 0.8, 1.1, 1.4,200), labels = c('1','2','3','4','5'))
sub_IL_corntrain$actual_loss_ratio = sub_IL_corntrain$indemnity / sub_IL_corntrain$premium

boxplot(actual_loss_ratio ~ group, data = sub_IL_corntrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_IL_corntrain, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Corn Model (Subset)")

# Out of Sample

IL_corntest$predicted_premiums <- predict.lm(IL_corn, IL_corntest)
IL_corntest$predicted_loss_ratios = IL_corntest$indemnity / IL_corntest$predicted_premiums
IL_corntest$group <- cut(IL_corntest$predicted_loss_ratios, breaks = c(-6000, 0.5, 0.8,1.1,1.4, 520), labels = c('1','2', '3', '4', '5'))

boxplot(predicted_loss_ratios ~ group, data = IL_corntest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Corn Model")

sub_ILcorntest = subset(IL_corntest, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_ILcorntest$group <- cut(sub_ILcorntest$predicted_loss_ratios, breaks = c(-100, 0.5,0.8,1.1,1.4,200), labels = c('1','2','3','4','5'))
sub_ILcorntest$actual_loss_ratio = sub_ILcorntest$indemnity / sub_ILcorntest$premium
# sub_ILcorntest = subset(sub_ILcorntest, actual_loss_ratio < 5)

boxplot(actual_loss_ratio ~ group, data = sub_ILcorntest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Corn Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_ILcorntest, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Corn Model (Subset)")

# IL Soy Test

# In sample
IL_soybeantrain$predicted_premiums <- predict.lm(IL, IL_soybeantrain)
IL_soybeantrain$predicted_loss_ratios = IL_soybeantrain$indemnity / IL_soybeantrain$predicted_premiums
IL_soybeantrain$group <- cut(IL_soybeantrain$predicted_loss_ratios, breaks = c(-160000, 0.5, 0.8, 1.1, 1.4,26000), labels = c('1', '2', '3', '4', '5'))

boxplot(predicted_loss_ratios ~ group, data = IL_soybeantrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Illinois Soy Model")

sub_IL_soybeantrain = subset(IL_soybeantrain, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_IL_soybeantrain$group <- cut(sub_IL_soybeantrain$predicted_loss_ratios, breaks = c(-100, 0.5, 0.8, 1.1, 1.4,200), labels = c('1','2','3','4','5'))
sub_IL_soybeantrain$actual_loss_ratio = sub_IL_soybeantrain$indemnity / sub_IL_soybeantrain$premium

boxplot(actual_loss_ratio ~ group, data = sub_IL_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soy Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_IL_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Soy Model (Subset)")

#Out of sample
IL_soybeantest$predicted_premiums <- predict.lm(IL, IL_soybeantest)
IL_soybeantest$predicted_loss_ratios = IL_soybeantest$indemnity / IL_soybeantest$predicted_premiums
IL_soybeantest$group <- cut(IL_soybeantest$predicted_loss_ratios, breaks = c(-600, 0.5, 0.8,1.1,1.4, 3075), labels = c('1','2', '3', '4', '5'))

boxplot(predicted_loss_ratios ~ group, data = IL_soybeantest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Soybean Model")

sub_ILsoytest = subset(IL_soybeantest, predicted_loss_ratios > -2 & predicted_loss_ratios < 5)
sub_ILsoytest$group <- cut(sub_ILsoytest$predicted_loss_ratios, breaks = c(-100, 0.5,0.8,1.1,1.4,200), labels = c('1','2','3','4','5'))
sub_ILsoytest$actual_loss_ratio = sub_ILsoytest$indemnity / sub_ILsoytest$premium

boxplot(actual_loss_ratio ~ group, data = sub_ILsoytest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soybean Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = sub_ILsoytest, outline=FALSE, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Soybean Model (Subset)")



IL_corntrain$predicted_lnpremiums <- predict.lm(IL_corn_ln, IL_corntrain)
IL_corntrain$predicted_premiums <- exp(IL_corntrain$predicted_lnpremiums)
IL_corntrain$predicted_loss_ratios = IL_corntrain$indemnity / IL_corntrain$predicted_premiums
IL_corntrain$group <- cut(IL_corntrain$predicted_loss_ratios, breaks = c(-1,0.5,0.8,1.1,1.4,870), labels = c('1','2','3','4','5'))
IL_corntrain$actual_loss_ratio = IL_corntrain$indemnity / IL_corntrain$premium


boxplot(actual_loss_ratio ~ group, data = IL_corntrain,  outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Illinois Soybean Model (Subset)")
boxplot(predicted_loss_ratios ~ group, data = IL_corntrain, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Illinois Base Model", outline=FALSE)
