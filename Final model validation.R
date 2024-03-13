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

# function to test performance
PerformanceTesting <- function(model, test.data){
  # Out of Sample
  library('MLmetrics')
  lnpremiums = predict.lm(model, test.data)
  predicted.premiums =  exp(lnpremiums)
  predicted.loss.ratios = test.data$indemnity / predicted.premiums
  actual.loss.ratio = test.data$indemnity / test.data$premium
  print("model test")
  return(MSE(predicted.loss.ratios, actual.loss.ratio))
  
}

#Helper function to test performance 
Performance  <- function(year){
  
  perform = data.frame(rowid = 1:4, model = 1:4, AdjRsq = 1:4, Out_MSE = 1:4, In_MSE = 1:4)
  
  # SUBSET DATA GIVEN SPECIFIC YEAR
  
  dftrain = df %>% filter(cropYear != year)
  dftest = df %>% filter(cropYear == year)
  
  # create unique dataframes for each state/crop
  # state Federal Information Processing Standards (fips) 17 is the code for Illinois while state fips 18 is Indiana
  INtrain = dftrain %>% filter(stfips == 18)
  INtest = dftest %>% filter(stfips == 18)
  ILtrain = dftrain %>% filter(stfips == 17)
  ILtest = dftest %>% filter(stfips == 17)
  
  # corn crop codes are 41
  Corntrain  = dftrain %>% filter(crop == 41)
  Corntest = dftest %>% filter(crop == 41)
  
  # soy bean crop codes are 41
  Soybeantrain = dftrain %>% filter(crop == 81)
  Soybeantest = dftest %>% filter(crop == 81)
  
  IL_corntrain  = ILtrain %>% filter(crop == 41)
  IL_corntest  = ILtest %>% filter(crop == 41)
  
  IL_soybeantrain = ILtrain %>% filter(crop == 81)
  IL_soybeantest = ILtest %>% filter(crop == 81)
  
  IN_corntrain  = INtrain %>% filter(crop == 41)
  IN_corntest  = INtest %>% filter(crop == 41)
  
  IN_soybeantrain = INtrain %>% filter(crop == 81)
  IN_soybeantest = INtest %>% filter(crop == 81)
  
  # ----------------------------------------------------
  
  # CREATE MODEL, TEST, REPEAT

  ## IL Corn MLR w/ ctyfips
  IL_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
  perform$AdjRsq[1] = summary(IL_corn)$adj.r.squared
  
  # Test IL_corn MLR
  perform$Out_MSE[1] = PerformanceTesting(IL_corn, IL_corntest)
  perform$In_MSE[1] = PerformanceTesting(IL_corn, IL_corntrain)
  print("model1")
  
  # Remove IL_corn MLR
  remove(IL_corn)
  remove(IL_corntrain)
  remove(IL_corntest)
  
  ## IL soy MLR w/ cty
  IL_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
  perform$AdjRsq[2] = summary(IL_soy)$adj.r.squared
  
  # Test IL_soy MLR
  perform$Out_MSE[2] = PerformanceTesting(IL_soy, IL_soybeantest)
  perform$In_MSE[2] = PerformanceTesting(IL_soy, IL_soybeantrain)
  print("model2")
  
  # Remove IL_soy MLR
  remove(IL_soy)
  remove(IL_soybeantrain)
  remove(IL_soybeantest)
  
  ## IN Corn MLR w/ ctyfips
  IN_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
  perform$AdjRsq[3] = summary(IN_corn)$adj.r.squared
  
  # Test  IN_corn MLR
  perform$Out_MSE[3] = PerformanceTesting(IN_corn, IN_corntest)
  perform$In_MSE[3] = PerformanceTesting(IN_corn, IN_corntrain)
  print("model3")
  
  # Remove IN_corn MLR
  remove(IN_corn)
  remove(IN_corntrain)
  remove(IN_corntest)
  
  ## IN soy MLR w/ cty
  IN_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)
  perform$AdjRsq[4] = summary(IN_soy)$adj.r.squared
  
  # Test IN_soy MLR
  perform$Out_MSE[4] = PerformanceTesting(IN_soy, IN_soybeantest)
  perform$In_MSE[4] = PerformanceTesting(IN_soy, IN_soybeantrain)
  print("model4")
  
  # Remove IN_soy MLR
  remove(IN_soy)
  remove(IN_soybeantrain)
  remove(IN_soybeantest)
  
  return(perform)
}

# Make data frame to hold performance measure values for each year
performance = data.frame(rowid = 1:80,
                         year = 1:80, modeltype = 1:80, AdjRsq = 1:80, Out_MSE = 1:80, In_MSE = 1:80)

# Set the year value for each row
performance$year[1:4] = 1998
performance$year[5:8] = 1999
performance$year[9:12] = 2000
performance$year[13:16] = 2001
performance$year[17:20] = 2002
performance$year[21:24] = 2003
performance$year[25:28] = 2004
performance$year[29:32] = 2005
performance$year[33:36] = 2006
performance$year[37:40] = 2007
performance$year[41:44] = 2008
performance$year[45:48] = 2009
performance$year[49:52] = 2010
performance$year[53:56] = 2011
performance$year[57:60] = 2012
performance$year[61:64] = 2013
performance$year[65:68] = 2014
performance$year[69:72] = 2015
performance$year[73:76] = 2016
performance$year[77:80] = 2017

# Set the model type for each row
performance$modeltype[1:4] = 1:4
performance$modeltype[5:8] = 1:4
performance$modeltype[9:12] = 1:4
performance$modeltype[13:16] = 1:4
performance$modeltype[17:20] = 1:4
performance$modeltype[21:24] = 1:4
performance$modeltype[25:28] = 1:4
performance$modeltype[29:32] = 1:4
performance$modeltype[33:36] = 1:4
performance$modeltype[37:40] = 1:4
performance$modeltype[41:44] = 1:4
performance$modeltype[45:48] = 1:4
performance$modeltype[49:52] = 1:4
performance$modeltype[53:56] = 1:4
performance$modeltype[57:60] = 1:4
performance$modeltype[61:64] = 1:4
performance$modeltype[65:68] = 1:4
performance$modeltype[69:72] = 1:4
performance$modeltype[73:76] = 1:4
performance$modeltype[77:80] = 1:4

#Call performance function for each year and set the appropriate values in the 
# data frame equal to the values returned from the function
per1998 = Performance(1998)
performance[1:4,4:6] = per1998[1:4, 3:5]
per1999 = Performance(1999)
performance[5:8,4:6] = per1999[1:4, 3:5]
per2000 = Performance(2000)
performance[9:12,4:6] = per2000[1:4, 3:5]
per2001 = Performance(2001)
performance[13:16,4:6] = per2001[1:4, 3:5]
per2002 = Performance(2002)
performance[17:20,4:6] = per2002[1:4, 3:5]
per2003 = Performance(2003)
performance[21:24,4:6] = per2003[1:4, 3:5]
per2004 = Performance(2004)
performance[25:28,4:6] = per2004[1:4, 3:5]
per2005 = Performance(2005)
performance[29:32,4:6] = per2005[1:4, 3:5]
per2006 = Performance(2006)
performance[33:36,4:6] = per2006[1:4, 3:5]
per2007 = Performance(2007)
performance[37:40,4:6] = per2007[1:4, 3:5]
per2008 = Performance(2008)
performance[41:44,4:6] = per2008[1:4, 3:5]
per2009 = Performance(2009)
performance[45:48,4:6] = per2009[1:4, 3:5]
per2010 = Performance(2010)
performance[49:52,4:6] = per2010[1:4, 3:5]
per2011 = Performance(2011)
performance[53:56,4:6] = per2011[1:4, 3:5]
per2012 = Performance(2012)
performance[57:60,4:6] = per2012[1:4, 3:5]
per2013 = Performance(2013)
performance[61:64,4:6] = per2013[1:4, 3:5]
per2014 = Performance(2014)
performance[65:68,4:6] = per2014[1:4, 3:5]
per2015 = Performance(2015)
performance[69:72,4:6] = per2015[1:4, 3:5]
per2016 = Performance(2016)
performance[73:76,4:6] = per2016[1:4, 3:5]
per2017 = Performance(2017)
performance[77:80,4:6] = per1998[1:4, 3:5]

# Plot the Out-of-Sample MSE, the In-sample MSE, and the Adjusted R-squared
boxplot(performance$Out_MSE ~ performance$modeltype, data = performance, xlab = "Model", ylab = "Out-of-Sample Mean Squared Error", main = "Out-of-Sample Mean Squared Error vs Model", col = "darkseagreen4")
boxplot(performance$In_MSE ~ performance$modeltype, data = performance, xlab = "Model", ylab = "In-Sample Mean Squared Error", main = "In-Sample Mean Squared Error vs Model", col = "darkseagreen4")
boxplot(performance$AdjRsq ~ performance$modeltype, data = performance, xlab = "Model Type", ylab = "Adjusted R-Squared", main = "Adjusted R-Squared vs Model Type", col = "darkseagreen4")

# Save the data frame
write.csv(performance, "C:/Users/Conor/OneDrive/Documents/EFIN 499R/Final Model Validation.csv", row.names=FALSE)
performance <- fread("C:/Users/Conor/OneDrive/Documents/EFIN 499R/Final Model Validation.csv")
