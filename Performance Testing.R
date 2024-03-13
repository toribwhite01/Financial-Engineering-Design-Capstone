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

#Function to compute performance measures
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
  
  perform = data.frame(rowid = 1:9, modeltype = 1:9, AdjRsq = 1:9, MSE = 1:9)
  
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
  
  # CREATE EACH MODEL, TEST, REPEAT
  
  ####
  #All ln(premium) and ln(liability)
  
  ##Overall MLR
  overall <- lm(log(premium) ~ stfips + ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = dftrain)
  perform$AdjRsq[1] = summary(overall)$adj.r.squared
  
  # Test Overall MLR
  perform$MSE[1] = PerformanceTesting(overall, dftest)
  print("model1")
  # Remove Overall MLR
  remove(overall)
  remove(dftrain)
  remove(dftest)
  
  ##IL MLR
  IL <- lm(log(premium) ~ ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = ILtrain)
  perform$AdjRsq[2] = summary(IL)$adj.r.squared
  
  # Test IL MLR
  perform$MSE[2] = PerformanceTesting(IL, ILtest)
  print("model2")
  
  # Remove IL MLR
  remove(IL)
  remove(ILtest)
  remove(ILtrain)
  
  ##IN MLR
  IN <- lm(log(premium) ~ ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = INtrain)
  perform$AdjRsq[3] = summary(IN)$adj.r.squared
  
  # Test IN MLR
  perform$MSE[3] = PerformanceTesting(IN, INtest)
  print("model3")
  
  # Remove IN MLR
  remove(IN)
  remove(INtrain)
  remove(INtest)
  
  ## Corn MLR
  corn <- lm(log(premium) ~ stfips + ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Corntrain)
  perform$AdjRsq[4] = summary(corn)$adj.r.squared

  # Test Corn MLR
  perform$MSE[4] = PerformanceTesting(corn, Corntest)
  print("model4")
  
  # Remove Corn MLR
  remove(corn)
  remove(Corntest)
  remove(Corntrain)
  
  ## Soy MLR w/ ctyfips (some ctyfips are not significant)
  soy <- lm(log(premium) ~ stfips + ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = Soybeantrain)
  perform$AdjRsq[5] = summary(soy)$adj.r.squared

  # Test Soy MLR
  perform$MSE[5] = PerformanceTesting(soy, Soybeantest)
  print("model5")
  
  # Remove Soy MLR
  remove(soy)
  remove(Soybeantrain)
  remove(Soybeantest)
  
  ## IL Corn MLR w/ ctyfips
  IL_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
  perform$AdjRsq[6] = summary(IL_corn)$adj.r.squared

  # Test IL_corn MLR
  perform$MSE[6] = PerformanceTesting(IL_corn, IL_corntest)
  print("model6")
  
  # Remove IL_corn MLR
  remove(IL_corn)
  remove(IL_corntrain)
  remove(IL_corntest)
  
  ## IL soy MLR w/ cty
  IL_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
  perform$AdjRsq[7] = summary(IL_soy)$adj.r.squared

  # Test IL_soy MLR
  perform$MSE[7] = PerformanceTesting(IL_soy, IL_soybeantest)
  print("model7")
  
  # Remove IL_soy MLR
  remove(IL_soy)
  remove(IL_soybeantrain)
  remove(IL_soybeantest)
  
  ## IN Corn MLR w/ ctyfips
  IN_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
  perform$AdjRsq[8] = summary(IN_corn)$adj.r.squared

  # Test  IN_corn MLR
  perform$MSE[8] = PerformanceTesting(IN_corn, IN_corntest)
  print("model8")
  
  # Remove IN_corn MLR
  remove(IN_corn)
  remove(IN_corntrain)
  remove(IN_corntest)
  
  ## IN soy MLR w/ cty
  IN_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)
  perform$AdjRsq[9] = summary(IN_soy)$adj.r.squared

  # Test IN_soy MLR
  perform$MSE[9] = PerformanceTesting(IN_soy, IN_soybeantest)
  print("model9")
  
  # Remove IN_soy MLR
  remove(IN_soy)
  remove(IN_soybeantrain)
  remove(IN_soybeantest)
  
  return(perform)
}

# Make data frame to hold performance measure values for each year
performance = data.frame(rowid = 1:180,
                     year = 1:180, modeltype = 1:180, AdjRsq = 1:180, MSE = 1:180)

# Set the year value for each row
performance$year[1:9] = 1998
performance$year[10:18] = 1999
performance$year[19:27] = 2000
performance$year[28:36] = 2001
performance$year[37:45] = 2002
performance$year[46:54] = 2003
performance$year[55:63] = 2004
performance$year[64:72] = 2005
performance$year[73:81] = 2006
performance$year[82:90] = 2007
performance$year[91:99] = 2008
performance$year[100:108] = 2009
performance$year[109:117] = 2010
performance$year[118:126] = 2011
performance$year[127:135] = 2012
performance$year[136:144] = 2013
performance$year[145:153] = 2014
performance$year[154:162] = 2015
performance$year[163:171] = 2016
performance$year[172:180] = 2017

# Set the model type for each row
performance$modeltype[1:9] = 1:9
performance$modeltype[10:18] = 1:9
performance$modeltype[19:27] = 1:9
performance$modeltype[28:36] = 1:9
performance$modeltype[37:45] = 1:9
performance$modeltype[46:54] = 1:9
performance$modeltype[55:63] = 1:9
performance$modeltype[64:72] = 1:9
performance$modeltype[73:81] = 1:9
performance$modeltype[82:90] = 1:9
performance$modeltype[91:99] = 1:9
performance$modeltype[100:108] = 1:9
performance$modeltype[109:117] = 1:9
performance$modeltype[118:126] = 1:9
performance$modeltype[127:135] = 1:9
performance$modeltype[136:144] = 1:9
performance$modeltype[145:153] = 1:9
performance$modeltype[154:162] = 1:9
performance$modeltype[163:171] = 1:9
performance$modeltype[172:180] = 1:9

#Call performance function for each year and set the appropriate values in the 
# data frame equal to the values returned from the function
per1998 = Performance(1998)
performance[1:9,4:5] = per1998[1:9, 3:4]
per1999 = Performance(1999)
performance[10:18,4:5] = per1999[1:9, 3:4]
per2000 = Performance(2000)
performance[19:27,4:5] = per2000[1:9, 3:4]
per2001 = Performance(2001)
performance[28:36,4:5] = per2001[1:9, 3:4]
per2002 = Performance(2002)
performance[37:45,4:5] = per2002[1:9, 3:4]
per2003 = Performance(2003)
performance[46:54,4:5] = per2003[1:9, 3:4]
per2004 = Performance(2004)
performance[55:63,4:5] = per2004[1:9, 3:4]
per2005 = Performance(2005)
performance[64:72,4:5] = per2005[1:9, 3:4]
per2006 = Performance(2006)
performance[73:81,4:5] = per2006[1:9, 3:4]
per2007 = Performance(2007)
performance[82:90,4:5] = per2007[1:9, 3:4]
per2008 = Performance(2008)
performance[91:99,4:5] = per2008[1:9, 3:4]
per2009 = Performance(2009)
performance[100:108,4:5] = per2009[1:9, 3:4]
per2010 = Performance(2010)
performance[109:117,4:5] = per2010[1:9, 3:4]
per2011 = Performance(2011)
performance[118:126,4:5] = per2011[1:9, 3:4]
per2012 = Performance(2012)
performance[127:135,4:5] = per2012[1:9, 3:4]
per2013 = Performance(2013)
performance[136:144,4:5] = per2013[1:9, 3:4]
per2014 = Performance(2014)
performance[145:153,4:5] = per2014[1:9, 3:4]
per2015 = Performance(2015)
performance[154:162,4:5] = per2015[1:9, 3:4]
per2016 = Performance(2016)
performance[163:171,4:5] = per2016[1:9, 3:4]
per2017 = Performance(2017)
performance[172:180,4:5] = per1998[1:9, 3:4]

# Plot the Out-of-Sample MSE and the Adjusted R-squared
boxplot(performance$MSE ~ performance$modeltype, data = performance, xlab = "Model Type", ylab = "Mean Squared Error", main = "Mean Squared Error vs Model Type", col = "darkseagreen4")
boxplot(performance$AdjRsq ~ performance$modeltype, data = performance, xlab = "Model Type", ylab = "Adjusted R-Squared", main = "Adjusted R-Squared vs Model Type", col = "darkseagreen4")

# Save the data frame
write.csv(performance, "C:/Users/Conor/OneDrive/Documents/EFIN 499R/Performance Testing.csv", row.names=FALSE)
performance <- fread("C:/Users/Conor/OneDrive/Documents/EFIN 499R/Performance Testing.csv")
