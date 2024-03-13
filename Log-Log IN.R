# set working directory
setwd("D:/")

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
INtrain = dftrain %>% filter(stfips == 18)
INtest = dftest %>% filter(stfips == 18)

# corn crop codes are 41
IN_corntrain  = INtrain %>% filter(crop == 41)
IN_corntest  = INtest %>% filter(crop == 41)

# soy bean crop codes are 41
IN_soybeantrain = INtrain %>% filter(crop == 81)
IN_soybeantest = INtest %>% filter(crop == 81)

####
#All ln(premium) and ln(liability)

##IN MLR w/ ctyfips
IN <- lm(log(premium) ~ ctyfips + crop + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = INtrain)
summary(IN)
#Adjusted R squared = 0.9799 (best IN soy)

## IN Corn MLR w/ ctyfips
IN_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_corntrain)
summary(IN_corn)
#Adjusted R squared = 0.9836 (best IN corn)

## IL soy MLR w/ cty
IN_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)
summary(IN_soy)
#Adjusted R squared = 0.9794 


# IN Corn Test

# In sample
IN_corntrain$predicted_lnpremiums <- predict.lm(IN_corn, IN_corntrain)
IN_corntrain$predicted_premiums <- exp(IN_corntrain$predicted_lnpremiums)
IN_corntrain$predicted_loss_ratios = IN_corntrain$indemnity / IN_corntrain$predicted_premiums
IN_corntrain$group <- cut(IN_corntrain$predicted_loss_ratios, breaks = c(-10000, 0.5, 0.8, 1.1, 1.4,110), labels = c('1', '2', '3', '4', '5'))
IN_corntrain$actual_loss_ratio = IN_corntrain$indemnity / IN_corntrain$premium

boxplot(predicted_loss_ratios ~ group, data = IN_corntrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Indiana Corn Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IN_corntrain, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Corn Model (1998 - 2016)")

groupmean_corntrain = aggregate(IN_corntrain$actual_loss_ratio, list(IN_corntrain$group), FUN=mean) 
lines(groupmean_corntrain$Group.1, groupmean_corntrain$x, type = "b", pch=20, col = "black")
legend("topleft", legend=c("Pre", "Obs"),
       col=c("darkseagreen4", "black"), lty=1, cex=0.8)

#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IN_corntrain, xlab = "Risk Group", ylab = "Loss Ratios", main = "Indiana Corn Model (1998 - 2016)", col = "darkseagreen4", ylim = c(0,6))
boxplot(actual_loss_ratio ~ group, data = IN_corntrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Corn Model (1998 - 2016)")

# Out of Sample

IN_corntest$predicted_lnpremiums <- predict.lm(IN_corn, IN_corntest)
IN_corntest$predicted_premiums <- exp(IN_corntest$predicted_lnpremiums)
IN_corntest$predicted_loss_ratios = IN_corntest$indemnity / IN_corntest$predicted_premiums
IN_corntest$group <- cut(IN_corntest$predicted_loss_ratios, breaks = c(-6000, 0.5, 0.8,1.1,1.4, 20), labels = c('1','2', '3', '4', '5'))
IN_corntest$actual_loss_ratio = IN_corntest$indemnity / IN_corntest$premium

boxplot(predicted_loss_ratios ~ group, data = IN_corntest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Indiana Corn Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IN_corntest, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Corn Model (2017)")

#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IN_corntest, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Indiana Corn Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IN_corntest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Corn Model (2017)")




# IN Soy Test

# In sample
IN_soybeantrain$predicted_lnpremiums <- predict.lm(IN, IN_soybeantrain)
IN_soybeantrain$predicted_premiums <- exp(IN_soybeantrain$predicted_lnpremiums)
IN_soybeantrain$predicted_loss_ratios = IN_soybeantrain$indemnity / IN_soybeantrain$predicted_premiums
IN_soybeantrain$group <- cut(IN_soybeantrain$predicted_loss_ratios, breaks = c(-10000, 0.5, 0.8, 1.1, 1.4,150), labels = c('1', '2', '3', '4', '5'))
IN_soybeantrain$actual_loss_ratio = IN_soybeantrain$indemnity / IN_soybeantrain$premium

boxplot(predicted_loss_ratios ~ group, data = IN_soybeantrain, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Indiana Soy Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IN_soybeantrain, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Soy Model (1998 - 2016)")


#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IN_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Indiana Soy Model (1998 - 2016)")
boxplot(actual_loss_ratio ~ group, data = IN_soybeantrain, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Soy Model (1998 - 2016)")

# Out of Sample

IN_soybeantest$predicted_lnpremiums <- predict.lm(IN, IN_soybeantest)
IN_soybeantest$predicted_premiums <- exp(IN_soybeantest$predicted_lnpremiums)
IN_soybeantest$predicted_loss_ratios = IN_soybeantest$indemnity / IN_soybeantest$predicted_premiums
IN_soybeantest$group <- cut(IN_soybeantest$predicted_loss_ratios, breaks = c(-6000, 0.5, 0.8,1.1,1.4, 73), labels = c('1','2', '3', '4', '5'))
IN_soybeantest$actual_loss_ratio = IN_soybeantest$indemnity / IN_soybeantest$premium

boxplot(predicted_loss_ratios ~ group, data = IN_soybeantest, xlab = "Risk Group", ylab = "Predicted Loss Ratios", main = "Indiana Soy Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IN_soybeantest, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Soy Model (2017)")

#remove outliers
boxplot(predicted_loss_ratios ~ group, data = IN_soybeantest, outline=FALSE, xlab = "Risk Group", ylab = " Predicted Loss Ratios", main = "Indiana Soy Model (2017)")
boxplot(actual_loss_ratio ~ group, data = IN_soybeantest, outline=FALSE, xlab = "Risk Group", ylab = "Actual Loss Ratios", main = "Indiana Soy Model (2017)")


groupmean = aggregate(IN_soybeantest$actual_loss_ratio, list(IN_soybeantest$group), FUN=mean) 

ggplot(IN_soybeantest, aes(x = group, y = predicted_loss_ratios)) + 
  geom_boxplot(data = IN_soybeantest,  aes(group = group)) + 
  geom_line(data = groupmean, colour = "red") +
  geom_point(data = groupmean, colour = "red", shape = 1) +
  theme_bw()

par(mfrow = c(2,2))
plot(IN)
