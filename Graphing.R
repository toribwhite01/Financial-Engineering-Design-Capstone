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


# Graphing test
graph0.5 <- function(model, data, title){
  data$lnpremiums = predict.lm(model, data)
  data$predicted.premiums =  exp(data$lnpremiums)
  data$predicted.loss.ratios = data$indemnity / data$predicted.premiums
  data$group <- cut(data$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
  data$actual.loss.ratio = data$indemnity / data$premium
  groupmean = aggregate(data$actual.loss.ratio, list(data$group), FUN=mean) 
  boxplot(data$predicted.loss.ratios ~ data$group, data = data, xlab = "Risk Group", ylab = "Loss Ratios", main = title, col = "darkseagreen4", ylim = c(0,4.5))
  lines(groupmean$Group.1, groupmean$x, type = "b", pch=20, col = "black")
  legend("topleft", legend=c("Predicted", "Observed"), col=c("darkseagreen4", "black"), lty=1, cex=0.8)
}

# Final Graphing function
graph <- function(model, data, title){
  data$lnpremiums = predict.lm(model, data)
  data$predicted.premiums =  exp(data$lnpremiums)
  data$predicted.loss.ratios = data$indemnity / data$predicted.premiums
  data$group <- cut(data$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
  data$actual.loss.ratio = data$indemnity / data$premium
  groupmean = aggregate(data$actual.loss.ratio, list(data$group), FUN=mean) 
  p <- ggplot() + 
    geom_boxplot(data = data, aes(x=group, y=predicted.loss.ratios, fill = "darkseagreen4"), color = "black") +
    geom_line(data = groupmean, aes(x=Group.1, y=x, colour = "Observed"), group=1) +
    ylim(0,5) +
    ylab("Loss Ratio")+
    xlab("Risk Group") + 
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_identity(name = NULL, guide = "legend", labels = c("Predicted")) +
    scale_color_manual(name = NULL, breaks = c("Observed"), values = c("Observed"="black"))
  print(p)
}

# Function used to graph residuals
graph_residuals <- function(model, data, title){
  #produce residual vs. fitted plot
  plot(fitted(model))
  res <- resid(model)
  qqnorm(res)
  qqline(res)
}

# Helper function to help make eight graphs for a year
graphHelper <- function(year){
  
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

  # CREATE EACH MODEL, GRAPH, REPEAT
  ## IL Corn MLR w/ ctyfips
  IL_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_corntrain)
  
  # Graph IL_corn MLR in-sample and out-of-sample
  graph(IL_corn, IL_corntrain, paste("IL Corn In-sample", year))
  graph(IL_corn, IL_corntest, paste ("IL Corn Out-of-Sample", year))

  
  # Remove IL_corn MLR
  remove(IL_corn)
  remove(IL_corntrain)
  remove(IL_corntest)
  
  ## IL soy MLR w/ cty
  IL_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IL_soybeantrain)
  
  # Graph IL_soy MLR in-sample and out-of-sample
  graph(IL_soy, IL_soybeantrain, paste("IL Soy In-sample", year))
  graph(IL_soy, IL_soybeantest, paste("IL Soy Out-of-Sample", year))

  # Remove IL_soy MLR
  remove(IL_soy)
  remove(IL_soybeantrain)
  remove(IL_soybeantest)
  
  ## IN Corn MLR w/ ctyfips
  IN_corn <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_corntrain)

  # Graph IN_corn MLR in-sample and out-of-sample
  graph(IN_corn, IN_corntrain, paste("IN Corn In-sample", year))
  graph(IN_corn, IN_corntest, paste("IN Corn Out-of-Sample", year))

  # Remove IN_corn MLR
  remove(IN_corn)
  remove(IN_corntrain)
  remove(IN_corntest)
  
  ## IN soy MLR w/ cty
  IN_soy <- lm(log(premium) ~ ctyfips + insurancePlan + coverageLevel + log(liability) + coverageTypeCodeDummy + mapAreaDummy + numberOfOptionalUnits + avgActualYields + avgTflagCount + avgBias + yieldBump + enterpriseUnitDummy, data = IN_soybeantrain)

  # Graph IN_soy MLR in-sample and out-of-sample
  graph(IN_soy, IN_soybeantrain, paste("IN Soy In-sample", year))
  graph(IN_soy, IN_soybeantest, paste("IN Soy Out-of-Sample", year))

  # Remove IN_soy MLR
  remove(IN_soy)
  remove(IN_soybeantrain)
  remove(IN_soybeantest)
}

# Function calls to get the graphs for each year where the year is the out-of-sample year
graphHelper(1998)
graphHelper(1999)
graphHelper(2000)
graphHelper(2001)
graphHelper(2002)
graphHelper(2003)
graphHelper(2004)
graphHelper(2005)
graphHelper(2006)
graphHelper(2007)
graphHelper(2008)
graphHelper(2009)
graphHelper(2010)
graphHelper(2011)
graphHelper(2012)
graphHelper(2013)
graphHelper(2014)
graphHelper(2015)
graphHelper(2016)
graphHelper(2017)

par(mfrow=c(1,1))

# Making High Resolution graphs for the design fair poster

IN_corntest$lnpremiums = predict.lm(IN_corn, IN_corntest)
IN_corntest$predicted.premiums =  exp(IN_corntest$lnpremiums)
IN_corntest$predicted.loss.ratios = IN_corntest$indemnity / IN_corntest$predicted.premiums
IN_corntest$group <- cut(IN_corntest$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
IN_corntest$actual.loss.ratio = IN_corntest$indemnity / IN_corntest$premium
groupmean = aggregate(IN_corntest$actual.loss.ratio, list(IN_corntest$group), FUN=mean) 
p1 <- ggplot() + 
  geom_boxplot(data = IN_corntest, aes(x=group, y=predicted.loss.ratios, fill = "darkseagreen4"), color = "black") +
  geom_line(data = groupmean, aes(x=Group.1, y=x, colour = "Observed"), group=1) +
  ylim(0,5) +
  ylab("Loss Ratio")+
  xlab("Risk Group") + 
  ggtitle("IN Corn Out-of-Sample 2017")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_identity(name = NULL, guide = "legend", labels = c("Predicted")) +
  scale_color_manual(name = NULL, breaks = c("Observed"), values = c("Observed"="black"))
print(p1)
ggsave(filename = "2017 IN Corn.png",path = "C:/Users/Conor/OneDrive/Documents/EFIN 499R", width = 6, height = 4, units = "in", dpi=700)

IN_soybeantest$lnpremiums = predict.lm(IN_soy, IN_soybeantest)
IN_soybeantest$predicted.premiums =  exp(IN_soybeantest$lnpremiums)
IN_soybeantest$predicted.loss.ratios = IN_soybeantest$indemnity / IN_soybeantest$predicted.premiums
IN_soybeantest$group <- cut(IN_soybeantest$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
IN_soybeantest$actual.loss.ratio = IN_soybeantest$indemnity / IN_soybeantest$premium
groupmean = aggregate(IN_soybeantest$actual.loss.ratio, list(IN_soybeantest$group), FUN=mean) 
p2 <- ggplot() + 
  geom_boxplot(data = IN_soybeantest, aes(x=group, y=predicted.loss.ratios, fill = "darkseagreen4"), color = "black") +
  geom_line(data = groupmean, aes(x=Group.1, y=x, colour = "Observed"), group=1) +
  ylim(0,5) +
  ylab("Loss Ratio")+
  xlab("Risk Group") + 
  ggtitle("IN Soy Out-of-Sample 2017")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_identity(name = NULL, guide = "legend", labels = c("Predicted")) +
  scale_color_manual(name = NULL, breaks = c("Observed"), values = c("Observed"="black"))
print(p2)
ggsave(filename = "2017 IN Soy.png",path = "C:/Users/Conor/OneDrive/Documents/EFIN 499R", width = 6, height = 4, units = "in", dpi=700)


IL_corntest$lnpremiums = predict.lm(IL_corn, IL_corntest)
IL_corntest$predicted.premiums =  exp(IL_corntest$lnpremiums)
IL_corntest$predicted.loss.ratios = IL_corntest$indemnity / IL_corntest$predicted.premiums
IL_corntest$group <- cut(IL_corntest$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
IL_corntest$actual.loss.ratio = IL_corntest$indemnity / IL_corntest$premium
groupmean = aggregate(IL_corntest$actual.loss.ratio, list(IL_corntest$group), FUN=mean) 
p3 <- ggplot() + 
  geom_boxplot(data = IL_corntest, aes(x=group, y=predicted.loss.ratios, fill = "darkseagreen4"), color = "black") +
  geom_line(data = groupmean, aes(x=Group.1, y=x, colour = "Observed"), group=1) +
  ylim(0,5) +
  ylab("Loss Ratio")+
  xlab("Risk Group") + 
  ggtitle("IL Corn Out-of-Sample 2017")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_identity(name = NULL, guide = "legend", labels = c("Predicted")) +
  scale_color_manual(name = NULL, breaks = c("Observed"), values = c("Observed"="black"))
print(p3)
ggsave(filename = "2017 IL Corn.png",path = "C:/Users/Conor/OneDrive/Documents/EFIN 499R", width = 6, height = 4, units = "in", dpi=700)

IL_soybeantest$lnpremiums = predict.lm(IL_soy, IL_soybeantest)
IL_soybeantest$predicted.premiums =  exp(IL_soybeantest$lnpremiums)
IL_soybeantest$predicted.loss.ratios = IL_soybeantest$indemnity / IL_soybeantest$predicted.premiums
IL_soybeantest$group <- cut(IL_soybeantest$predicted.loss.ratios, breaks = c(-1, 0.5, 0.8,1.1,1.4, 1000), labels = c('1','2', '3', '4', '5'))
IL_soybeantest$actual.loss.ratio = IL_soybeantest$indemnity / IL_soybeantest$premium
groupmean = aggregate(IL_soybeantest$actual.loss.ratio, list(IL_soybeantest$group), FUN=mean) 
p4 <- ggplot() + 
  geom_boxplot(data = IL_soybeantest, aes(x=group, y=predicted.loss.ratios, fill = "darkseagreen4"), color = "black") +
  geom_line(data = groupmean, aes(x=Group.1, y=x, colour = "Observed"), group=1) +
  ylim(0,5) +
  ylab("Loss Ratio")+
  xlab("Risk Group") + 
  ggtitle("IL Soy Out-of-Sample 2017")+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_identity(name = NULL, guide = "legend", labels = c("Predicted")) +
  scale_color_manual(name = NULL, breaks = c("Observed"), values = c("Observed"="black"))
print(p4)
ggsave(filename = "2017 IL Soy.png",path = "C:/Users/Conor/OneDrive/Documents/EFIN 499R", width = 6, height = 4, units = "in", dpi=700)
