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

# Calculate Loss ratios and make histograms of important variables 
df$actual.loss.ratios = df$indemnity / df$premium
hist(df$actual.loss.ratios)
hist(df$premium, xlab = "Premium", main = "Histogram of Premium" )
df$lnpremium = log(df$premium)
hist(df$lnpremium, xlab = "Log Premium", main = "Histogram of Log Premium")
hist(df$liability, xlab = "Liability", main = "Histogram of Liability")
hist(log(df$liability), xlab = "Log Liability", main = "Histogram of Log Liability")
