##CB 1 Using Software

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

Marketing <- read_csv("HW/marketing.csv")
Marketing$shelfht <- as.factor(Marketing$shelfht)
Marketing$day <- as.factor(Marketing$day)
View(Marketing)

plotmeans(Marketing$Sales ~ Marketing$day, error.bars="se")

AnovaModel.1 <- lm(Marketing$Sales ~ Marketing$day+Marketing$shelfht)
pander(Anova(AnovaModel.1))

AnovaModel.2 <- lm(Marketing$Sales ~ Marketing$shelfht)
pander(Anova(AnovaModel.2))

par(mfrow=c(1,2))
plot(AnovaModel.1, which=1:2)
plot(AnovaModel.2, which=1:2)


MarketingBF <- aov(Marketing$Sales ~ Marketing$shelfht)
plot(MarketingBF, which=1:2)
pander(summary(MarketingBF))

####HW 3
Weeks = c(1,3,2,2,1,3,3,2,1) 
Diet = c("Roughage", "Roughage", "Roughage", "Partial", "Partial",
         "Partial", "Full", "Full", "Full") 
Cow = c(1,2,3,1,2,3,1,2,3) 
Yield = c(608,711,766,716,885,832,845,1086,940)
Cows = data.frame(Weeks, Diet, Cow, Yield)

Cows$Weeks <- as.factor(Cows$Weeks)
Cows$Cow <- as.factor(Cows$Cow)
View(Cows)

CowsAov <- aov(Yield ~ Weeks + Diet + Cow, data = Cows)
summary(CowsAov)

par(mfrow=c(1,2))
plot(CowsAov, which=1:2)

plotmeans(Cows$Yield ~ Cows$Diet, error.bars="se")

FEVData <- read_csv("HW/ForcedExpiratoryVolume.csv")
View(FEVData)

FEV1 <- lm(FEV ~ Height*SmokingStatus, data = FEVData,
           contrasts=list(SmokingStatus=contr.sum))
Anova(FEV1, type="III") 

FEV2 <- lm(FEV~Height+SmokingStatus, data = FEVData,
           contrasts=list(SmokingStatus=contr.sum))
Anova(FEV2, type="III")

par(mfrow=c(1,2))
plot(FEV1,which=1:2)

par(mfrow=c(1,2))
hist(FEV2$residuals)
qqPlot(FEV2$residuals)

FEV3 <- lm(FEV ~ Age*SmokingStatus, data = FEVData,
           contrasts=list(SmokingStatus=contr.sum))
Anova(FEV3, type="III") 

FEV4 <- lm(FEV~Age+SmokingStatus, data = FEVData,
           contrasts=list(SmokingStatus=contr.sum))
Anova(FEV4, type="III")

par(mfrow=c(1,2))
plot(FEV3,which=1:2)

par(mfrow=c(1,2))
hist(FEV4$residuals)
qqPlot(FEV4$residuals)



### Randomization for semester project 

sample(1:5,5,replace=F)

sample(1:3,3,replace=F)


















