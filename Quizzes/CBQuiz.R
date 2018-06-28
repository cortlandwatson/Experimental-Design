#CB 

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

##########1-2
auditor <- read_csv("Quizzes/auditor.csv")
auditor$block <- as.factor(auditor$block)
View(auditor)

plotmeans(auditor$score ~ auditor$block, error.bars="se")

AnovaModel.1 <- lm(auditor$score~auditor$block+auditor$method)
Anova(AnovaModel.1)

AnovaModel.2 <- lm(auditor$score~auditor$method)
Anova(AnovaModel.2)
#Can be done another way using aov rather than lm
AnovaModel.2_2 <- aov(auditor$score~auditor$method)
summary(AnovaModel.2_2)

par(mfrow=c(1,2))
plot(AnovaModel.1, which=1:2)
plot(AnovaModel.2, which=1:2)

###HW decomposition
Marketing <- read_csv("HW/marketing.csv")
Marketing$shelfht <- as.factor(Marketing$shelfht)
Marketing$day <- as.factor(Marketing$day)
View(Marketing)

plotmeans(Marketing$Sales ~ Marketing$day, error.bars="se")

AnovaModel.1 <- lm(Marketing$Sales~Marketing$day+Marketing$shelfht)
Anova(AnovaModel.1)

##########1-3
##Class Example
#We do not have to put character variables as.factor
Milk <- read_csv("Quizzes/MilkYield.csv")
View(Milk)

MilkAov <- aov(MilkYield ~ Cow + Weeks + Treatments, data = Milk)
summary(MilkAov)

par(mfrow=c(1,2))
plot(MpgAov, which=1:2)

MilkAov1 <- aov(MilkYield ~ Treatments, data = Milk)
summary(MilkAov1)

par(mfrow=c(1,2))
plot(MpgAov1, which=1:2)

#Quiz
Mpg <- read_csv("Quizzes/mpg.csv") %>% 
  mutate(driver = as.factor(driver), 
         model = as.factor(model),
         blend = as.factor(blend))
View(Mpg)

MpgAov <- aov(mpg ~ driver + model + blend, data = Mpg)
summary(MpgAov)

par(mfrow=c(1,2))
plot(MpgAov, which=1:2)

MpgAov1 <- aov(mpg ~ blend, data = Mpg)
summary(MpgAov1)

par(mfrow=c(1,2))
plot(MpgAov1, which=1:2)

## 1 - 4

Handwash <- read_csv("Quizzes/handwash.csv")
View(Handwash)

Handwash1 <- lm(CFUAfter ~ CFUBefore*Cleanser, data = Handwash,
           contrasts=list(Cleanser=contr.sum))
Anova(Handwash1, type="III") 

Handwash2 <- lm(CFUAfter ~ CFUBefore+Cleanser, data = Handwash,
                contrasts=list(Cleanser=contr.sum))
Anova(Handwash2, type="III")

par(mfrow=c(1,2))
plot(Handwash1,which=1:2)

par(mfrow=c(1,2))
hist(Handwash2$residuals)
qqPlot(Handwash2$residuals)









