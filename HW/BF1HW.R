## BF1 Enrichment

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)
library(agricolae)

headinjury <- read_csv("HW/headinjury.csv")
View(headinjury)

Head <- aov(headinjury$HeadInjury ~ headinjury$Type)
pander(summary(Head))

TukeyHSD(Head, "headinjury$Type")
TestHead <- scheffe.test(Head, "headinjury$Type", group = TRUE, console = TRUE)
pairwise.t.test(headinjury$HeadInjury, headinjury$Type, "bonferroni")
pairwise.t.test(headinjury$HeadInjury, headinjury$Type, "none")

## 2 using software
Bone <- read_csv("HW/Bonemineral.csv")
View(Bone)

favstats(Bone$BMD~Bone$Treatment)
par(mfrow=c(1,1))
plotmeans(Bone$BMD~Bone$Treatment, error.bars="se") 
par(mfrow=c(1,1))
qqmath(~Bone$BMD|Bone$Treatment)
histogram(~Bone$BMD|Bone$Treatment)

BoneAOV <- aov(Bone$BMD~Bone$Treatment)
summary(BoneAOV)

par(mfrow=c(1,2))
plot(BoneAOV, which=1:2)

favstats(Bone$BMD~Bone$Treatment)
par(mfrow=c(1,2))
qqPlot(BoneAOV$residuals)
hist(BoneAOV$residuals, dist="norm")

Cancer <- read_csv("HW/Cancer.csv")
View(Cancer)

CancerAOV <- aov(Cancer$days~Cancer$type)
summary(CancerAOV)

par(mfrow=c(1,2))
plot(CancerAOV, which=1:2)

favstats(Cancer$days~Cancer$type)
par(mfrow=c(1,2))
qqPlot(CancerAOV$residuals)
hist(CancerAOV$residuals, dist="norm")

#log transformation
CancerLog <- aov(log(Cancer$days)~Cancer$type)
summary(CancerLog)

par(mfrow=c(1,2))
plot(CancerLog, which=1:2)

favstats(log(Cancer$days)~Cancer$type)
par(mfrow=c(1,2))
qqPlot(CancerLog$residuals)
hist(CancerLog$residuals, dist="norm")



















