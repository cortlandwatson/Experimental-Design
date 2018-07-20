
library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

Assembly <- read_csv("~/Downloads/Assemblytimes.csv")
View(Assembly)

Assembly$Sequence <- as.factor(Assembly$Sequence)

AnovaModel.1 <- (lm(Times ~ Gender*Sequence*Experience, data=Assembly))
Anova(AnovaModel.1)
##option #1 Checking equal variance and normality
par(mfrow=c(1,2))
plot(AnovaModel.1,which=1:2)
##option #2 Checking normality
par(mfrow=c(1,2))
hist(AnovaModel.1$residuals)

#HW
Soil <- read_csv("HW/Soilrunoff.csv")
Soil$Rain <- as.factor(Soil$Rain)
Soil$Log <- as.factor(Soil$Log)
Soil$Catch <- as.factor(Soil$Catch)
View(Soil)

AnovaModel.1 <- (lm(Soil$y ~ Soil$Rain*Soil$Log*Soil$Catch))
Anova(AnovaModel.1)
##option #1 Checking equal variance and normality
par(mfrow=c(1,2))
plot(AnovaModel.1,which=1:2)


Syrup <- read_csv("HW/softdrinksyrup.csv")
Syrup$Pressure <- as.factor(Syrup$Pressure)
Syrup$Design <- as.factor(Syrup$Design)
Syrup$Speed <- as.factor(Syrup$Speed)
View(Syrup)

AnovaModel.2 <- (lm(Syrup$Frothing ~ Syrup$Pressure*Syrup$Design*Syrup$Speed))
Anova(AnovaModel.2)
##option #1 Checking equal variance and normality
par(mfrow=c(1,2))
plot(AnovaModel.2,which=1:2)
