library(mosaic)
library(package)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)
library(readr)

BatteryLife <- read_csv("Exams/BatteryLife.csv")
SurfaceFinish <- read_csv("Exams/SurfaceFinish.csv")
View(BatteryLife)
View(SurfaceFinish)

##Battery life
BatteryLife$Material <- as.factor(BatteryLife$Material)
AnovaBL <- aov(BatteryLife$FailureTime ~ BatteryLife$Material)
pander(summary(AnovaBL))

par(mfrow=c(1,2))
plot(AnovaBL, which=1:2)

boxplot(BatteryLife$FailureTime ~ BatteryLife$Material)
plotmeans(BatteryLife$FailureTime ~ BatteryLife$Material)
pander(favstats(BatteryLife$FailureTime ~ BatteryLife$Material))

##Surface Finish
SurfaceFinish$Feedrate <- as.factor(SurfaceFinish$Feedrate)
SurfaceFinish$DepthofCut <- as.factor(SurfaceFinish$DepthofCut)
AnovaSF <- aov(SurfaceFinish$SurfaceFinish ~ 
                 SurfaceFinish$Feedrate*SurfaceFinish$DepthofCut)
pander(summary(AnovaSF))

plot(AnovaSF, which=1:2)

boxplot(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$Feedrate, id.method="y")
boxplot(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$DepthofCut, id.method="y")
plotmeans(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$Feedrate, error.bars="se")
plotmeans(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$DepthofCut, error.bars="se")
interaction.plot(SurfaceFinish$Feedrate, SurfaceFinish$DepthofCut,
                 SurfaceFinish$SurfaceFinish)
interaction.plot(SurfaceFinish$DepthofCut, SurfaceFinish$Feedrate,
                 SurfaceFinish$SurfaceFinish)
pander(favstats(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$Feedrate))
pander(favstats(SurfaceFinish$SurfaceFinish ~ SurfaceFinish$DepthofCut))

#Interaction
AnovaSFT <- aov((SurfaceFinish$SurfaceFinish)^1/2 ~ 
                 SurfaceFinish$Feedrate*SurfaceFinish$DepthofCut)
pander(summary(AnovaSFT))
plot(AnovaSFT, which=1:2)
























