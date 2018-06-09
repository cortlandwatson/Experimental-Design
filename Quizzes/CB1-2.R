## CB 1-2

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

auditor <- read_csv("Quizzes/auditor.csv")
auditor$block <- as.factor(auditor$block)
View(auditor)

plotmeans(auditor$score ~ auditor$block, error.bars="se")

AnovaModel.1 <- lm(auditor$score~auditor$block+auditor$method)
Anova(AnovaModel.1)

AnovaModel.2 <- lm(auditor$score~auditor$method)
Anova(AnovaModel.2)

par(mfrow=c(1,2))
plot(AnovaModel.1, which=1:2)
plot(AnovaModel.2, which=1:2)





























