#BF 2

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

###########BF2-3

wear <- read_csv("wear.csv")
View(wear)

wear$prop <- as.factor(wear$prop)
wear$filler <- as.factor(wear$filler)

pander(favstats(wear$wear1~wear$prop)) 
pander(favstats(wear$wear1~wear$filler))

par(mfrow=c(3,2))

boxplot(wear$wear1~wear$prop, data=wear, id.method="y")
boxplot(wear$wear1~wear$filler, data=wear, id.method="y")

interaction.plot(wear$prop,wear$filler, wear$wear1)
interaction.plot(wear$filler,wear$prop, wear$wear1)

plotmeans(wear$wear1~wear$prop, error.bars="se") 
plotmeans(wear$wear1~wear$filler, error.bars="se")

##Running the test
wearAnova <- aov(wear1 ~ prop*filler, data=wear)
pander(summary(wearAnova))

par(mfrow=c(1,2))
plot(wearAnova, which=1:2)

####Checking for fit of transformation
boxcox(wearAnova)

wearAnova1 <- aov(log(wear1) ~ prop*filler, data=wear)
pander(summary(wearAnova1))

par(mfrow=c(1,2))
plot(wearAnova1, which=1:2)


###########BF2-4

Cancer <- read_csv("Cancer.csv")
View(Cancer)

Cancer$gender <- as.factor(Cancer$gender)

## This gives us Type I Sum of Squares in R
CancerAnova <- anova(lm(age ~ type*gender, data=Cancer))
pander(CancerAnova)

######rearranging the variables affects the p-value
CancerAnovaMix <- anova(lm(age ~ gender*type, data=Cancer))
pander(CancerAnovaMix)

## This gives us Type III Sum of Squares in R
## Need car package
CancerAnova1 <- lm(age~gender*type, data=Cancer, contrasts=list(type=contr.sum, gender=contr.sum))
pander(Anova(CancerAnova1, type="III")) 



































