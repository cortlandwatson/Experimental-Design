#BF2-3
library(readr)
library(mosaic)
library(pander)
library(car)
library(gplots)
library(MASS)

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





