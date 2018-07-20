####SPRM HomeWork

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

### 3

Auxin <- read_csv("HW/auxin.csv")
Auxin$plant <- as.factor(Auxin$plant)
View(Auxin)

#Descriptive
favstats(Auxin$days~Auxin$plant)  
favstats(Auxin$days~Auxin$auxin)
favstats(Auxin$days~Auxin$deblading)

par(mfrow=c(3,2))

boxplot(Auxin$days~Auxin$auxin, id.method="y")
boxplot(Auxin$days~Auxin$deblading, id.method="y")
boxplot(Auxin$days~Auxin$plant, id.method="y")
interaction.plot(Auxin$auxin,Auxin$deblading, Auxin$days)
interaction.plot(Auxin$deblading,Auxin$auxin, Auxin$days)

plotmeans(Auxin$days~Auxin$auxin, error.bars="se") 
plotmeans(Auxin$days~Auxin$deblading, error.bars="se")
plotmeans(Auxin$days~Auxin$plant, error.bars="se")

#Inferential Statistics
splitplot <- aov(Auxin$days ~ Auxin$auxin + Auxin$plant + Auxin$auxin*Auxin$deblading)
summary(splitplot)
F=309.8/40.31
1-pf(F,3,12)

par(mfrow=c(1,2))
plot(splitplot,which=1:2)

hist(splitplot$residuals)
qqPlot(splitplot$residuals)

#### 4 
x <- rep(NA,20)
for (i in 2:20){
  x[i] <- power.anova.test(groups=4, between.var=var(c(0,0,0,6)), within.var=4.7^2, sig.level=0.05, n=i)$power
  
}
x
i
n <- c(1:20)
scatterplot(x~n, reg.line=FALSE, spread=FALSE, boxplots=FALSE)

Injury <- read_csv("HW/headinjury.csv")
View(Injury)



1-###Project
sample(1:9, 9, replace=FALSE)
power.anova.test(groups=9, between.var=var(c(0,0,0,0,0,0,0,0,12)),within.var=77, sig.level=0.05, power=.80)
sqrt(77)










