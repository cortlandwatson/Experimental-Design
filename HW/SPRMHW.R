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
power.anova.test(groups=4, between.var=var(c(21,23,25,27)),within.var=4.7, sig.level=0.05, n=2)
# 2=.31, 3=.64, 4=.84, 5=.94, 6=.97, 7=.993, 8=.998, 9=.9994, 10=.9998, 11=.99995, 
# 12=.99998, 13=.999997, 14=.9999994, 15=.9999998, 16=1, 17=1, 18=1, 19=1, 20=1
x <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
y <- c(.31,.64,.84,.94,.97,.993,.998,.9994,.9998,.99995,.99998,.999997,.9999994,.9999998,1,1,1,1,1)
Power <- data.frame(x,y)
plot(Power)
power.anova.test(groups=5, between.var=var(c(0,0,0,0,10)),within.var=11.67^2, sig.level=0.05, n=25)

x <- rep(NA,20)
for (i in 2:20){
  x[i] <- power.anova.test(groups=4, between.var=var(c(0,2,4,6)), within.var=4.7^2, sig.level=0.05, n=i)$power
  
}
x
i
n <- c(1:20)
scatterplot(x~n)


###Project
sample(1:2, 8, replace=TRUE)












