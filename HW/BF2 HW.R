##HW BF2 using software

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

Bean <- read_csv("snapbean.csv")
Programmers <- read_csv("programmers.csv")
View(Bean)
View(Programmers)

#snapbean
Bean$sowdate <- as.factor(Bean$sowdate)
Bean$variety <- as.factor(Bean$variety)

pander(favstats(Bean$Yield~Bean$sowdate)) 
pander(favstats(Bean$Yield~Bean$variety))

par(mfrow=c(3,2))

boxplot(Bean$Yield~Bean$sowdate, data=Bean, id.method="y")
boxplot(Bean$Yield~Bean$variety, data=Bean, id.method="y")

interaction.plot(Bean$sowdate,Bean$variety, Bean$Yield)
interaction.plot(Bean$variety,Bean$sowdate, Bean$Yield)

plotmeans(Bean$Yield~Bean$sowdate, error.bars="se") 
plotmeans(Bean$Yield~Bean$variety, error.bars="se")

BeanAnova <- aov(Yield ~ sowdate*variety, data=Bean)
pander(summary(BeanAnova))

par(mfrow=c(1,2))
plot(BeanAnova, which=1:2)

#programmers
#TimePredE ~ LgSysEx * YearsOfExp
Programmers$LgSysEx <- as.factor(Programmers$LgSysEx)
Programmers$YearsOfExp <- as.factor(Programmers$YearsOfExp)

pander(favstats(Programmers$TimePredE~Programmers$LgSysEx)) 
pander(favstats(Programmers$TimePredE~Programmers$YearsOfExp))

par(mfrow=c(3,2))

boxplot(Programmers$TimePredE~Programmers$LgSysEx, data=Programmers, id.method="y")
boxplot(Programmers$TimePredE~Programmers$YearsOfExp, data=Programmers, id.method="y")

interaction.plot(Programmers$LgSysEx, Programmers$YearsOfExp, Programmers$TimePredE)
interaction.plot(Programmers$YearsOfExp, Programmers$LgSysEx, Programmers$TimePredE)

plotmeans(Programmers$TimePredE~Programmers$LgSysEx, error.bars="se") 
plotmeans(Programmers$TimePredE~Programmers$YearsOfExp, error.bars="se")

ProgrammersAnova <- aov(TimePredE ~ LgSysEx*YearsOfExp, data=Programmers)
pander(summary(ProgrammersAnova))

par(mfrow=c(1,2))
plot(ProgrammersAnova, which=1:2)

ProgrammersAnova <- aov(sqrt(TimePredE) ~ LgSysEx*YearsOfExp, data=Programmers)
pander(summary(ProgrammersAnova))

par(mfrow=c(1,2))
plot(ProgrammersAnova, which=1:2)


































