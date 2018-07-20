#####Test 2
library(mosaic)
library(CARS)
library(MASS)
library(gplots)
library(car)
library(readr)
library(readxl)

B <- read_csv("Exams/ActiveBattery.csv")
B$Temp.Chamber <- as.factor(B$Temp.Chamber)
View(B)
B.aov <- aov(B$Activated.Lives ~ B$Temperature + B$Electrolyte + B$Temperature*B$Temp.Chamber)
summary(B.aov)
F=3.09/12.76
1-pf(F,2,15)

par(mfrow=c(1,2))
plot(B.aov,which=1:2)
F


N <- read_csv("Exams/NozzleDesign.csv")
N$Nozzle <- as.factor(N$Nozzle)
N$Velocity <- as.factor(N$Velocity)
View(N)
N.aov <- aov(N$Response ~ N$Velocity + N$Nozzle) 
summary(N.aov)
plot(N.aov,which=1:2)



P <- read_csv("Exams/pigfeed.csv")
P$Treatment <- as.factor(P$Treatment)
View(P)
P.ancova <- lm(Final.Weight ~ Initial.Weight*Treatment, data=P, contrasts = list(Treatment=contr.sum))
Anova(P.ancova, type = "III")
plot(P.ancova,which=1:2)

P.ancova2 <- lm(Final.Weight ~ Initial.Weight+Treatment, data=P, contrasts = list(Treatment=contr.sum))
Anova(P.ancova2, type = "III")
plot(P.ancova2,which=1:2)


power.anova.test(groups=5, between.var = var(c(10,5,15,0,5)),within.var = 10^2,sig.level = 0.05, power = .80)
power.anova.test(groups=5, between.var = var(c(10,5,15,0,5)),within.var = 10^2,sig.level = 0.05, n=20)
