#### Split Plot Repeated Measures Quizzes

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

# 1-2
Dogs <- read_csv("HW/Diabetic Dogs.csv") %>% 
  mutate(Dog = as.factor(Dog))
View(Dogs)

DogsSplit <- aov(Dogs$LacticAcid ~ Dogs$Method +
                   as.factor(Dogs$Dog) + Dogs$Method*Dogs$Operations)
summary(DogsSplit)
F=320/63.5
1-pf(F,1,8)
# p-value for Method = 0.05500723
par(mfrow=c(1,2))
plot(DogsSplit,which=1:2)

DogsSplit2 <- aov(Dogs$LacticAcid ~ Dogs$Method * Dogs$Operations + 
                    Error(Dogs$Dog:Dogs$Method))
summary(DogsSplit2)

## 1-3  
power.anova.test(n=20, between.var=var(c(0,0,0,5)),
                 within.var=11.67^2, sig.level=0.05, groups = 4)
power.anova.test(power = 0.85, between.var=var(c(0,0,0,5)),
                 within.var=11.67^2, sig.level=0.05, groups = 4)

















































