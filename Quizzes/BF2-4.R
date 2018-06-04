library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

#BF2-4

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



































