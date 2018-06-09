## BF1 Enrichment

library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)
library(agricolae)

headinjury <- read_csv("HW/headinjury.csv")
View(headinjury)

Head <- aov(headinjury$HeadInjury ~ headinjury$Type)
pander(summary(Head))

TukeyHSD(Head, "headinjury$Type")
TestHead <- scheffe.test(Head, "headinjury$Type", group = TRUE, console = TRUE)
pairwise.t.test(headinjury$HeadInjury, headinjury$Type, "bonferroni")
pairwise.t.test(headinjury$HeadInjury, headinjury$Type, "none")



























