###Project and descriptive statistics
library(mosaic)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(CARS)
library(pander)
library(MASS)
library(gplots)
library(car)

AirplaneData <- read_csv("Projects/AirplaneData.csv")
View(AirplaneData)


#Graphics

ggplot(AirplaneData, aes(x = Plane, y = Dist, color = Winglets)) +
  geom_boxplot() +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Paper Airplane Distances by Design", 
       x = "Airplane Design", y = "Distance (inches)") +
  theme_minimal() 

ggplot(AirplaneData, aes(x = Winglets, y = Dist, color = Plane)) +
  geom_jitter(size = 2, width = .35, alpha = .8) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Paper Airplane Distances by Design", 
       subtitle = "Each individual observation is measured by the points", 
       x = "Airplane Design", y = "Distance (inches)") +
  theme_minimal() 

#Descriptive Statistics
favstats(AirplaneData$Dist ~ AirplaneData$Plane + AirplaneData$Winglets)
mean(AirplaneData$Dist ~ AirplaneData$Plane + AirplaneData$Winglets)

interaction.plot(AirplaneData$Dist, AirplaneData$Plane, 
                 AirplaneData$Winglets)
interaction.plot(AirplaneData$Dist, AirplaneData$Winglets,
                 AirplaneData$Plane)

plotmeans(AirplaneData$Dist ~ AirplaneData$Plane, error.bars="se") 
plotmeans(AirplaneData$Dist ~ AirplaneData$Winglets, error.bars="se")

power.anova.test(groups=9, between.var=var(c(0,0,0,0,0,0,0,0,33)),
                 within.var=5582, sig.level=0.05, n=10)
#This gives us the power to be able to detect at 80% a difference of
#33 inches.











