SLA<-read.csv("Data/SLA_Canadian cities.csv")

library(vegan)
library(car)
library(tidyverse)

glimpse(SLA)

unique(SLA$City)

## Histogram

histogram <- ggplot(SLA, aes(x=value, fill=City))+
  facet_wrap(~City)+
  geom_histogram(bins=50)
histogram

## one way ANOVA

ANOVA<- lm(value ~ City, data=SLA)
summary(ANOVA)
Anova(ANOVA, type = "2")
#Response: value
#Sum Sq   Df F value Pr(>F)
#City         889    5  1.5407 0.1739
#Residuals 253337 2195 


