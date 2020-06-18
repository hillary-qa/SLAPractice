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

Summary<- SLA %>% 
  group_by(City) %>% 
  summarise(SLA.avg=mean(value),
            SLA.sd=sd(value),
            SLA.n=length(value),
            SLA.se=sd(value)/sqrt(n()))
Summary
#City      SLA.avg SLA.sd SLA.n SLA.se
#<fct>       <dbl>  <dbl> <int>  <dbl>
#  1 Edmonton     22.7   9.25   163  0.724
#2 Halifax      24.0  10.0    358  0.530
#3 Montreal     24.8  11.5    514  0.505
#4 Toronto      24.0  10.0    545  0.430
#5 Vancouver    23.8  11.9    504  0.530
#6 Winnipeg     22.6   9.42   117  0.871

##Figure
ANOVA.fig<- ggplot(Summary,aes(City,SLA.avg, fill=City))+
  geom_col()+
  geom_errorbar(aes(ymin=SLA.avg-SLA.se,ymax=SLA.avg+SLA.se),width=0.2)+
  theme_classic()+
  ylim(0,40)

ANOVA.fig

ggsave("Figures/SLA_barchart.JPEG")

