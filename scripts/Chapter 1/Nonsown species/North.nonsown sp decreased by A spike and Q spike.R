library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)
library(multcomp)

north <- read_csv("outputs/northmeta.rel.csv")
nonsown <- north |> 
  filter(north$seeded == "N") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

# To determine if herbicide effects relative cover of nonsown species ---------------------------------------

ggplot(data = nonsown, mapping = aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()
# NO DIFFERENCE WITH INVASIVES INCLUDED

## lets try without invasives
ncinonsown <- north |> 
  filter(Plot %in% c(10:39)) |>    #removing controls
  filter(!Spp %in% c("POAPRA",'BROINE','EUPVIR')) |> 
  filter(seeded == "N") |> # removing invasives
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = ncinonsown, mapping = aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()  ## glyphosate seems more

hist(ncinonsown$sumAvg_Rel.Cover)

beta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb*Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                      data = ncinonsown, # using leafy spurge only data set
                      family = beta_family(link = "logit"))  # using beta probability distribution 

plot(simulateResiduals(beta)) 
testDispersion(beta)# test dispersion - none significant so all good
testOutliers(beta) # test for outliers - non significant
testZeroInflation(beta) # test for zero inflation - none significant
summary(beta)     
(means <- emmeans(beta, list(pairwise ~ Herb*Mix), adjust = "tukey"))
cld(means)
# A spike and Q spike are best at reducing nonsowns
