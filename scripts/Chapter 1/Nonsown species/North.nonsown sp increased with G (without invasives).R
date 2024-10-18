library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")
nonsown <- north |> 
  filter(north$seeded == "N") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

# To determine if herbicide effects relative cover of nonsown species ---------------------------------------

ggplot(data = nonsown, mapping = aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()
# NO DIFFERENCE WITH INVASIVES INCLUDED

## lets try without invasives
ncinonsown <- north |> 
  filter(Plot %in% c(10:39)) |>    #removing controls
  filter(!Spp %in% c("POAPRA",'BROINE','EUPVIR')) |> 
  filter(seeded == "N") |> # removing invasives
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = ncinonsown, mapping = aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()  ## glyphosate seems more

hist(ncinonsown$sumAvg_Rel.Cover)

gamma <- glmmTMB(I(sumAvg_Rel.Cover+0.000001) ~ Herb+(1|Rep)+(1|Sampler), 
               data = ncinonsown,
               family = Gamma(link = "identity"))  

plot(simulateResiduals(gamma)) 
testDispersion(gamma)# test dispersion - none significant so all good
testOutliers(gamma) # test for outliers - non significant
testZeroInflation(gamma) # test for zero inflation - none significant
summary(gamma)     
(means <- emmeans(gamma, list(pairwise ~ Herb), adjust = "tukey"))

# glyphosate has sig increased nonsown species


