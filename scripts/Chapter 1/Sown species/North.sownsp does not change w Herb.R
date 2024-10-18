library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")

sown <- north |> 
  filter(north$seeded == "Y") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Rep,Sampler),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros


# To determine if herbicide effects relative cover of sown species ---------------------------------------

ggplot(data = sown, mapping = aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()

betamodherb <- glmmTMB(I((sumAvg_Rel.Cover/100)) ~ Herb+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and removed the 0.000001
                       data = sown, # using sown dataset
                       family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betamodherb)) # no sig - good to go  
testDispersion(betamodherb)# test dispersion - none significant so all good
testOutliers(betamodherb) # test for outliers - non significant
testZeroInflation(betamodherb) # test for zero inflation - none significant
summary(betamodherb)     
(means <- emmeans(betamodherb, list(pairwise ~ Herb), adjust = "tukey"))
cld(means)
# no sig difference of abundance of sown species between herbicide treatments