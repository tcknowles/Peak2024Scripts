library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")


### Determine if leafy spurge relative cover changes with seed mix ---------------------------------------------
LS.Mix <- north |> 
  filter(north$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix,Sampler,Rep),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))      
ggplot(data = LS.Mix, aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot()
LSMixmeans <- LS.Mix |> 
  group_by(Mix) |> 
  summarize(Cover = mean(sumAvg_Rel.Cover))
hist(LS.Mix$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
LS.Mixmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Mix+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                         data = LS.Mix, # using leafy spurge only data set
                         family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(LS.Mixmodbeta))   # no significant dharmas
testDispersion(LS.Mixmodbeta)# test dispersion - none significant so all good
testOutliers(LS.Mixmodbeta) # test for outliers - non significant
testZeroInflation(LS.Mixmodbeta) # test for zero inflation - none significant
summary(LS.Mixmodbeta)      # intercept here is the A herbicide
emmeans(LS.Mixmodbeta, list(pairwise ~ Mix), adjust = "tukey"))  
# Seed mix alone does not change leafy spurge abundance 




