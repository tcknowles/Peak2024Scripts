library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)


north <- read_csv("outputs/northmeta.rel.csv")



# Does Cool season grass abundance change w herbicide? -------------------------------------------------------

CSG <- north |> 
  filter(north$Spp %in% c('POAPRA', 'BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Sampler,Rep),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = CSG, aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()   # looks like G reduces CSGs the most

#LM analysis
hist(CSG$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal

CSGmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                      data = CSG, # using leafy spurge only data set
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(CSGmodbeta))   # no significant dharmas
testDispersion(CSGmodbeta)# test dispersion - none significant so all good
testOutliers(CSGmodbeta) # test for outliers - non significant
testZeroInflation(CSGmodbeta) # test for zero inflation - none significant
summary(CSGmodbeta)      # intercept here is the A herbicide
means <- emmeans(CSGmodbeta, list(pairwise ~ Herb), adjust = "tukey")
cld(means)
# herbicides have varying effects on CSGs