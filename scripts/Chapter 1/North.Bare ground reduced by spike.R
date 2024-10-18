library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)

# Bare ground ------------------------------------------------------------------------------------------------
northnorm <- read_csv("outputs/northmeta.csv")

bare <- northnorm |> 
  filter(northnorm$Spp == 'Bare ground') |> 
  summarize(.by = c(Plot,Spp,Herb,Mix),    # keep plot, herb. and mix
            Sum_Avg_Cover= sum(Avg_Cover))             # takes average cover of invasives

ggplot(data = bare, aes(x = Mix, y = Sum_Avg_Cover))+
  geom_boxplot()
# spike reduced bare ground by 10-15%
ggplot(data = bare, aes(x = Herb, y = Sum_Avg_Cover))+
  geom_boxplot()
#glyphosate did not have the most bare ground

#analysis for bare ground
hist(bare$Sum_Avg_Cover)

gammabare <- glmmTMB(I(Sum_Avg_Cover+0.00001) ~ Mix, 
                     data = bare, 
                     family = Gamma(link = "inverse"))
plot(simulateResiduals(gammabare))   # KS, dispersion significant


barebeta <- glmmTMB(I((Sum_Avg_Cover/100)+0.00001) ~ Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                    data = bare, # using invasive data set
                    family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(barebeta)) # all good
summary(barebeta)     
emmeans(barebeta, list(pairwise ~ Mix), adjust = "tukey")  
# no sig difference between seed mixes, but p value of .18 so close, real close. add random effects next?


# try normal?
gaumod <- glm(Sum_Avg_Cover ~ Mix, 
              data = bare, 
              family = gaussian(link = "identity"))
plot(simulateResiduals(gaumod))   # nothing significant
#lets continue with the normal distribution
summary(gaumod)     
emmeans(gaumod, list(pairwise ~ Mix), adjust = "tukey")
# no significant difference between seed mixes but p value is 0.12







