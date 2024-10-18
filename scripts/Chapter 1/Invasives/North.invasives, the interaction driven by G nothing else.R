library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")

### How does the interaction between herb and mix change invasive cover? -----------------------------------
invint <- north |> 
  filter(north$Spp %in% c('POAPRA','EUPVIR','BROINE'))  |> 
  summarize(.by = c(Plot,Herb,Mix,Sampler,Rep),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of invasive for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = invint, aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()
# lets try and remove control
invint <- invint |> 
  filter(invint$Plot %in% c(10:39))   #removing controls

#LM analysis
hist(invint$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
invintbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb*Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                      data = invint, # using leafy spurge only data set
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(invintbeta))   # no significant dharmas
testDispersion(invintbeta)# test dispersion - none significant so all good
testOutliers(invintbeta) # test for outliers - non significant
testZeroInflation(invintbeta) # test for zero inflation - none significant
summary(invintbeta)      # intercept here is the A herbicide
confint(invintbeta)     # confience intervals
(means <- emmeans(invintbeta, list(pairwise ~ Herb*Mix), adjust = "tukey"))
cld(means)
# Only sig was from the herbicide differences, nothing with the seed mix


