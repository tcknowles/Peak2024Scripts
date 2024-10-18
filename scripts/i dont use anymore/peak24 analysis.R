library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)

north <- read_csv("outputs/northmeta.rel.csv")

# To determine if herbicide effects relative cover of invasive species ---------------------------------------

invasives <- north |> 
  filter(north$Spp %in% c('POAPRA','EUPVIR','BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives


ggplot(data = invasives, aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot() # here we see the herbicides effect invasives differently


# linear model with normal distribution
hist(invasives$Avg_Rel.Cover) # heavily right skewed so normal prob wont work
lm_model <- lm(Avg_Rel.Cover ~ Herb, data = invasives)
summary(lm_model) 
simulationoutput <- simulateResiduals(lm_model, plot = T) #dharma on model indicates no homogeniety of variance

# try a beta
beta_mod <- glmmTMB(I((Avg_Rel.Cover/100)+0.00001) ~ Herb, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                    data = invasives, # using invasives data set
                    family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(beta_mod))   # no significant dharmas
testDispersion(beta_mod)# test dispersion - all good
testOutliers(beta_mod) # test for outliers - all good
testZeroInflation(beta_mod) # test for zero inflation - all good
summary(beta_mod)      # intercept here is the A herbicide
confint(beta_mod)     # confience intervals
(em.beta <- emmeans(beta_mod, list(pairwise ~ Herb), adjust = "tukey"))  
  
# turns out em.beta is a list of 2 - emmeans of herb and pairwise differences
# lets extractthe emmeans of herb to run the cld
em.beta_emmeans <- em.beta[[1]]
em.beta_pairwise <- em.beta[[2]]

cld(em.beta_pairwise, interval = "prediction", level = 0.05, Letters = letters)
?cld
?emmeans()
class(em.beta)
em.beta

# try a gamma

# try a zero inflated gamma







### Determine if leafy spurge relative cover changes with seed mix ---------------------------------------------
LS.Mix <- north |> 
  filter(north$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))      
ggplot(data = LS.Mix, aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot()

hist(LS.Mix$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
LS.Mixmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                         data = LS.Mix, # using leafy spurge only data set
                         family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(LS.Mixmodbeta))   # no significant dharmas
testDispersion(LS.Mixmodbeta)# test dispersion - none significant so all good
testOutliers(LS.Mixmodbeta) # test for outliers - non significant
testZeroInflation(LS.Mixmodbeta) # test for zero inflation - none significant
summary(LS.Mixmodbeta)      # intercept here is the A herbicide
confint(LS.Mixmodbeta)     # confience intervals
(LSembeta <- emmeans(LS.Mixmodbeta, list(pairwise ~ Mix), adjust = "tukey"))  
# Seed mix alone does not change leafy spurge abundance 

### Determine if leafy spurge relative cover changes with herbicides -------------------------------------------
LS <- north |> 
  filter(north$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = LS, aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()

#LM analysis
hist(LS$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
LSmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                     data = LS, # using leafy spurge only data set
                     family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(LSmodbeta))   # no significant dharmas
testDispersion(LSmodbeta)# test dispersion - none significant so all good
testOutliers(LSmodbeta) # test for outliers - non significant
testZeroInflation(LSmodbeta) # test for zero inflation - none significant
summary(LSmodbeta)      # intercept here is the A herbicide
confint(LSmodbeta)     # confience intervals
(LSembeta <- emmeans(LSmodbeta, list(pairwise ~ Herb), adjust = "tukey"))  
# inference here is that all are different from control, but not a significant difference between the different herbs



### How does the interaction between herb and mix change leafy spurge cover? -----------------------------------
LSint <- north |> 
  filter(north$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = LSint, aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()
# lets try and remove control
LSint <- LSint |> 
  filter(LSint$Plot %in% c(10:39))   #removing controls

#LM analysis
hist(LSint$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
LSintmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb*Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                        data = LSint, # using leafy spurge only data set
                        family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(LSintmodbeta))   # no significant dharmas
testDispersion(LSintmodbeta)# test dispersion - none significant so all good
testOutliers(LSintmodbeta) # test for outliers - non significant
testZeroInflation(LSintmodbeta) # test for zero inflation - none significant
summary(LSintmodbeta)      # intercept here is the A herbicide
confint(LSintmodbeta)     # confience intervals
(LSintembeta <- emmeans(LSintmodbeta, list(pairwise ~ Herb*Mix), adjust = "tukey"))  
# leafy spurge is not significantly changed with herb or mix











