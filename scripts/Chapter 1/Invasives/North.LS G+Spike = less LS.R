library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")



### How does the interaction between herb and mix change leafy spurge cover? -----------------------------------
LSnc <- north |> 
  filter(north$Plot %in% c(10:39))     #removing controls
LSint <- LSnc |> 
  filter(LSnc$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix,Rep,Sampler),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    

ggplot(data = LSint, aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()


#LM analysis
hist(LSint$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
LSintmodbeta <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ (Herb*Mix)+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                        data = LSint, # using leafy spurge only data set
                        family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(LSintmodbeta))   # no significant dharmas
testDispersion(LSintmodbeta)# test dispersion - none significant so all good
testOutliers(LSintmodbeta) # test for outliers - non significant
testZeroInflation(LSintmodbeta) # test for zero inflation - none significant
summary(LSintmodbeta)  
confint(LSintmodbeta)
means<- emmeans(LSintmodbeta, list(pairwise ~ Herb*Mix), adjust = "tukey")
cld(means)
# leafy spurge is only changed with G and spike to reduce LS
