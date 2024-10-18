library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")
### Determine if leafy spurge relative cover changes with herbicides -------------------------------------------
LS <- north |> 
  filter(north$Spp %in% c('EUPVIR')) |> 
  summarize(.by = c(Plot,Herb,Mix,Rep,Sampler),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = LS, aes(x = Herb, y = sumAvg_Rel.Cover))+
  geom_boxplot()

#LM analysis
hist(LS$sumAvg_Rel.Cover)  # assessing the distribution
LSmodbeta <- glmmTMB(sumAvg_Rel.Cover ~ Herb+(1|Rep)+(1|Sampler),
                     data = LS, # using leafy spurge only data set
                     family = gaussian(link = "identity"))  # using normal here
plot(simulateResiduals(LSmodbeta))   # no significant dharmas
testDispersion(LSmodbeta)# test dispersion - none significant so all good
testOutliers(LSmodbeta) # test for outliers - non significant
testZeroInflation(LSmodbeta) # test for zero inflation - none significant
summary(LSmodbeta)      # intercept here is the A herbicide
emmeans(LSmodbeta, list(pairwise ~ Herb), adjust = "tukey"))  
# all different from control
# Q different from Q+D and G
