library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)


north <- read_csv("outputs/northmeta.rel.csv")

### Determine if cool season grass relative cover changes with seed mix ---------------------------------------------
CSG.Mix <- north |> 
  filter(north$Spp %in% c('POAPRA', 'BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Sampler,Rep),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))      
ggplot(data = CSG.Mix, aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot()

hist(CSG.Mix$sumAvg_Rel.Cover)  # assessing the distribution - right skewed so none normal
CSGmod <- glmmTMB(sumAvg_Rel.Cover ~ Mix+(1|Rep)+(1|Sampler), 
                         data = CSG.Mix, # using leafy spurge only data set
                         family = gaussian(link = "identity"))  # using beta probability distribution 
plot(simulateResiduals(CSGmod))   # no significant dharmas
testDispersion(CSGmod)# test dispersion - none significant so all good
testOutliers(CSGmod) # test for outliers - non significant
testZeroInflation(CSGmod) # test for zero inflation - none significant
summary(CSGmod)      
(CSGembeta <- emmeans(CSGmod, list(pairwise ~ Mix), adjust = "tukey"))  
cld(CSGembeta)
# seed mix doesn't effect cool season grasses






# how are the different CSG effected individually? -----------------------------------------------------------

CSG.ind <- north |> 
  filter(north$Spp %in% c('POAPRA', 'BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Spp),
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))  # sum rel cover of leafy spurge for each treatment - sum or mean wont matter - its just one value for each plot    
ggplot(data = CSG.ind, aes(x = Mix, y = sumAvg_Rel.Cover, col = Spp ))+
  geom_boxplot()
# doesnt show much difference

