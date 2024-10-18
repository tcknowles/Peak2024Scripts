library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)
library(ggplot2)

north <- read_csv("outputs/northmeta.rel.csv")
invasives <- north |> 
  filter(north$Spp %in% c('POAPRA','EUPVIR','BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Rep,Sampler, Spp),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives

# Herbicide --------------------------------------------------------------------------------------------------


# fixing herbicide names
invasives <- invasives  |> 
  mutate(Herb = recode(Herb, 
                       "A" = "Aminopyrachlor", 
                       "G" = "Glyphosate", 
                       "P+I" = "Picloram + Imazapic",
                       "Q" = "Quinclorac",
                       "Q+D" = "Quinclorac + Diflufenzopyr"))

# ggplot via species
ggplot(data = invasives, aes(x = Herb, y = sumAvg_Rel.Cover))+
geom_boxplot()+
  scale_fill_manual(
    values = c("skyblue", "royalblue", "blue", "navy", "salmon"))+
labs(x = "Herbicide Pretreatment", y = "Avg Invasive Species Abundance",
     title = "Glyphosate best at reducing invasive abundance") +
  theme_bw()
    
# stats on it
hist(invasives$sumAvg_Rel.Cover)
betaherb <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Herb +(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                   data = invasives, # using invasives data set
                   family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betaherb))   # no significant dharmas
emmeans(betaherb, list(pairwise ~ Herb), adjust = "tukey")  # only glyphosate reduces invasives
# Seed mix ---------------------------------------------------------------------------------------------------
#filter out control
ncinvasives <- invasives |> 
  filter(invasives$Plot %in% c(10:39))   #removing controls
ncinvasives <- ncinvasives  |> 
  mutate(Mix = recode(Mix, 
                       "Basic" = "Non-spiked", 
                       "Spike" = "Spiked"))
# ggplot
ggplot(data = ncinvasives, aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot()+
labs(x = "Seed Mix", y = "Avg Invasive Species Abundance",
     title = "No chance in invasive abundance between seed mixes") +
  theme_bw()

