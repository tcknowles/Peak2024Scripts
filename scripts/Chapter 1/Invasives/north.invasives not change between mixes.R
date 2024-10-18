library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)

north <- read_csv("outputs/northmeta.rel.csv")


### Determine if invasive relative cover changes with seed mix ---------------------------------------------
# remove control
northnc <- north |> 
  filter(!Plot %in% c(40:42))

inv.Mix <- northnc |> 
  filter(northnc$Spp %in% c('POAPRA','EUPVIR','BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Sampler,Rep),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives


# Plot -------------------------------------------------------------------------------------------------------
inv.Mix_Plot <- inv.Mix  |> 
  mutate(Mix = recode(Mix, 
                       "Basic" = "Non-spike"))

mix_invasive_plot <- ggplot(data = inv.Mix_Plot, aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot(fill = c("#9fc8c8","#298c8c"))+  # color it
labs(x = "Seeding Treatment", y = "Avg Invasive Species Abundance (%)",
     title = "No Significant Difference in Invasive Abundance between Seed Mixes")+
theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))  # change title size and location

# save
ggsave("outputs/figs/Chapter 1/mix_invasive.png",mix_invasive_plot,
       width = 15, height = 10)    





# Stats ------------------------------------------------------------------------------------------------------
## im fucked here with the stats

hist(inv.Mix$sumAvg_Rel.Cover)  
shapiro.test(inv.Mix$sumAvg_Rel.Cover)  # not normal

mixmod <- glmmTMB(sumAvg_Rel.Cover ~ Mix+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                         data = inv.Mix, # using invasive data set
                         family = gaussian(link = "identity"))  # using beta probability distribution 
plot(simulateResiduals(mixmod)) 
testDispersion(mixmod)
testOutliers(mixmod) 
testZeroInflation(mixmod)
summary(mixmod)     
(means <- emmeans(mixmod, list(pairwise ~ Mix), adjust = "tukey"))
cld(means)
# no sig difference between seed mixes 
# BUT NEED TO WORK ON THIS CUZ MODEL DOESNT FIT






