library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")


# Without invasives ------------------------------------------------------------------------------------------

## lets remove invasives from here
ncinonsown <- north |> 
  filter(Plot %in% c(10:39)) |>    #removing controls
  filter(!Spp %in% c("POAPRA",'BROINE','EUPVIR')) |>  # removing invasives
  filter(seeded == "N") |> # nonsown only
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ncinonsown_ready <- ncinonsown  |> 
  mutate(Mix = recode(Mix, 
                      "Basic" = "Non-spike"))
mix_nonsown_plot <- ggplot(data = ncinonsown_ready,
                        mapping = aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot(fill = c("#9fc8c8","#298c8c"))+  # color it
  labs(x = "Seeding Treatment", y = "Non-Seeded Species Abundance (%)",
       title = "Spike Seeding Decreases Non-Seeded Species Abundance")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))  # change title size and location
ggsave("outputs/figs/Chapter 1/mix_nonsown.png", mix_nonsown_plot,   # name your path, then which object you want to save
       width = 15, height = 10)                           # set width and height


# Stats ------------------------------------------------------------------------------------------------------
hist(ncinonsown$sumAvg_Rel.Cover) 

betamodmix <- glmmTMB(I((sumAvg_Rel.Cover/100)) ~ Mix+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and removed the 0.000001
                      data = ncinonsown, # using sown dataset
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betamodmix)) # no sig - good to go  
testDispersion(betamodmix)# test dispersion - none significant so all good
testOutliers(betamodmix) # test for outliers - non significant
testZeroInflation(betamodmix) # test for zero inflation - none significant
summary(betamodmix)     
(means <- emmeans(betamodmix, list(pairwise ~ Mix), adjust = "tukey"))
cld(means)
## sig differences between groups 






# With invasives present for nonsown species -----------------------------------------------------------------

northnc <- north |> 
  filter(!Plot %in% c(40:42))      # remove controls

nonsown <- northnc |> 
  filter(northnc$seeded == "N") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros


(mix_nonsown_plot <- ggplot(data = nonsown,
                            mapping = aes(x = Mix, y = sumAvg_Rel.Cover)))+
  geom_boxplot()

# Stats ------------------------------------------------------------------------------------------------------

hist(nonsown$sumAvg_Rel.Cover) 

betamodmix <- glmmTMB(I((sumAvg_Rel.Cover/100)+0.00001) ~ Mix+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and removed the 0.000001
                      data = nonsown, # using sown dataset
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betamodmix)) # no sig - good to go  
testDispersion(betamodmix)# test dispersion - none significant so all good
testOutliers(betamodmix) # test for outliers - non significant
testZeroInflation(betamodmix) # test for zero inflation - none significant
summary(betamodmix)     
(means <- emmeans(betamodmix, list(pairwise ~ Mix), adjust = "tukey"))
cld(means)





