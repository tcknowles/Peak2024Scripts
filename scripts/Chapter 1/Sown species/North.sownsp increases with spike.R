library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")
northnc <- north |> 
  filter(!Plot %in% c(40:42))

sown <- northnc |> 
  filter(northnc$seeded == "Y") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Rep,Sampler),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

# How does does species cover change with seeding treatment?----------------------

mix_sown_plot <- ggplot(data = sown,
       mapping = aes(x = Mix, y = sumAvg_Rel.Cover))+
geom_boxplot(fill = c("#9fc8c8","#298c8c"))+  # color it
  labs(x = "Seeding Treatment", y = "Seeded Species Abundance (%)",
       title = "Spike Seeding Increases Seeded Species Abundance")+
  theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))  # change title size and location
ggsave("outputs/figs/Chapter 1/mix_sown.png", mix_sown_plot,   # name your path, then which object you want to save
       width = 15, height = 10)                           # set width and height



# Stats ------------------------------------------------------------------------------------------------------


hist(sown$sumAvg_Rel.Cover)  # to get the distribution 

betamodmix <- glmmTMB(I((sumAvg_Rel.Cover/100)) ~ Mix+(1|Rep)+(1|Sampler), # beta needs to be from 0-1, and removed the 0.000001
                      data = sown, # using sown dataset
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betamodmix)) # no sig - good to go  
 testDispersion(betamodmix)# test dispersion - none significant so all good
testOutliers(betamodmix) # test for outliers - non significant
testZeroInflation(betamodmix) # test for zero inflation - none significant
summary(betamodmix)     
emmeans(betamodmix, list(pairwise ~ Mix), adjust = "tukey")
# sig differences between seed mixes on abundance of sown species

