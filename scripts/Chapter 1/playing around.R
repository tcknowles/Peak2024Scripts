library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)


north <- read_csv("outputs/northmeta.rel.csv")
ncinonsown <- north |> 
  filter(Plot %in% c(10:39)) |>    #removing controls
  filter(!Spp %in% c("POAPRA",'BROINE','EUPVIR')) |> 
  filter(seeded == "N") |> # removing invasives 
  
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros




# exotics ----------------------------------------------------------------------------------------------------

exotics <- north |> 
  filter(north$origin == "Intro") |> 
summarise(.by = c(Plot, seeded,Mix, Herb,Rep,Sampler),
          sumAvg_Rel.Cover = sum(Avg_Rel.Cover))
ggplot(data = exotics,
       mapping = aes(x = Mix, y = sumAvg_Rel.Cover))+
  geom_boxplot()

hist(exotics$sumAvg_Rel.Cover)  # wonky distribution...
norm <- glmmTMB(sumAvg_Rel.Cover ~ Mix+(1|Rep)+(1|Sampler),
                     data = exotics, # using sown dataset
                     family = gaussian(link = "identity"))  # using gaussian probability distribution
plot(simulateResiduals(norm)) # no sig - good to go 
emmeans(norm, list(pairwise ~ Mix), adjust = "tukey")  # only glyphosate reduces invasives






nonsown_unite <- ncnorth |>     # unite fn groups
  unite("fngroup",                   # new column
        origin, Duration,   # unite origin, fngroup, and duration
        sep = "_", remove = T)

ggplot(data = nonsown_unite,
       mapping = aes(x = Herb, y = Avg_Rel.Cover, col = fngroup))+
  geom_boxplot()



