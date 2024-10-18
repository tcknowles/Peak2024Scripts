library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)

library(vegan)

south <- read_csv("outputs/southmeta.rel.csv")

# answers invasive abundance, sown, and nonsown abundance. farther down in forb abundance and richness
# Dbl vs singe spike -----------------------------------------------------------------------------------------


invasives <- south |> 
  filter(south$Spp %in% c('POAPRA','EUPVIR','BROINE')) |> 
  summarize(.by = c(Plot,Spike,Type),
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives


ggplot(data = invasives, aes(x = Spike, y = sumAvg_Rel.Cover))+
  geom_boxplot() 
#doesnt look to be change in invasives via spike mix


sown <- south |> 
  filter(south$Seeded == "Y") |> 
  summarise(.by = c(Plot, Seeded,Type, Spike),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = sown,
       mapping = aes(x = Spike, y = sumAvg_Rel.Cover))+
  geom_boxplot()

means <- sown |> 
  group_by(Spike) |> 
  summarize(Cover = mean(sumAvg_Rel.Cover))

hist(sown$sumAvg_Rel.Cover)  # gaussian?

gaumod <- glmmTMB(sumAvg_Rel.Cover ~ Spike, 
                     data = sown, 
                     family = gaussian(link = "identity"))
plot(simulateResiduals(gaumod))   # nothing significant
testDispersion(gaumod)# test dispersion - all good
testOutliers(gaumod) # test for outliers - all good
testZeroInflation(gaumod) # test for zero inflation - all good

#lets continue with the normal distribution
summary(gaumod)     
emmeans(gaumod, list(pairwise ~ Spike), adjust = "tukey")
#No significance between double and single spike


nonsown <- south |> 
  filter(south$Seeded == "N") |> 
  summarise(.by = c(Plot, Seeded,Type, Spike),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = nonsown,
       mapping = aes(x = Spike, y = sumAvg_Rel.Cover))+
  geom_boxplot()

bobmeans <- nonsown |> 
  group_by(Spike) |> 
  summarize(Cover = mean(sumAvg_Rel.Cover))

hist(nonsown$sumAvg_Rel.Cover)  # gaussian?

nongaumod <- glmmTMB(sumAvg_Rel.Cover ~ Spike, 
                  data = nonsown, 
                  family = gaussian(link = "identity"))
plot(simulateResiduals(nongaumod))   # nothing significant
testDispersion(nongaumod)# test dispersion - all good
testOutliers(nongaumod) # test for outliers - all good
testZeroInflation(nongaumod) # test for zero inflation - all good

#lets continue with the normal distribution
summary(nongaumod)     
emmeans(nongaumod, list(pairwise ~ Spike), adjust = "tukey")
#No significance between double and single spike


# Restoration type -------------------------------------------------------------------------------------------
ggplot(data = invasives, aes(x = Type, y = sumAvg_Rel.Cover))+
  geom_boxplot() 
# no change in invasive via restoration type

ggplot(data = sown,
       mapping = aes(x = Type, y = sumAvg_Rel.Cover))+
  geom_boxplot()
# no change in sown sp abundance per treatment 

ggplot(data = nonsown,
       mapping = aes(x = Type, y = sumAvg_Rel.Cover))+
  geom_boxplot()
# maybe a little change but barely in BSH being lower


# FORBS ------------------------------------------------------------------------------------------------------
forbs <- south |> 
    filter(south$Fngroup == "forb") |> 
  summarise(.by = c(Plot, Seeded,Type, Spike),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = forbs,
       mapping = aes(x = Spike, y = sumAvg_Rel.Cover))+
  geom_boxplot()
# forb abundance does not change with spike seeding rate
forbspike <- forbs |> 
  group_by(Spike) |> 
  summarize(Cover = mean(sumAvg_Rel.Cover))

ggplot(data = forbs,
       mapping = aes(x = Type, y = sumAvg_Rel.Cover))+
  geom_boxplot()
# forb abundance does not change with restoration type
forbtype <- forbs |> 
  group_by(Type) |> 
  summarize(Cover = mean(sumAvg_Rel.Cover))
# BSH might have higher forb abundance

# forb richness
forbrich <- south |> 
  filter(south$Fngroup == "forb")
forbnometa <- forbrich[, c(1:5)]

forbwide <- forbnometa |> 
  pivot_wider(names_from = Spp,  # Columns to create
              values_from = Avg_Rel.Cover)  # Values to fill in the columns

forbwide$richness <- specnumber(forbwide) # use vegan package for species richness

ggplot(data = forbwide, aes(x = Type, y = richness))+
  geom_boxplot()

means <- forbwide |> 
  group_by(Spike) |> 
  summarize(Rich = mean(richness))


