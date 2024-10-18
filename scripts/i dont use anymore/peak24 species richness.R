library(tidyverse)
library(vegan)
north <- read_csv("outputs/northmeta.rel.csv")

# plot richness ---------------------------------------------------------------------------------------------
#all species

good_north <- north[, c(1:5)]

northwide <- good_north |> 
  pivot_wider(names_from = Spp,  # Columns to create
              values_from = Avg_Rel.Cover)  # Values to fill in the columns

northwide$richness <- specnumber(northwide) # use vegan package for species richness

ggplot(data = northwide, aes(x = Mix, y = richness))+
  geom_boxplot()


# sown speices
sown <- north |> 
  filter(north$seeded == "Y") # filtering for sown species


good_north_s <- sown[, c(1:5)]

northwide_s <- good_north_s |> 
  pivot_wider(names_from = Spp,  # Columns to create
            values_from = Avg_Rel.Cover)  # Values to fill in the columns

northwide_s$richness <- specnumber(northwide_s) # use vegan package for species richness

ggplot(data = northwide_s, aes(x = Mix, y = richness))+
  geom_boxplot()

northwide_s <- northwide_s[1:30,] # remove controls

hist(northwide_s$richness)
gau <- glmmTMB(richness ~ Mix,
               data = northwide_s,
               family = gaussian(link = "identity"))
plot(simulateResiduals(gau))   # nothing significant
summary(gau)     
emmeans(gau, list(pairwise ~ Mix), adjust = "tukey")
## total richness for each plot, no averages
northnz <- north |> 
  filter(Avg_Rel.Cover >= 0.001)  # filtering out all 0s
spikerich <- northnz |> 
  filter(Mix == "Spike")   # filter only for spike 
spikesp <- unique(spikerich$Spp)  #110

basicrich <- northnz |> 
  filter(Mix == "Basic")   # filter only for spike 
basicsp <- unique(basicrich$Spp)  #108

# lets do richness of sown species
sowntot <- north |> 
  filter(north$seeded == "Y") |> # filtering for sown species
  filter(Avg_Rel.Cover >= 0.001)  # filtering out all 0s



sownspikerich <- sowntot |> 
  filter(Mix == "Spike")   # filter only for spike 
sownspikesp <- unique(sownspikerich$Spp)  #25

sownbasicrich <- sowntot |> 
  filter(Mix == "Basic")   # filter only for spike 
sownbasicsp <- unique(sownbasicrich$Spp)  #23
