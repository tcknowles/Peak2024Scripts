library(tidyverse)
library(readxl)
plants <- read_excel("raw/peakchapter2.spiketiming.xlsx")
plants <- plants[,1:6]    # keep only the mean cover
# graphs

spike <- plants |> 
  filter(plants$Spp %in% c('ACHMIL','GAIARI','RATCOL','RUDHIR'))
ggplot(data = spike, aes(x = Season, y = Avg_Cover))+
  geom_boxplot()   # shows fall seeding had roughly a 5% cover increase

invasives <- plants |> 
  filter(plants$Spp %in% c('POAPRA','EUPVIR','BROINE'))
ggplot(data = invasives, aes(x = Season, y = Avg_Cover))+
  geom_boxplot()   # shows fall seeding had a small amount of decrease

