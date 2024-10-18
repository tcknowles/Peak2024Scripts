# seperating weeds based on native and exotic
north <- read_csv("outputs/northmeta.rel.csv")

nonsown <- north |> 
  filter(!Spp %in% c("TCK127", "POAPRA","EUPVIR","BROINE")) |> # remove unknown
  filter(seeded == "N") |> # remove all sown species
  filter(!Plot %in% c(40:42)) |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Sampler,Rep,origin),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros

ggplot(data = nonsown,
       aes (x=Mix, y = sumAvg_Rel.Cover, col = origin))+
  geom_boxplot()
