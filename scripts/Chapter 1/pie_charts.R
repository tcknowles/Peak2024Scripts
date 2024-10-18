library(tidyverse)


# making a pie chart -----------------------------------------------------------------------------------------

north <- read_csv("outputs/northmeta.rel.csv")
# remove moss and fecal
north <- north |> 
  filter(!Spp %in% c("MOSS", "FECAL"))  # removed moss and fecal
north_fn <- north |> 
  unite(col = group,origin,fngroup,
        sep = " ",
        remove = T)

### for spike seeding
spike <- north_fn |> 
  filter(north$Mix == "Spike") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb, group),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  

# pie chart for spike seeding and fn groups
ggplot(data = spike, aes(x='', y = sumAvg_Rel.Cover, fill = group))+
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void()


##### for both seeding
northsumnc <- north_fn |>
  filter(!Plot %in% c(40:43)) |>   # removed control
  summarise(.by = c(Plot, seeded,Mix, Herb, group),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  
ggplot(data = northsumnc, aes(x='', y = sumAvg_Rel.Cover, fill = group))+
  facet_wrap(facets = vars(Mix))+
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void()
