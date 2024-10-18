






# fn groups --------------------------------------------------------------------------------------------------

north_fn <- north |> 
  unite(col = group,origin,fngroup,
        sep = " ",
        remove = T)

## what functional groups are establishing?
sown <- north_fn |> 
  filter(north$seeded == "Y") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb, group),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros




# fn group establishment changes with herbicide
ggplot(data = sown,
       mapping = aes(x = Mix, y = sumAvg_Rel.Cover, fill = group))+
  geom_col(position = "stack")


# just by the look of things, all forb establishment is similar but G has most gram establishment
# fn group via seed mix
ggplot(data = ncsown,
       mapping = aes(x = Mix, y = sumAvg_Rel.Cover, col = fngroup))+
  geom_boxplot() # just forbs are higher in spike - expected
# fn group via both herb and seed mix
ggplot(data = ncsown,
       mapping = aes(x = Herb, y = sumAvg_Rel.Cover, col = fngroup))+
  geom_boxplot()+
  facet_wrap(facets = vars(Mix))
  #glyphosate again has highest graminoid cover in both basic and spike. Q+D had least in both