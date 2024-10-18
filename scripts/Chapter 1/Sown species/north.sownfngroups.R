

# Saving species that were sown ------------------------------------------------------------------------------


north <- read_csv("outputs/northmeta.rel.csv")
sown<- north |> 
  filter(north$seeded == "Y")
write.csv(sown, "outputs/sownspeciesnorth.csv")


# doing some manipulating on the sown species data -----------------------------------------------------------

library(tidyverse)
sown <- read_csv('outputs/sownspeciesnorth.csv')
sown <- sown[,-1]
#remove control
sown.noC <- sown |> 
  filter(!Plot %in% c(40,41,42))    # ! indicates removing plots 40,41,42 aka controls

#adding fn groups
unique(sown.noC$Spp)
season <- ifelse(sown.noC$Spp %in% c("RUDHIR",'ACHMIL','ELYCAN','ELYTRA','GAIARI','HESCOM','KOEMAC','LINPER','NASVIR',
                                 'PASSMI','PENGRA','ZIZAUR'),
                  "Cool", # cool season species
                  "Warm") #  warm season species
sown1 <- data.frame(sown.noC, season)

sownsum <- sown1 |> 
  summarize(.by = c(Plot,Herb,Mix,season,origin,fngroup),    # groups to sum by
            Sum_Avg_Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives

ggplot(data = sownsum, aes(x = Mix, y = Sum_Avg_Cover, col = fngroup))+
  geom_boxplot()

sownsum.u <- sownsum |> 
  unite(fn,
        fngroup,season,
        sep = "_",
        remove = F)

ggplot(data = sownsum.u, aes(x = Mix, y = Sum_Avg_Cover, col = fn))+
  geom_boxplot()


# most abundant species of seeded species --------------------------------------------------------------------

sownsum.sp <- sown1 |> 
  summarize(.by = c(Spp,Plot, Mix, Herb),    # only sum by species - keep plot to have multiple observations & treatments
            Sum_Avg_Cover= sum(Avg_Rel.Cover))    
ggplot(data = sownsum.sp, aes(x = Spp, y = Sum_Avg_Cover))+
  geom_boxplot()  ### spike was most abundant, then PENGRA and VERSTR
# lets see by treatment
ggplot(data = sownsum.sp, aes(x = Spp, y = Sum_Avg_Cover))+
  geom_boxplot()+
  facet_wrap(facets = vars(Mix))
