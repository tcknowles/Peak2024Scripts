library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcomp)

north <- read_csv("outputs/northmeta.rel.csv")

# To determine if herbicide effects relative cover of invasive species ---------------------------------------

invasives <- north |> 
  filter(north$Spp %in% c('POAPRA','EUPVIR','BROINE')) |> 
  summarize(.by = c(Plot,Herb,Mix,Rep,Sampler),    # keep plot, herb. and mix
            sumAvg_Rel.Cover= sum(Avg_Rel.Cover))             # takes average cover of invasives


# Plot -------------------------------------------------------------------------------------------------------
invasives$Herb <- factor(invasives$Herb, levels = c("Control", "G", "P+I", "A", "Q","Q+D"))  # making control go first in plot

# fixing herbicide names
invasives_plot <- invasives  |> 
  mutate(Herb = recode(Herb, 
                       "A" = "Aminopyrachlor", 
                       "G" = "Glyphosate", 
                       "P+I" = "Picloram + Imazapic",
                       "Q" = "Quinclorac",
                       "Q+D" = "Quinclorac + Diflufenzopyr"))


# ggplot it
 (herb_invasive_plot <- ggplot(data = invasives_plot, aes(x = Herb, y = sumAvg_Rel.Cover)))+
geom_boxplot(fill = c("gray","skyblue", "skyblue3", "royalblue", "royalblue3", "darkslateblue"))+  # color it
labs(x = "Herbicide Pretreatment", y = "Avg Invasive Species Abundance (%)",
     title = "Glyphosate best reduces invasive abundance")+
theme_bw()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))  # change title size and location

# here we see the herbicides effect invasives differently

 # save
 ggsave("outputs/figs/herb_invasive.png", herb_invasive_plot,   # name your path, then which object you want to save
        width = 15, height = 10)                           # set width and height
 

# Stats ------------------------------------------------------------------------------------------------------

# linear model with normal distribution
hist(invasives$sumAvg_Rel.Cover) # kinda normal except for lots of 0s
lm_model <- glmmTMB(sumAvg_Rel.Cover ~ Herb+(1|Rep)+(1|Sampler),
               data = invasives,
               family = gaussian(link = "identity")) # lets check out a basic normal model
plot(simulateResiduals(lm_model)) #dharma indicates no sig so could proceed tbh
shapiro.test(invasives$sumAvg_Rel.Cover)   # normal! 
testDispersion(lm_model)# test dispersion - none significant so all good
testOutliers(lm_model) # test for outliers - non significant
testZeroInflation(lm_model) # test for zero inflation - none significant
summary(lm_model) 
(means <- emmeans(lm_model, list(pairwise ~ Herb), adjust = "tukey"))
cld(means)
#inference: sig differences between glyphosate and other herbs, not much else

