library(tidyverse)
library(DHARMa)
library(lme4)  
library(glmmTMB)
library(emmeans)
library(multcompView)

north <- read_csv("outputs/northmeta.rel.csv")

sown <- north |> 
  filter(north$seeded == "Y") |> 
  summarise(.by = c(Plot, seeded,Mix, Herb,Rep,Sampler),
            sumAvg_Rel.Cover = sum(Avg_Rel.Cover))  # notice i summed here, averaging it would lead to low values due to zeros


# To determine if herbicide effects relative cover of sown species ---------------------------------------

### How does the interaction between herb and mix change sown species cover? -----------------------------------
# lets try and remove control
ncsown <- sown |>   # nc = no control
  filter(sown$Plot %in% c(10:39))   #removing controls
ggplot(data = ncsown, aes(x = Herb, y = sumAvg_Rel.Cover, col = Mix))+
  geom_boxplot()

#LM analysis
hist(ncsown$sumAvg_Rel.Cover)  # assessing the distribution - could be normal tbh? a little right skewed
betamodint <- glmmTMB(I((sumAvg_Rel.Cover/100)) ~ Herb*Mix, # beta needs to be from 0-1, and remove 0s by adding .00001 at the end. how cover changes with herbicide
                      data = ncsown, # using leafy spurge only data set
                      family = beta_family(link = "logit"))  # using beta probability distribution 
plot(simulateResiduals(betamodint))   # no significant dharmas
testDispersion(betamodint)# test dispersion - none significant so all good
testOutliers(betamodint) # test for outliers - non significant
testZeroInflation(betamodint) # test for zero inflation - none significant
summary(betamodint)      # intercept here is the A herbicide
confint(betamodint)     # confience intervals
(means <- emmeans(betamodint, list(pairwise ~ Herb*Mix), adjust = "tukey"))
cld(means)
# Spike + A and G are best

