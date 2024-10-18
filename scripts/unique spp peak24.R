# Finding unique species for peak 24 ------------------------------------------------------------------------

library(tidyverse)
library(readxl)

?read_excel()
peakfull <- read_excel("raw/PeakFull.xlsx",
                       sheet = "Cover")

unq.sp <- unique(peakfull$Spp)
unq.sp.df <- as.data.frame(unq.sp)


# unique spp in individual chapters --------------------------------------------------------------------------

species1 <- read_excel("raw/peakchapter1.xlsx")
unq.sp1 <- unique(species1$Spp)
unq.sp1.df <- as.data.frame(unq.sp1)


species2 <- read_csv("raw/peakchapter2.csv")
unq.sp2 <- unique(species2$Spp)
unq.sp2.df <- as.data.frame(unq.sp2)
