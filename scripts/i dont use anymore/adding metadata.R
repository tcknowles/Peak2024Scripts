library(tidyverse)
library(readxl)


# Chapter 1 --------------------------------------------------------------------------------------------------
plants1 <- read_excel("raw/peakchapter1.xlsx")

plants_long <- plants1 |> 
  pivot_longer(names_to = "Subplot",
               values_to = "Cover",
               cols = '1':'10')

# average out the cover of each species within the subplots of one plot
data <- data.frame()

for (species in unique(plants_long$Spp)){            # for every unique species do the following
  for (plot in unique(plants_long$Plot)){           # for every plot do the following
    temp <- plants_long |>                          # create temporary data frame
      filter(Spp == species) |>                     # filter for a single species
      filter(Plot == plot)                          # then filter for a single plot
    # temp is a df with one species, one plot, and many subplots
    mean.cover <- mean(temp$Cover)                # average out the subplots to give one average cover for the species in the plot
    meantemp <- data.frame(Spp = species,         # create a new df to store the species, plot, and avg
                           Plot = plot,
                           Avg_Cover = mean.cover)
    
    print(paste0("The average cover for ",species," in the plot ",plot,"  is ", mean.cover,".")) # print out output
    data <- bind_rows(data, meantemp)             #smash temp data frame with avg cover, plot, and species into a overall df so for loop keeps adding new species plots and avg covers
  }
}
# now I have a data frame (data) with average cover of each species in each plot. need to now: 
### convert NaN to 0s
noNandata <- data                  # creating new df to not mess up data
noNandata[is.na(noNandata)] <- 0   # replace NaN with 0
unique(noNandata$Avg_Cover)        # determine if NaN has been replaced with 0 - cannot find any NaNs
unique(data$Avg_Cover)             # double checking if the data df had Nan - which it did - so this worked

### apply plot type data to the mean cover data
# Add new columns - mix and herbicide
north <- noNandata


# if else statements to add the meta data
Mix <- ifelse(north$Plot %in% c(10, 12, 14, 17, 18, 21, 23, 25, 27, 28, 31, 32, 34, 37, 39),  # if plot number are these
              "Spike",    # add spike in mix vector
              ifelse(north$Plot %in% c(40,41,42), # if plot number are these
                     "Control", # add control in the mix vector
                     "Basic"))  # if not either of these, add basic in mix vector
Mix             
northM <- data.frame(north, Mix)   # add mix vector to the data frame north

Herb <- ifelse(north$Plot %in% c(10, 11, 26,27, 32, 33), 
               "A", 
               ifelse(north$Plot %in% c(12,13, 20, 21, 30, 31),
                      "Q",
                      ifelse(north$Plot %in% c(14, 15, 24,25, 36, 37),
                             "G",
                             ifelse(north$Plot %in% c(16, 17, 22, 23, 38, 39),
                                    "P+I",
                                    ifelse(north$Plot %in% c(18, 19, 28, 29, 34, 35),
                                           "Q+D",
                                           "Control")))))

northMH <- data.frame(northM, Herb)   # add mix vector to the data frame north

### Adding species labels to peak chapter 1
origin <- ifelse(northMH$Spp %in% c("Bare ground", 'Litter',"MOSS",'FECAL'),
                                    "-",
                                    ifelse(northMH$Spp %in% c("TCK127"),  # if spp names are these
                                           "unknown",    # its unknown if the plant is native or introduced
                                           ifelse(northMH$Spp %in% c("CHEALB", 'LEPDEN', 'POROLE'),  # if spp names are these
                                                  "Both",    # its both native and introduced
                                                  ifelse(northMH$Spp %in% c('AGRSTO','BROINE', 'BROJAP', 'CANSAT', 'CIRARV','CONARV', 'ELYREP',
                                          'ERASPE','EUPVIR', 'FALCON','LACSER','MATDIS', 'MEDLUP', 'MELOFF', 
                                          'PERHYD', 'PERMAC','PLAMAJ', 'POAPRA','POLAVI', 'SALTRA',  
                                          'SETVIR','SONARV','TAROFF', 'TRADUB','TRIPRA', 'TRIREP','ULMPUM'), # if spp names are these
                                          "Intro", # add introduced to the vector
                                          "Native"))))  # if not either of these, add native in vector

northMHO <- data.frame(northMH, origin)   # add mix vector to the data frame north


fngroup <- ifelse(northMH$Spp %in% c("Bare ground", 'Litter',"MOSS",'FECAL'),
                  "-",
                  ifelse(northMH$Spp %in% c('ACENEG','ULMPUM', 'FRAPEN','ROSARK', 'SALINT', 'SYMOCC'),  # if spp names are these
                  "woody",    # its woody
                  ifelse(northMH$Spp %in% c('AGRSTO','BROINE','BROJAP','ELYREP','ERASPE','POAPRA', 'SETVIR',
                                            'ANDGER','BOUCUR','CALLON','ELYCAN','ELYTRA','HESCOM','KOEMAC','PANVIR',
                                            'PASSMI','SCHSCO','CAREX','DICLEI','ELECOM','HESSPA','JUNBAL',
                                            'JUNINT','JUNTOR','PANCAP','PASSET','SPOCRY','BOUGRA','NASVIR','SPAPEC',
                                            'CARBRE','CARGRA','CARPRA','CYPACU','CYPSCH'), # if spp names are these
                         "graminoid", # add graminoid to vector
                         "forb")))  # if not either of these, add forb in vector

northMHOF <- data.frame(northMHO, fngroup)   # add mix vector to the data frame north

seeded <- ifelse(northMH$Spp %in% c("Bare ground", 'Litter',"MOSS",'FECAL'),
                 "-",
                 ifelse(northMH$Spp %in% c('ACHMIL','ANDGER','BOUCUR','BOUGRA','CALLON','DALPUR','ELYCAN','ELYTRA','GAIARI',
                                   'HELMAX','HELPAU','HESCOM','KOEMAC','LIATRIS','LINPER','NASVIR','PASSMI','PANVIR',
                                   'PASSMI','PENGRA','RATCOL','RUDHIR','SCHSCO','SYMLAN','VERSTR','ZIZAUR'),# if spp names are these
                 "Y",    # it was seeded
                 "N")) # it was not seeded
northmeta <- data.frame(northMHOF, seeded)

write_csv(northmeta, "outputs/northmeta.csv")


# Chapter 2 --------------------------------------------------------------------------------------------------


plants2 <- read_excel("raw/peakchapter2.xlsx")

# pivoting long in order to get subplot into its own column.

plants2_long <- plants2 |> 
  pivot_longer(names_to = "Subplot",
               values_to = "Cover",
               cols = '1':'10')

# average out the cover of each species within the subplots of one plot
data2 <- data.frame()

for (species in unique(plants2_long$Spp)){            # for every unique species do the following
  for (plot in unique(plants2_long$Plot)){           # for every plot do the following
    temp <- plants2_long |>                          # create temporary data frame
      filter(Spp == species) |>                     # filter for a single species
      filter(Plot == plot)                          # then filter for a single plot
    # temp is a df with one species, one plot, and many subplots
    mean.cover <- mean(temp$Cover)                # average out the subplots to give one average cover for the species in the plot
    meantemp <- data.frame(Spp = species,         # create a new df to store the species, plot, and avg
                           Plot = plot,
                           Avg_Cover = mean.cover)
    
    print(paste0("The average cover for ",species," in the plot ",plot,"  is ", mean.cover,".")) # print out output
    data2 <- bind_rows(data2, meantemp)             #smash temp data frame with avg cover, plot, and species into a overall df so for loop keeps adding new species plots and avg covers
  }
}
# now I have a data frame (data) with average cover of each species in each plot. need to now: 
### convert NaN (which i got from averaging out 0s) to 0s
noNandata2 <- data2                  # creating new df to not mess up data
noNandata2[is.na(noNandata2)] <- 0   # replace NaN with 0
unique(noNandata2$Avg_Cover)        # determine if NaN has been replaced with 0 - cannot find any NaNs
unique(data2$Avg_Cover)             # double checking if the data df had Nan - which it did - so this worked

### apply meta data to the mean cover data
# Add new columns - Spiked level and Restoration Type
south <- noNandata2

# if else statements to add the meta data
Spiked <- ifelse(south$Plot %in% c(52, 53, 55, 58, 60, 63, 64, 67, 71, 72, 75, 78),  # if plot number are these
                 "Spike",    # add spike in mix vector
                 ifelse(south$Plot %in% c(50,66,76), # if plot number are these
                        "Control", # add control in the mix vector
                        "Dbl_spike"))  # if not either of these, add basic in mix vector
Spiked             
southS <- data.frame(south, Spiked)   # add Spiked vector to the data frame north

Type <- ifelse(south$Plot %in% c(51, 52, 60, 61, 77, 78), 
               "BSH", 
               ifelse(south$Plot %in% c(53, 54, 62, 63, 72, 73),
                      "S",
                      ifelse(south$Plot %in% c(55, 56, 64, 65, 74, 75),
                             "BS",
                             ifelse(south$Plot %in% c(57, 58, 67, 68, 70, 71),
                                    "SH",
                                    "Control"))))
Type
southST <- data.frame(southS, Type) 

## Adding species labels now
Origin <- ifelse(southST$Spp %in% c("Bare ground", 'Litter'),
                 "-",
                 ifelse(southST$Spp %in% c("TCK140",'TCK143'),  # if spp names are these
                 "unknown",    # its unknown if the plant is native or introduced
              ifelse(southST$Spp %in% c("CHEALB", 'LEPDEN', 'POROLE'),  # if spp names are these
                  "Both",    # its both native and introduced
                  ifelse(southST$Spp %in% c('AGRGIG','AGRSTO','BROINE','BROJAP','CIRARV','DESSOP','ELYREP','ERASPE',
                                            'EUPVIR','FALCON','LACSER','MEDLUP','MELALB','MELOFF',
                                              'PERHYD','PHAARU','PHLPRA','PLAMAJ','POAPRA','POLAVI', 
                                            'SALTRA','SETVIR','SONARV','TAROFF', 'TRIPRA','TRIREP', 'ULMPUM'), # if spp names are these
                         "Intro", # add intro to vector
                         "Native"))))  # if not either of these, add native in vector

southSTO <- data.frame(southST, Origin)   # add mix vector to the data frame north

Fngroup <- ifelse(southST$Spp %in% c("Bare ground", 'Litter'),
                  "-",
                  ifelse(southST$Spp %in% c('ACENEG', 'FRAPEN','ROSARK', 'SALINT', 'SYMOCC','ULMPUM'),  # if spp names are these
                   "woody",    # its woody
                   ifelse(southST$Spp %in% c('TCK143','AGRGIG','AGRSTO','BROINE','BROJAP','ELYREP',
                                             'ERASPE','PHAARU','PHLPRA','POAPRA','SETVIR','ANDGER',
                                             'CALLON','CARBRE','ELYCAN','ELYTRA','HESCOM','HESSPA',
                                             'MUHRAC','PANVIR','PASSMI','SCHSCO','SORNUT','DICLEI',
                                             'JUNINT','JUNTOR','PANCAP','PASSET','SPOCRY','BOUGRA',
                                             'NASVIR','SPAPEC','AVEHOO','CARGRA','CARPRA','CYPACU',
                                             'CYPSCH','CYPSQU','ERAPEC'), # if spp names are these
                          "graminoid", # add graminoid to vector
                          "forb")))  # if not either of these, add forb in vector

southSTOF <- data.frame(southSTO, Fngroup)   # add mix vector to the data frame north

Seeded <- ifelse(southST$Spp %in% c("Bare ground", 'Litter'),
                 "-",
                 ifelse(southST$Spp %in% c('VERSTR','SPAPEC','ARTLUD','ANDGER','CARBRE','RUDHIR','GAIARI',
                                    'RATCOL','ACHMIL','DESCAN','ROSARK','POTARG','MONFIS','PANVIR',
                                    'HELMAX','ELYTRA','HELPAU','NASVIR','HEURIC','OENBIE','PASSMI',
                                    'ELYCAN','PENGRA','AMOCAN','ECHANG','SORNUT','LINPER','AGAFOE',
                                    'ZIZAPT','GLYLEP','ZIZAUR','HELHEL','SCHSCO','DALPUR','LIATRIS',
                                    'DALCAN','CALLON','MUHRAC','BOUGRA','HESCOM','THADAS','ANECAN',
                                    'HESSPA'),# if spp names are these
                  "Y",    # it was seeded
                  "N")) # it was not seeded
southmeta <- data.frame(southSTOF, Seeded)

write_csv(southmeta, "outputs/southmeta.csv") # saving
