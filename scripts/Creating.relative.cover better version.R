library(tidyverse)


plants <- read_csv("raw/peakchapter2.csv")
plantsnoenv <- plants |>                        # removing bare ground and litter rows
  filter(!Spp %in% c("Bare ground", "Litter"))
# rename columns since R doesn't like numbers for column names
plantsnoenv <- plantsnoenv |> 
  rename(subplot1 = "1",
         subplot2 = "2",
         subplot3 = "3",
         subplot4 = "4",
         subplot5 = "5",
         subplot6 = "6",
         subplot7 = "7",
         subplot8 = "8",
         subplot9 = "9",
         subplot10 = "10",
         subplot11 = "11",
         subplot12 = "12",
         subplot13 = "13",
         subplot14 = "14",
         subplot15 = "15")


plants_long <- plantsnoenv |>                 # pivot longer to get subplot(quadrat) into own column
  pivot_longer(names_to = "Subplot",              
               values_to = "Cover",
               cols = 'subplot1':'subplot15')

data <- data.frame()                              # create blank dataframe

for (plot in unique(plants_long$Plot)) {           # for each plot
  for(subplot in unique(plants_long$Subplot)) {     # for each subplot within each plot
    temp <- plants_long |>                           # create temporary data frame
      filter(Plot == plot, Subplot == subplot)        # filter for only the one plot and subplot selected from forloop statements
    temp$sumcover <- sum(temp$Cover)               #add new column with summed cover of all the plot
    temp$Q1rel <- (temp$Cover/temp$sumcover)*100     # relativize each spp by dividing its cover by the sum cover *100
    temp <- temp[,c(-6,-7)]                      # remove original and sum cover to just leave relative cover
    data <- bind_rows(data,temp)               # iteratively build  into blank data frame for each plot and subplot
  }
}

write_csv(data, "outputs/southrel.csv")