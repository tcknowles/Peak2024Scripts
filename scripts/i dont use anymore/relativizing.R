library(tidyverse)

# Chapter 2 --------------------------------------------------------------------------------------------------


plants <- read_csv("raw/peakchapter2.csv")
plantsnoenv <- plants |>                        # removing bare ground and litter rows
  filter(!Spp %in% c("Bare ground", "Litter"))
# rename columns
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

# this version is good and only needs one chunk of code, compared to doing it for each quadrat....

plants_long <- plantsnoenv |> 
  pivot_longer(names_to = "Subplot",              
               values_to = "Cover",
               cols = 'subplot1':'subplot15')

data <- data.frame()                              # create blank dataframe

for (plot in unique(plants_long$Plot)) {           # for each plot
  for(subplot in unique(plants_long$Subplot)) {     # for each subplot within each plot
   temp <- plants_long |>
    filter(Plot == plot, Subplot == subplot)        # filter for only the one plot and subplot selected
  temp$sumcover <- sum(temp$Cover)               #add new column with summed cover of all the plot
  temp$Q1rel <- (temp$Cover/temp$sumcover)*100     # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-6,-7)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # iteratively build  into blank data frame for each plot and subplot
  }
}
                              # convert now filled df to the overarching df

# first subplot
plants1ne <- plantsnoenv[,1:5]                       # selecting only the first subplot

data <- data.frame()                              # create blank dataframe

for (plot in unique(plants1ne$Plot)) {
  temp <- plants1ne |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot1)               #add new column with summed cover of all the plot
  temp$rel1 <- (temp$subplot1/temp$sumcover)*100     # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build  into blank data frame for each plot
}
reldata <- data                               # convert now filled df to the overarching df

## rest of subplots
#2
sub2 <- plantsnoenv[,c(1:4,6)]                       # selecting the second subplot

data <- data.frame()

for (plot in unique(sub2$Plot)) {
  temp <- sub2 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot2)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot2/temp$sumcover)*100 
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel2 <- data[,5]  # add only the relativized cover to the overarching data frame
#3
sub3 <- plantsnoenv[,c(1:4,7)]                       # selecting the third subplot

data <- data.frame()

for (plot in unique(sub3$Plot)) {
  temp <- sub3 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot3)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot3/temp$sumcover)*100   # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel3 <- data[,5]  # add only the relativized cover to the overarching data frame
#4
sub4 <- plantsnoenv[,c(1:4,8)]                       # # selecting the fourth subplot

data <- data.frame()

for (plot in unique(sub4$Plot)) {
  temp <- sub4 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot4)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot4/temp$sumcover)*100   # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel4 <- data[,5]  # add only the relativized cover to the overarching data frame
#5
sub5 <- plantsnoenv[,c(1:4,9)]                       # # selecting the fifth subplot

data <- data.frame()

for (plot in unique(sub5$Plot)) {
  temp <- sub5 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot5)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot5/temp$sumcover)*100    # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel5 <- data[,5]  # add only the relativized cover to the overarching data frame
#6
sub6 <- plantsnoenv[,c(1:4,10)]                       # # selecting the sixth subplot

data <- data.frame()

for (plot in unique(sub6$Plot)) {
  temp <- sub6 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot6)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot6/temp$sumcover)*100    # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel6 <- data[,5]  # add only the relativized cover to the overarching data frame
#7
sub7 <- plantsnoenv[,c(1:4,11)]                       # selecting the seventh subplot

data <- data.frame()

for (plot in unique(sub7$Plot)) {
  temp <- sub7 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot7)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot7/temp$sumcover)*100   # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel7 <- data[,5]  # add only the relativized cover to the overarching data frame
#8
sub8 <- plantsnoenv[,c(1:4,12)]                       # selecting the eighth subplot

data <- data.frame()

for (plot in unique(sub8$Plot)) {
  temp <- sub8 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot8)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot8/temp$sumcover)*100    # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel8 <- data[,5]  # add only the relativized cover to the overarching data frame
#9
sub9 <- plantsnoenv[,c(1:4,13)]               # selecting the ninth subplot

data <- data.frame()

for (plot in unique(sub9$Plot)) {
  temp <- sub9 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot9)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot9/temp$sumcover)*100      # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel9 <- data[,5]  # add only the relativized cover to the overarching data frame
#10
sub10 <- plantsnoenv[,c(1:4,14)]                     # selecting the tenth subplot

data <- data.frame()

for (plot in unique(sub10$Plot)) {
  temp <- sub10 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot10)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot10/temp$sumcover)*100     # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel10 <- data[,5]  # add only the relativized cover to the overarching data frame
#11
sub11 <- plantsnoenv[,c(1:4,15)]                       # selecting the eleventh subplot

data <- data.frame()

for (plot in unique(sub11$Plot)) {
  temp <- sub11 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot11)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot11/temp$sumcover)*100      # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel11 <- data[,5]  # add only the relativized cover to the overarching data frame
#12
sub12 <- plantsnoenv[,c(1:4,16)]                       # selecting the twelveth subplot

data <- data.frame()

for (plot in unique(sub12$Plot)) {
  temp <- sub12 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot12)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot12/temp$sumcover)*100      # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel12 <- data[,5]  # add only the relativized cover to the overarching data frame
#13
sub13 <- plantsnoenv[,c(1:4,17)]                       # selecting the thirteenth subplot

data <- data.frame()

for (plot in unique(sub13$Plot)) {
  temp <- sub13 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot13)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot13/temp$sumcover)*100    # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel13 <- data[,5]  # add only the relativized cover to the overarching data frame
#14
sub14 <- plantsnoenv[,c(1:4,18)]                       # selecting the 14th subplot

data <- data.frame()

for (plot in unique(sub14$Plot)) {
  temp <- sub14 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot14)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot14/temp$sumcover)*100      # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel14 <- data[,5]  # add only the relativized cover to the overarching data frame
#15
sub15 <- plantsnoenv[,c(1:4,19)]                       # selecting the 15th subplot

data <- data.frame()

for (plot in unique(sub15$Plot)) {
  temp <- sub15 |>
    filter(Plot == plot) 
  temp$sumcover <- sum(temp$subplot15)               #add new column with summed cover of all the plot
  temp$relcover <- (temp$subplot15/temp$sumcover)*100      # relativize each spp by dividing its cover by the sum cover *100
  temp <- temp[,c(-5,-6)]                      # remove original and sum cover to just leave relative cover
  data <- bind_rows(data,temp)               # alliteratively build for each plot
}
reldata$rel15 <- data[,5]  # add only the relativized cover to the overarching data frame

str(reldata)
write_csv(reldata, "outputs/southrel.csv")
