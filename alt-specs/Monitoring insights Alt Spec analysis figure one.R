#Alt spec insight looking at usage by year

#Load libraries
library(ggplot2)
library(dplyr)
library(plyr)
library(readr)

setwd("Pathway") #Set the working directory
df <- read.csv('results.csv') #loading data


#Renaming parameters with clear titles
names <- unique(df$Parameter)
namesnew <- c("SO2 concentration","NOx rate","CO2 concentration", "O2 concentration", "stack gas flow")
for (i in 1:length(names)) {
  df$Parameter[df$Parameter == names[i]]<- namesnew[i]
}

df <- df[df$Parameter !="stack gas flow",]
df <- df[df$Parameter !="O2 concentration",]
df <- df[df$Parameter !="CO2 concentration",]

namesnew <- c("SO2 concentration","NOx rate")

#Removing the quarter from the end of the year
df <- df %>%
  mutate(Year.and.Quarter = substr(Year.and.Quarter,1,nchar(Year.and.Quarter)-1))

#Function to find the percent usage of each parameter during a specific year
alt_spec_usage <- function(p,y,dataframe) {
  dataframerps <- filter(dataframe,dataframe$Parameter == p)
  dataframerps <- filter(dataframerps,dataframerps$Year.and.Quarter == y)
  dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
  dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
  ascount<- nrow(dfavg)
  perc_passed_as <- (ascount/nrow(dataframerps))*100 
  
  dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
  dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
  avg_primary <- mean(dfavg$Relative.Accuracy)
  
  output <- list(parameter= p, year = y, percent = perc_passed_as, count = ascount, Avg_Pri = avg_primary)
}

#Seting up to use the function

Year <-c(unique(df$Year.and.Quarter)) #Making a string of years to loop over

Perc_Use <- as.data.frame(alt_spec_usage("NOx rate", 2017, df)) #Calling the function to set up a df to append

#Looping over alt_spec_usage function for each year and parameter
for(j in 1:length(namesnew)){
  for(i in 1:length(Year)) {
    new <- as.data.frame(alt_spec_usage(namesnew[j], Year[i], df))
    Perc_Use[nrow(Perc_Use) + 1, ] <- new                   # Append new row
  }
}

Perc_Use <- distinct(Perc_Use) #deleting duplicate rows (created with first call to function)

#Plot
#Prepare the data
Perc_Use = subset(Perc_Use, select = -c(Avg_Pri))

#Scatter Plot
p<-ggplot(data=Perc_Use, aes(x=year, y=percent, colour=parameter, group=parameter)) + 
  geom_line() + 
  ggtitle('Percent of RATAs by year using alternative specifications') +  
  xlab('Year') + ylab(' Percent of RATAs (%)') +
  scale_y_continuous(breaks=seq(15, 40, by = 5), labels = seq(15,40, by = 5))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+ #Turns x labels sideways
  scale_color_manual(values = c("#6699FF", "#996600", "#003366", "#99CC66"))
p

#Color key
#6699FF:Light Blue
#996600: Brownish
#003366: Dark Blue
#99CC66: Green


