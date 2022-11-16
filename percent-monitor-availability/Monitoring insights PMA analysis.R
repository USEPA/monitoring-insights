## This month in monitoring- PMA
library(ggplot2) #Loading ggplot
library(dplyr) #loading dplyr library
library(scales) #loading in scales
library(Rmisc)

#Load in data
setwd("C:/Users/szintgra/OneDrive - Environmental Protection Agency (EPA)/Documents")
data<-read.csv('PMA Monitoring Data Analysis.csv')
data <- na.omit(data)
data <- data[data$PROGRAM_CODE !="SIPNOX",] #remove rows that have zero or anything



#Yearly PMA Analysis
df <- data %>% 
  select(year = "CALENDAR_YEAR",
         parameter = "PARAMETER_CD",
         PMA = "LAST_RPT_PMA")

df <- df[df$parameter !="NOXC",] #Removing NOX Rate from the analysis
df <- df[df$parameter !="H2O",] #Removing NOX Rate from the analysis
df <- df[df$parameter !="O2C",] #Removing NOX Rate from the analysis

#Renaming parameters with clear titles
names <- unique(df$parameter)
namesnew <- c("CO2 concentration", "stack gas flow","NOx rate", "SO2 concentration" )
for (i in 1:length(names)) {
  df[df== names[i]]<- namesnew[i]
}

p <- ggplot(df,aes(factor(year),PMA)) +  
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = c(97.5,100))+ #Note that limits are hard coded, fix if used again
  ggtitle('Last reported PMA for each calendar year for all emission reporters') +  xlab('Year') + 
  ylab('Last Reported PMA') +
  facet_wrap(~parameter)
p
