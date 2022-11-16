#This code is for an analysis on RA
#The goal is to create a distribution of RA based on parameter

#Load libraries to create plots and wrangle data
library(ggplot2)
library(dplyr)
library(plyr)
library(readr)

#Loading in the data
setwd("INSERT PATHWAY HERE") #Set the working directory
df <- read.csv('resultsRAanalysis.csv') #loading data

#Renaming parameters with clear titles
names <- unique(df$Parameter)
namesnew <- c("SO2 concentration","NOx rate","CO2 concentration", "O2 concentration", "stack gas flow")
for (i in 1:length(names)) {
  df$Parameter[df$Parameter == names[i]]<- namesnew[i]
}

#Separate year and quarter
df <- df %>%
   mutate(Year.and.Quarter = substr(Year.and.Quarter,1,nchar(Year.and.Quarter)-1))

#Remove RATAs that were passed with Alternate Specs
df <- df[df$testresult !="PASSAPS_4QTRS",]
df <- df[df$testresult !="PASSAPS_2QTRS",]


#Create a subset to prepare to graph
dfRA <- df %>% 
   select(Year = "Year.and.Quarter",
          RA = "Relative.Accuracy",
          Parameter = "Parameter")


#Change the order of the facet wrapped plots
neworder <- c("CO2 Concentration", "NOX Concentration", "O2 Concentration", "SO2 Concentration", "Stack gas flow")
df <- arrange(transform(df,Parameter=factor(Parameter,levels=neworder)),Parameter)


#Create box and whisker plot
p<-ggplot(dfRA,aes(factor(Year),RA)) +  
   geom_boxplot(outlier.shape = NA) + 
   scale_y_continuous(limits = c(0,10), minor_breaks = seq(0, 10,5), breaks=seq(0,25,5))+ #Note that limits are hard coded to adjust from outliers
   geom_hline(aes(yintercept=7.5), colour="blue", linetype = 2)+
   geom_hline(aes(yintercept=10), colour="red", linetype = 2)+
   ggtitle('Distribution of relative accuracy by year and parameter') +  xlab('Year') + ylab('Relative Accuracy (%)')+
   facet_wrap(~Parameter) #To break out by specific column

#I did not use the code below for the RA analysis but it makes plots editable in PowerPoint
library(officer)
library(rvg)

plot <- rvg::dml(ggobj = p)

outputplot <- read_pptx() %>%
   add_slide() %>%
   ph_with(plot, ph_location(left = 1.3, top = 0.4, width = 8.5, height = 4.5)) #Full slide size

print(outputplot, "Pathway here") #This pathway needs to end with the file name like /plot.pptx
