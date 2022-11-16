#This code is for an analysis on RA
#The goal is to create a distribution of RA based on parameter

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

#Separate year and quarter
df <- df %>%
   mutate(Year.and.Quarter = substr(Year.and.Quarter,1,nchar(Year.and.Quarter)-1))

#Figure out the percent of test passed with Alt Specs
alt_spec_usage <- function(p,dataframe) {
   dataframerps <- filter(dataframe,dataframe$Parameter == p)
   dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
   dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
   ascount<- nrow(dfavg)
   perc_passed_as <- (ascount/nrow(dataframerps))*100 
   
   dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
   dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
   avg_primary <- mean(dfavg$Relative.Accuracy)
   
   output <- list(percent = perc_passed_as, count = ascount, Avg_Pri = avg_primary)
}

dfpu <- df[df$testresult !="FAILED",]
NOxrasperc <- as.data.frame(alt_spec_usage("NOx rate",dfpu))
Otrasperc <- as.data.frame(alt_spec_usage("O2 concentration",dfpu))
SOrasperc <- as.data.frame(alt_spec_usage("SO2 concentration",dfpu))
COrasperc <- as.data.frame(alt_spec_usage("CO2 concentration",dfpu))
SGrasperc <- as.data.frame(alt_spec_usage("stack gas flow",dfpu))

#All Parameter Value
alt_spec_usage <- function(dataframe) {
   dataframerps <- dataframe
   dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
   dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
   ascount<- nrow(dfavg)
   perc_passed_as <- (ascount/nrow(dataframe))*100 
   
   dfavg <- dataframerps[dataframerps$testresult !="PASS_4QTRS",]
   dfavg <- dfavg[dfavg$testresult !="PASS_2QTRS",]
   avg_primary <- mean(dfavg$Relative.Accuracy)
   
   output <- list(percent = perc_passed_as, count = ascount, Avg_Pri = avg_primary)
}
Allrasperc <- as.data.frame(alt_spec_usage(dfpu))

Parameter <- unique(df$Parameter)

AltSpec_Useage <- rbind(NOxrasperc,Otrasperc,SOrasperc,COrasperc,SGrasperc,Allrasperc)
parameter <- c("NOx rate","O2 concentration", "SO2 concentration", "CO2 concentration", "stack gas flow", "All Parameters")
AltSpec_Useage <- cbind(parameter,AltSpec_Useage)


#Create a subset to prepare to graph of alt specs
#Remove primary Specs
dfplot <- df[df$testresult !="PASS_4QTRS",]
dfplot <- dfplot[dfplot$testresult !="PASS_2QTRS",]

dfMD <- dfplot %>% 
   select(Year = "Year.and.Quarter",
          MD = "Mean.Diff",
          Parameter = "Parameter")

dfplotnox <- filter(dfMD,Parameter == "NOx rate" )
dfplotnox <- dfplotnox[dfplotnox$Year !="2010",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2011",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2012",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2013",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2014",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2015",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2016",]
dfplotnox <- dfplotnox[dfplotnox$Year !="2022",]


#Create box and whisker plot for NOx
p<-ggplot(dfplotnox,aes(factor(Year),MD)) +  
   geom_boxplot() + 
   scale_y_continuous(limits = c(-.025,.025), minor_breaks = seq(-.03, .03,.01), breaks=seq(-.03, .03,.01))+ #Note that limits are hard coded to adjust from outliers
   geom_hline(aes(yintercept=0.015), colour="blue", linetype = 2)+
   geom_hline(aes(yintercept=0.02), colour="red", linetype = 2)+
   geom_hline(aes(yintercept=-0.015), colour="blue", linetype = 2)+
   geom_hline(aes(yintercept=-0.02), colour="red", linetype = 2)+
   ggtitle('Distribution of NOx rate mean difference by year and parameter') +  xlab('Year') + ylab('Mean Difference (lb/mmBtu)')
p

dfplotso2 <- filter(dfMD,Parameter == "SO2 concentration")
dfplotso2 <- dfplotso2[dfplotso2$Year !="2010",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2011",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2012",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2013",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2014",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2015",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2016",]
dfplotso2 <- dfplotso2[dfplotso2$Year !="2022",]
#Create box and whisker plot for SO2

ps<-ggplot(dfplotso2,aes(factor(Year),MD)) +  
   geom_boxplot() + 
   scale_y_continuous(limits = c(-15,15), minor_breaks = seq(-15,15,5), breaks=seq(-15,15,5))+ #Note that limits are hard coded to adjust from outliers
   geom_hline(aes(yintercept=12), colour="blue", linetype = 2)+
   geom_hline(aes(yintercept=15), colour="red", linetype = 2)+
   geom_hline(aes(yintercept=-12), colour="blue", linetype = 2)+
   geom_hline(aes(yintercept=-15), colour="red", linetype = 2)+
   ggtitle('Distribution of SO2 concentration mean difference by year and parameter') +  xlab('Year') + ylab('Mean Difference (ppm)')
ps

#By the numbers:
btn <- function(dfRA, parameter, year) {
     dfRA <- dfRA

     #filter out all other parameters
     f <- unique(dfRA$Parameter)
     f1 <- f[!f %in% parameter]
      
     for(i in 1:length(f1)) {
       dfRA <- dfRA[dfRA$Parameter !=f1[i],]
     }
     
     #filter out all other years
     f <- unique(dfRA$Year)
     f1 <- f[!f %in% year]
     
     for(i in 1:length(f1)) {
        dfRA <- dfRA[dfRA$Year !=f1[i],]
     }
     
     numtests <- (length(dfRA$Relative.Accuracy))
     COUNT <- nrow(dfRA)
     Mid <- median(dfRA$Relative.Accuracy)
     output <- list(count = COUNT, mid = Mid)
     
}

dfbtn <- df[df$testresult !="PASS_4QTRS",]
dfbtn <- dfbtn[dfbtn$testresult !="PASS_2QTRS",]

#When checking these- I run them one at a time
CO2Count <- btn(dfbtn,"CO2 concentration",2017)
CO2Countb <- btn(dfbtn,"CO2 concentration",2021)
NOXCount <- btn(dfbtn,"NOx rate",2017)
NOXCountb <- btn(dfbtn,"NOx rate",2021)
O2Count <- btn(dfbtn,"O2 concentration",2017)
O2Countb <- btn(dfbtn,"O2 concentration",2021)
SO2Count <- btn(dfbtn,"SO2 concentration",2017)
SO2Countb <- btn(dfbtn,"SO2 concentration",2021)
FLOWCount <- btn(dfbtn,"stack gas flow",2017)
FLOWCountb <- btn(dfbtn,"stack gas flow",2021)

