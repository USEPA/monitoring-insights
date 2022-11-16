## This month in monitoring- substitute data
library(ggplot2) #Loading ggplot
library(dplyr) #loading dplyr library
library(scales) #loading in scales
library(waffle) #loading waffle chart package
library(RColorBrewer)

#Load in data
setwd("download pathway")
data<-read.csv('MODC_counts_2015_2019.csv')

#Create a subset of the data include MODC, MODC use count, and parameters
df <- data %>% 
  select(modc = "MODC_CD",
         modccount = "MODC_USE_COUNT",
         parameter = "PARAMETER_CD")

## Preparing the data for graphing
#Aggregate the by parameter and MODC
df = setNames(aggregate(df$modccount,
                        by = list(df$parameter, df$modc),
                        FUN = sum), c('parameter','modc','modccount'))

df <- df[df$parameter !="NOXR",] #Removing NOX Rate from the analysis

#Renaming parameters
names <- unique(df$parameter)
namesnew <- c("CO2 Concentration", "Stack gas flow", "NOx Concentration", "O2 Concentration", "SO2 Concentration" )
for (i in 1:length(names)) {
  df[df== names[i]]<- namesnew[i]
}

#Add tier information to Data
Measured <-c(1, 2, 3, 4, 5, 14, 16, 17, 21, 22, 26, 40)
Tierone <- c(6, 7, 11)
Tiertwo <- c(8, 9)
Tierthree <- c(10, 12, 13, 15, 18, 19, 20, 23, 24, 25, 46, 47, 48) 
Allsubs <- c(Tierone, Tiertwo, Tierthree)

#Separate Data by tier
df <- df %>%
  mutate(Tier = ifelse (df$modc %in% Measured,"Measured",
                        ifelse(df$modc %in% Allsubs, "Substituted",
                               "other" )))

#Aggregate by type of data
dfall <- setNames(aggregate(df$modccount,
                            by = list(df$Tier),
                            FUN = sum),c('Tier','modccount'))

#calculating percents
dfall <- dfall %>% 
  mutate(perc = (modccount / sum(modccount))*100)

dfall[,'perc']=round(dfall[,'perc'],1) #rounding data

wafflechart <- function(header,dataframe) {
  dfsub <- dataframe
  vector <- c(1:5,6)*0
  for (r in 1:length(vector))
    if (dfsub$Tier[r] %in% "Measured") {
      vector[1] <- c('Measured' = dfsub$perc[r])
    } else if ( dfsub$Tier[r] %in% "Substituted") {
      vector[2] <- c('Substituted' = dfsub$perc[r])
    }
  
  named <- c(`Measured`= vector[1], `Substituted`= vector[2])
  
  waffle(named*10, rows=10, size=0.6,
         colors=c("#35978F", "#BF812D"),
         title=header,
         xlab="1 square = 0.1%")
}

wafflechart('Measured and substituted data- all parameters',dfall)








##Substitute Data Breakdown by tier of substitute data
#Mutate function to add Tier column
Subdf <- df %>%
  mutate(Tier = ifelse (df$modc %in% Measured,"Measured",
                        ifelse(df$modc %in% Tierone, "1",
                               ifelse(df$modc %in% Tiertwo, "2",
                                      ifelse(df$modc %in% Tierthree, "3",
                                             "other" )))))

#Aggregate by parameter and tier
Subdf <- setNames(aggregate(df$modccount,
                            by = list(Subdf$parameter, Subdf$Tier),
                            FUN = sum),c('parameter','Tier','modccount'))

#calculating percents by parameter
Subdf$parameter <- as.character(Subdf$parameter)
Subdf %<>% 
  group_by(parameter) %>% 
  mutate(perc = (modccount / sum(modccount))*100)

Tierdf <- Subdf #Saving data frame to create table of %measured data by parameter
Subdf <- Subdf[Subdf$Tier != "Measured", ] #Remove Measured

#Change the order of the facet wrapped plots
neworder <- c("CO2 Concentration", "NOx Concentration", "O2 Concentration", "SO2 Concentration", "Stack gas flow")
Subdf <- arrange(transform(Subdf,parameter=factor(parameter,levels=neworder)),parameter)

col<- c("#BF812D","#E6AB02","#35978F","#543005")

#Create Bar Plot
All<- ggplot(Subdf, aes(fill=Tier, y=perc, x= Tier)) +  
  geom_bar(position="stack", stat="identity") + #Position controls type of plots
  ggtitle('Substitute data as a percent of total data')+  
  ylab('Percent of total data (%)')+ 
  xlab('Tier')+ 
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  scale_fill_manual("Tier", values = c("1" = "#BF812D", "2" ="#35978F" , "3" = "#E6AB02", "other" = "#543005"))+
  facet_wrap(~parameter)
All





##Saving other information of interest
#Add tier information to Data

Substituted <- c("1","2","3")
Tierdf$Tier = as.factor(Tierdf$Tier)

#Separate Data by tier
Tierdfone <- Tierdf %>%
  mutate(Tier = ifelse(Tier %in% Substituted, "Substituted",
                       ifelse(Tier %in% "Measured", "Measured", 
                              "Other")))

#Aggregate by type of data
Tierdfone <- setNames(aggregate(Tierdfone$modccount,
                                by = list(Tierdfone$Tier, Tierdfone$parameter),
                                FUN = sum),c('Tier', 'parameter','modccount'))

#calculating percents by parameter
Tierdfone$parameter <- as.character(Tierdfone$parameter)
Tierdfone %<>% 
  group_by(parameter) %>% 
  mutate(perc = (modccount / sum(modccount))*100)

Tierdfone[,'perc']=round(Tierdfone[,'perc'],1) #rounding data

#Finding the number of units

unitdf <- data %>%
  select(oris = "ORIS_CODE",unitid = "UNITID", modc = "MODC_USE_COUNT")

unitdf <- setNames(aggregate(unitdf$modc,
                             by = list(unitdf$oris, unitdf$unit),
                             FUN = sum),c('modc','oris','unitid'))

unitcount <- nrow(unitdf)

#Total operating hours
allhours <- sum(dfall$modccount)

#Tier 1 hours all parameters
drop <- c("parameter","perc")
tierdftwo <- Tierdf[,!(names(Tierdf) %in% drop)]

tierdftwo <- setNames(aggregate(tierdftwo$modccount,
                                by = list(tierdftwo$Tier),
                                FUN = sum),c('tier','hours'))


