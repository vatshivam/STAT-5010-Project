library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotrix)

# Extract key variables for analysis
data = read.csv('D:/STAT 5010/project/STAT-5010-project/data.csv')
data = data[,c('farmtype', 'hhsize', 'hhelectric', 'hhhdocc1', 'fplotarea1', 'yearsuse1', 'farmsalev', 's1p1c1area','s1p1c1qharv', 's1p1c1cons', 's1p1c1lives', 's1p1c1lost', 's1p1c1sold', 'pc1', 'extc', 'adm0',  'adm1', 'incfarm','incnfarm')]

# Data sample
head(data)

# Summary Statistics
summary(data)

# Datatype information
str(data)

#determines the number of NAs in each column before cleaning
temp = data.frame(colSums(is.na(data)))
ggplot(data=temp)+
  geom_col(aes(y=colSums(is.na(data)),x=row.names(temp)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust=0.5))+
  ggtitle("Null values in each column")+
  xlab("Columns")+
  ylab("Count")

# Remove NAs associated with the target variable that is harvest loss (s1p1c1lost).
data = data[complete.cases(data[,c("s1p1c1lost")]),] #removes all rows that contain no loss data

temp = data.frame(colSums(is.na(data)))
ggplot(data=temp)+
  geom_col(aes(y=colSums(is.na(data)),x=row.names(temp)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust=0.5))+
  ggtitle("Null values in each column")+
  xlab("Columns")+
  ylab("Count")

# Additionally, we remove any variables that have NAs from categorical which are less than 10% of the total number of  observations (~9500) 

# farmtype: small, medium, large - ordinal
# extc: binary
# pc1: type of crop - categorical
# hhelectric: Has electricity or not: binary
# hhhdocc1: Occupation of interviewee: categorical
# adm0/adm1: Country name

# Delete rows with null values of qualitative variables
data = data[complete.cases(data[,c("farmtype", "hhelectric", "hhhdocc1", "pc1", "extc", "adm0", "adm1")]),] #removes rows with NAs for categorical variables with <10% NAs 

# Imputing null values in quantitative features using mean with respect to the country.
data = data %>% 
  group_by('cleanData2$adm0') %>% 
  mutate_at(vars('hhsize', 'yearsuse1', 'farmsalev','s1p1c1area', 's1p1c1qharv', 's1p1c1cons', 's1p1c1lives', 's1p1c1sold', 'incfarm', 'incnfarm'), ~replace_na(., median(., na.rm = TRUE)))
finalNaCount = colSums(is.na(data)) #recounts the number of NAs in each column
print(finalNaCount)

str(data)
