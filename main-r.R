library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggcorrplot)

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

table(data$hhelectric)

# Removing incorrect values other than 1 and 2
data[data$hhelectric == 0,c('hhelectric')] = 2
data[data$hhelectric == 3,c('hhelectric')] = 2

# Imputing null values in quantitative features using mean with respect to the country.
data = data %>% 
  group_by('cleanData2$adm0') %>% 
  mutate_at(vars('hhsize', 'yearsuse1', 'farmsalev','s1p1c1area', 's1p1c1qharv', 's1p1c1cons', 's1p1c1lives', 's1p1c1sold', 'incfarm', 'incnfarm'), ~replace_na(., median(., na.rm = TRUE)))
finalNaCount = colSums(is.na(data)) #recounts the number of NAs in each column
print(finalNaCount)

str(data)

numeric_features = c('s1p1c1lost','hhsize','fplotarea1','yearsuse1','farmsalev','s1p1c1area','s1p1c1qharv','s1p1c1cons','s1p1c1lives','s1p1c1sold','incfarm','incnfarm')
categorical_features = c('farmtype','hhelectric','hhhdocc1','extc','adm0')

# converting categorical features into factor:
data$farmtype = factor(data$farmtype,labels=c("Small","Medium","Large"))
data$hhelectric = factor(data$hhelectric,labels=c("Yes","No"))
data$hhhdocc1 = as.factor(data$hhhdocc1)
data$pc1 = as.factor(data$pc1)
data$extc = as.factor(data$extc)

# Examining distribution of target variable: harvest lost: s1p1c1lost
# Removing outliers for neater visualizations:
final_data = data %>% filter(s1p1c1lost <= IQR(s1p1c1lost)*1.5)
ggplot(data=final_data)+
  stat_density(aes(x=s1p1c1lost,fill=adm0),position='stack') +
  guides(fill=guide_legend(title="Countries")) +
  coord_cartesian(xlim=c(0,25),ylim=c(0,2))+
  xlab("Harvest Lost (in kg(s))") +
  ylab("Density") +
  ggtitle("KDE of lost harvest") +
  theme(plot.title = element_text(hjust=0.5))

# from the above visualization, we could infer that the target variable
# is not following a normal distribution, which is not an essential condition
# for performing ordinary least squares.

# Correlation matrix:
correlation = round(cor(data[,numeric_features]),1)
ggcorrplot(correlation)

# High correlation pairs
# plot area-quantity harvested
# plot area-quantity sold
# saleValue - plotarea
# saleValue-quantity sold
# saleValue-quantity harvested
# quantityharvested-quantitysold

# Quantity Harvest Distribution
filtered_harv = data %>% filter(s1p1c1qharv <= IQR(s1p1c1qharv)*1.5)

ggplot(data=data, aes(s1p1c1qharv))+
  geom_boxplot() +
  coord_cartesian(xlim=c(0,10000)) +
  ggtitle("Quantity Harvest Distribution")+
  xlab("Quantity in Kilograms")+
  theme(plot.title = element_text(hjust=0.5))

ggplot(data = filtered_harv,aes(x=(s1p1c1qharv-mean(s1p1c1qharv))/sd(s1p1c1qharv))) +
  stat_density(aes(fill=adm0),position="stack") +
  guides(fill=guide_legend(title="Countries")) +
  xlab("Quantity harvested (in kg(s))")+
  ylab("Density")+
  ggtitle("KDE of Quantity harvested")+
  theme(plot.title = element_text(hjust=0.5))

# Quantity Sold Distribution

filtered_sold = data %>% filter(s1p1c1sold <= IQR(s1p1c1sold)*1.5)
ggplot(data = filtered_sold,aes(x=(s1p1c1sold-mean(s1p1c1sold))/sd(s1p1c1sold))) +
  stat_density(aes(fill=adm0),position="stack") +
  guides(fill=guide_legend(title="Countries")) +
  xlab("Quantity harvested (in kg(s))")+
  ylab("Density")+
  ggtitle("KDE of Quantity harvested")+
  theme(plot.title = element_text(hjust=0.5))


# Percentage quantity consumed by household w.r.t occupation
occupations = c("Farmer","Agricultural Labour","Artisan","Office Worker","Civil Servant","Teacher","Health Worker","Trader","Student","Unemployed","Non-labour","Other")
filtered_harv %>%
  group_by(as.numeric(hhhdocc1)) %>%
  mutate("percent_consumed" = (median(s1p1c1cons)*100)/median(s1p1c1qharv)) %>%
  filter(hhhdocc1!=0) %>%
  ggplot(aes(x=as.numeric(hhhdocc1),y=percent_consumed)) +
  geom_col(position="identity",aes(fill=hhhdocc1))+
  xlab("Occupation")+
  ylab("Consumption %")+
  ggtitle("Consumption percentage for different occupations") +
  scale_x_continuous(labels=occupations,breaks=c(1:12)) +
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,face="bold",vjust=0.8),legend.position = "none")

# Harvest lost wrt to occupation

filtered_harv %>%
  group_by(as.numeric(hhhdocc1)) %>%
  filter(hhhdocc1!=0) %>%
  ggplot(aes(x=as.numeric(hhhdocc1),y=s1p1c1lost)) +
  geom_col(position="identity",aes(fill=hhhdocc1))+
  xlab("Occupation")+
  ylab("Quantity Sold")+
  ggtitle("Quantity sold Vs. occupations") +
  scale_x_continuous(labels=occupations,breaks=c(1:12)) +
  coord_cartesian(ylim=c(0,50000)) +
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,face="bold",vjust=0.8),legend.position = "none")

# scatter plot:
# harvest lost vs quantity sold

ggplot(data=final_data,aes(x=s1p1c1sold,y=s1p1c1lost))+
  geom_point(position = "jitter",alpha=0.5,aes(color=farmtype))+
  coord_cartesian(xlim=c(0,500),ylim=c(0,200))+
  xlab("Quantity Sold")+
  ylab("Quantity Lost")+
  ggtitle("Harvest Lost vs Sold with farm types")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Size of farm")

# harvest lost vs quantity harvested
ggplot(data=final_data,aes(x=s1p1c1lost,y=s1p1c1qharv))+
  geom_point(position = "jitter",alpha=0.5,aes(color=hhelectric))+
  coord_cartesian(xlim=c(0,35),ylim=c(0,40000))+
  xlab("Harvest Lost")+
  ylab("Quantity Harvest")+
  ggtitle("Lost vs Harvested")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Has Electricity")

# harvest lost vs income

ggplot(data=final_data,aes(x=s1p1c1lost,y=incfarm))+
  geom_point(position = "jitter",alpha=0.5,aes(color=hhelectric))+
  coord_cartesian(xlim=c(0,50),ylim=c(0,100000))+
  xlab("Harvest Lost")+
  ylab("Income")+
  ggtitle("Income vs Harvest Lost")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Has Electricity")

# harvest lost vs consumed

ggplot(data=final_data,aes(x=s1p1c1lost,y=incfarm))+
  geom_point(position = "jitter",alpha=0.5,aes(color=hhelectric))+
  coord_cartesian(xlim=c(0,30),ylim=c(0,100000))+
  xlab("Harvest Lost")+
  ylab("Income")+
  ggtitle("Income vs Harvest Lost")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Has Electricity")

############################################################

#   2. What sub-regions within countries experience the highest rate of food loss?
#   3. What is the relationship between farm size and post-harvest losses?
#   4. What is the relationship between household income and post-harvest losses?
#   5. What is the relationship between farm productivity and post-harvest losses?
#   6. What is the relationship between household income and farm productivity?
#   7. Do farmers with access to extension services experience less post-harvest losses?
#   8. Which crops are most prone to post-harvest losses?
#   9. What is the relationship between household size and post-harvest losses?
#   10. Do heads of households with primary occupations other than farming experience more or less loss than farmers with farming as their primary occupation

#   1. What countries experience the highest rates of post-harvest loss?
final_data %>%
  group_by(adm0) %>%
  mutate("percent_lost" = (mean(s1p1c1lost+0.01)*100)/mean((s1p1c1qharv)+0.01)) %>%
  ggplot(aes(x=adm0,y=percent_lost)) +
  geom_col(position="identity",aes(fill=adm0))+
  xlab("Country")+
  ylab("Lost %")+
  ggtitle("Lost percentage for different countries") +
  # scale_x_continuous(labels=countries,breaks=c(1:12)) +
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,face="bold",vjust=0.8),legend.position = "none")


