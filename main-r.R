library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggcorrplot)

############# Data Cleaning ############


# Extract key variables for analysis
data = read.csv('D:/STAT 5010/project/STAT-5010-project/data.csv')
data = data[,c('farmtype', 'hhsize', 'hhelectric', 'hhhdocc1', 'fplotarea1', 'yearsuse1', 'farmsalev', 's1p1c1area','s1p1c1qharv', 's1p1c1cons', 's1p1c1lives', 's1p1c1lost', 's1p1c1sold', 'pc1', 'extc', 'adm0',  'adm1', 'incfarm','incnfarm')]

# Data sample
head(data)

# Summary Statistics
summary(data)

# Datatype information
str(data)

# Unique values in each column
unique_val = sapply(data, function(x) n_distinct(x))
temp = data.frame(unique_val)
ggplot(data=temp)+
  geom_col(aes(y=unique_val,x=row.names(temp),fill=row.names(temp)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust=0.5))+
  ggtitle("Unique values in each column")+
  xlab("Columns")+
  ylab("Count")+
  scale_fill_discrete(name = "Columns")

# Features having more than 500 unique values categorized as quantitative 
numeric_features = c('fplotarea1','farmsalev','s1p1c1qharv','s1p1c1sold','incfarm')
categorical_features = c('farmtype','hhelectric','hhhdocc1','extc','adm1',
                         'adm0','incnfarm','s1p1c1lives','s1p1c1lost',
                         'hhsize','yearsuse1','s1p1c1area','s1p1c1cons','pc1')

# Number of NAs in each column
temp = data.frame(round(colSums(is.na(data))*100/nrow(data),2))
ggplot(data=temp)+
  geom_col(aes(fill=row.names(temp),y=round(colSums(is.na(data))*100/nrow(data),2),x=row.names(temp)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust=0.5))+
  ggtitle("Null values in each column")+
  xlab("Columns")+
  ylab("Percent")+
  scale_fill_discrete(name = "Columns")

# s1p1c1lives: Amount consumed by livestock
# s1p1c1lost: Amount lost

# Dropping qualitative columns with more than 30% null values:
data = subset(data, select = -c(s1p1c1lost,s1p1c1lives,s1p1c1cons))
categorical_features = categorical_features[categorical_features %in% c("s1p1c1lives", "s1p1c1lost","s1p1c1cons") == FALSE]

# Remove NAs from rest of the columns
cols = c(categorical_features,numeric_features)
data = data[complete.cases(data[,cols]),]

# No null values left
colSums(is.na(data))

# farmtype: small, medium, large - ordinal
# extc: binary
# pc1: type of crop - categorical
# hhelectric: Has electricity or not: binary
# hhhdocc1: Occupation of interviewee: categorical
# adm0/adm1: Country name
# incnfarm: % income from non farm activities
# hhsize: household size
# yearsuse1: Number of years for which the land is used

# Removing incorrect values from variables using survey manual
mode<-function(x){which.max(tabulate(x))}
data[data$hhelectric == 0,c('hhelectric')] = mode(data$hhelectric)
data[data$hhhdocc1 == 0,c('hhhdocc1')] = mode(data$hhhdocc1)
data[data$extc == 0,c('extc')] = mode(data$extc)
data[data$pc1 == 0,c('pc1')] = mode(data$pc1)
data[data$incnfarm > 100,c('incnfarm')] = 100

get_bins = function(data,num_partitions){
  # calculate the size of each partition
  partition_size <- ceiling(length(data)/num_partitions)
  # create a sequence of indices to divide the data into partitions
  partition_indices <- seq(1, length(data), by = partition_size)
  # add the last index if needed to ensure all data points are included
  if (partition_indices[length(partition_indices)] != length(data)) {
    partition_indices <- c(partition_indices, length(data))
  }
  data = sort(data)
  names = c()
  for (i in c(1:length(partition_indices)-1)){
    names[i]=paste0(data[partition_indices[i]],'-',data[partition_indices[i+1]])
  }
  return (names)
}

# Binning categorical variables
incnfarm_bins = get_bins(data$incnfarm, n=10)
data$incnfarm = ntile(data$incnfarm, n=10)
data$incnfarm = factor(data$incnfarm,labels=incnfarm_bins)

hhsize_bins = get_bins(data$hhsize, n=5)
data$hhsize = ntile(data$hhsize, n=5)
data$hhsize = factor(data$hhsize,labels=hhsize_bins)

yearsuse1_bins = get_bins(data$yearsuse1, n=5)
data$yearsuse1 = ntile(data$yearsuse1, n=5)
data$yearsuse1 = factor(data$yearsuse1,labels=yearsuse1_bins)

s1p1c1area_bins = get_bins(data$s1p1c1area, n=7)
data$s1p1c1area = ntile(data$s1p1c1area, n=7)
data$s1p1c1area = factor(data$s1p1c1area,labels=s1p1c1area_bins)

# converting categorical features into factor:
occupations = c("Farmer","Agricultural Labour","Artisan","Office Worker","Civil Servant","Teacher","Health Worker","Trader","Student","Unemployed","Non-labour","Other")
data$farmtype = factor(data$farmtype,labels=c("Small","Medium","Large"))
data$hhelectric = factor(data$hhelectric,labels=c("Yes","No"))
data$hhhdocc1 = factor(data$hhhdocc1,labels=occupations)
data$pc1 = as.factor(data$pc1)
data$extc = as.factor(data$extc)

str(data)

write.csv(data,"D:/STAT 5010/project/STAT-5010-project/clean_data.csv")

############# EDA ############

# Target -> incfarm -> income from farming over last 12 months

# Examining distribution of target variable
# Removing outliers for neater visualizations:
temp = data %>% filter(incfarm <= IQR(incfarm)*1.5)
ggplot(data=temp)+
  geom_histogram(aes(x=incfarm),fill='red',color='white') +
  guides(fill=guide_legend(title="Countries")) +
  xlab("Income from farming") +
  ylab("Density") +
  ggtitle("Income Distribution") +
  theme(plot.title = element_text(hjust=0.5))

# from the above visualization, we could infer that the target variable
# is not following a normal distribution and it is skewed to the right.
# Having a normal distribution is not an essential condition
# for performing ordinary least squares.

ggplot(data=temp)+
  geom_boxplot(aes(x=incfarm)) +
  ggtitle("Income Distribution")+
  xlab("Income")+
  theme(plot.title = element_text(hjust=0.5))

# Correlation matrix:
correlation = round(cor(data[,numeric_features]),1)
ggcorrplot(correlation)

# High correlation pairs
# plot area-quantity harvested
# plot area-quantity sold
# saleValue-farm income
# quantityharvested-quantitysold

# s1p1c1qharv
filtered_harv = data %>% filter(s1p1c1qharv <= IQR(s1p1c1qharv)*1.5)
ggplot(data=filtered_harv)+
  stat_density(aes(x=s1p1c1qharv,fill=adm0)) +
  guides(fill=guide_legend(title="Countries")) +
  xlab("Quantity harvested") +
  ylab("Density") +
  ggtitle("KDE of harvest") +
  theme(plot.title = element_text(hjust=0.5))

# farmsalev
temp = data %>% filter(farmsalev <= IQR(farmsalev)*1.5)
ggplot(data = temp,aes(x=farmsalev)) +
  geom_histogram(fill='green',color='white') +
  xlab("Selling Value")+
  ylab("Density")+
  ggtitle("Selling value distribution")+
  theme(plot.title = element_text(hjust=0.5))

# Quantity Sold Distribution
filtered_sold = data %>% filter(s1p1c1sold <= IQR(s1p1c1sold)*1.5)
ggplot(data = filtered_sold,aes(x=(s1p1c1sold-mean(s1p1c1sold))/sd(s1p1c1sold))) +
  stat_density(aes(fill=adm0),position="stack") +
  guides(fill=guide_legend(title="Countries")) +
  xlab("Quantity Sold (in kg(s))")+
  ylab("Density")+
  ggtitle("KDE of Quantity Sold")+
  theme(plot.title = element_text(hjust=0.5))

######### Multivariate Analysis ##############
##############################################

#   1. What countries experience the highest rates of income?
data %>%
  group_by(adm0) %>%
  mutate("income" = median(incfarm)) %>%
  ggplot(aes(x=adm0,y=income)) +
  geom_col(position="identity",aes(fill=adm0))+
  xlab("Country")+
  ylab("Income")+
  ggtitle("Income for different countries") +
  # scale_x_continuous(labels=countries,breaks=c(1:12)) +
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,face="bold",vjust=0.8),legend.position = "none")

# 2. What is the relationship between farm size and income generated?
temp = data %>% filter(incfarm <= IQR(incfarm)*1.5)
ggplot(data=temp,aes(x=incfarm,y=s1p1c1qharv))+
  geom_point(position = "jitter",alpha=0.5,aes(color=farmtype))+
  coord_cartesian(xlim=c(0,750000),ylim=c(0,10000))+
  xlab("Income")+
  ylab("Quantity Harvested")+
  ggtitle("Harvest vs Income")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Size of farm")

# 3. What is the relationship between household income and occupation of family head?

occupations = c("Farmer","Agricultural Labour","Artisan","Office Worker","Civil Servant","Teacher","Health Worker","Trader","Student","Unemployed","Non-labour","Other")
data %>% filter(incfarm <= IQR(incfarm)*1.5) %>%
  group_by(as.numeric(hhhdocc1)) %>%
  mutate("median_inc" = median(incfarm)) %>%
  filter(hhhdocc1!=0) %>%
  ggplot(aes(x=as.numeric(hhhdocc1),y=median_inc)) +
  geom_col(position="identity",aes(fill=hhhdocc1))+
  xlab("Occupation")+
  ylab("Median Income")+
  ggtitle("Income value for different occupations") +
  scale_x_continuous(labels=occupations,breaks=c(1:12)) +
  theme(plot.title = element_text(hjust=0.5),axis.text.x = element_text(angle=30,face="bold",vjust=0.8),legend.position = "none")

# 4. What is the relationship between income and quantity sold with respect to electricity
ggplot(data=temp,aes(x=incfarm,y=s1p1c1sold))+
  geom_point(position = "jitter",alpha=0.5,aes(color=hhelectric))+
  coord_cartesian(xlim=c(0,500000),ylim=c(0,10000))+
  xlab("Income")+
  ylab("Quantity Sold")+
  ggtitle("Harvest vs Income")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Has Electricity")

# 5. What is the relationship between income and farm's market value with respect to electricity and farm size:
ggplot(data=temp,aes(x=incfarm,y=farmsalev))+
  geom_point(position = "jitter",alpha=0.5,aes(color=hhelectric))+
  coord_cartesian(xlim=c(0,500000),ylim=c(0,10^5))+
  facet_grid(.~farmtype)+
  xlab("Income")+
  ylab("Sale value")+
  ggtitle("SaleValue vs Income")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(color="Has electricity")
