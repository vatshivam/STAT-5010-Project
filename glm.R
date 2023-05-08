library(ggplot2)
library(dplyr)

data = read.csv('D:/STAT 5010/project/STAT-5010-project/clean_data.csv')

str(data)


one_hot_encoded <- (model.matrix(~ farmtype - 1, data = data))

data = cbind(data,one_hot_encoded)

str(data)
