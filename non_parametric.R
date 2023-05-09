library(ggplot2)
library(dplyr)

data<-readRDS(file="D:/STAT 5010/project/STAT-5010-project/clean_data.rds")

str(data)

train_test_split = function(data){
  set.seed(11)
  n = floor(0.8 * nrow(data))
  index = sample(seq_len(nrow(data)), size = n)
  train = data[index, ]
  test = data[-index, ]
  return (list(train=train,test=test))
}

data = subset(data,select = -c(adm0,adm1,farmtype,hhhdocc1,yearsuse1,pc1,extc))

# Encoding values to facilitate partial selection of different categorical features
data = cbind(data,model.matrix(~ hhsize - 1, data = data))
data = cbind(data,model.matrix(~ hhelectric - 1, data = data))
data = cbind(data,model.matrix(~ s1p1c1area - 1, data = data))
data = cbind(data,model.matrix(~ incnfarm - 1, data = data))

#Removing original columns
data = subset(data,select = -c(hhsize,hhelectric,s1p1c1area,incnfarm))

for (column in grep("hhsize|hhelectric|s1p1c1area|incnfarm", names(data), value = TRUE)){
  data[,c(column)] = as.factor(data[,c(column)])
}

filter_iqr <- function(x) {
  if (length(unique(x))==2){
    return (TRUE)
  }
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(x >= lower & x <= upper)
}

reduced_data <- Reduce("&", lapply(data, filter_iqr))
data_reduced <- data[reduced_data, ]

sample = sample_n(data_reduced,size=500)

res=train_test_split(sample)

par(mfrow=c(1,1))

######### Kernel Regression ##########

x = res$train$fplotarea1
y = res$train$incfarm
plot(x, y, main = "Kernel Estimators of Income", xlab = "PlotArea", ylab = "Sales")
fit1 = ksmooth(x, y, kernel = "normal",bandwidth = 1)
lines(fit1, col = "red")
fit2 = ksmooth(x, y, kernel = "normal",bandwidth = 1.5)
lines(fit2, col='blue')
fit3 = ksmooth(x, y, kernel = "normal",bandwidth = 3)
lines(fit3, col='brown')
fit4 = ksmooth(x, y, kernel = "normal",bandwidth = 5)
lines(fit4, col='green')

#From the above plot, we could see that there is no major pattern to be picked
# up by kernel estimators. However, for exploratory analysis, the bandwidth
# value of 1 seems to be the best fit out of other options.

ksmooth_pred = ksmooth(x, y, x.points = res$test$fplotarea1, kernel = "normal",bandwidth = 10)
ksmooth_mspe = mean((res$test$incfarm-ksmooth_pred$y)^2)
cat("MSPE for kernel smoothing: ",ksmooth_mspe)

####### Smoothing Spline #######

plot(y ~ x, pch = 16, col = "grey")
lines(smooth.spline(x, y, spar = 0.5),col='blue')
lines(smooth.spline(x, y, spar = 0.8),col='red')
lines(smooth.spline(x, y, spar = 1),col='green',lwd=2)
lines(smooth.spline(x, y),col='black')

spline = smooth.spline(x,y,spar = 1)
spline_pred = predict(spline,res$test$fplotarea1)$y
spline_mspe = mean((res$test$incfarm-spline_pred)^2)
cat("MSPE for spline smoothing: ",spline_mspe)

# In terms of capturing the non linear pattern, smoothing splines has a similar
# performance as compared to the kernel regression. However, the MSPE is  significantly
# lower in this case.

########## Loess Fit #############

ggplot(data=res$train,aes(x=fplotarea1,y=incfarm))+
  geom_point()+
  geom_smooth(method = 'loess',span=1,se=FALSE)+
  geom_smooth(method = 'loess',span=0.2,color='green',se=FALSE)+
  geom_smooth(method = 'loess',span=0.5,color='black',se=FALSE)+
  geom_smooth(method = 'loess',span=0.75,color='red',se=FALSE)

loess = loess(y~x, span=0.75, data=res$train)
loess_pred = predict(loess,newdata=res$test$fplotarea1)
loess_pred[is.na(loess_pred)] = median(loess_pred[is.na(loess_pred)==FALSE])
loess_mspe = mean((res$test$incfarm-loess_pred)^2)
cat("MSPE for loess: ",loess_mspe)

# The non linear fitting lines are similar to what we obtained in the rest of
# the non parametric methods. The MSPE for loess is less than the MSPE for rest
# of the methods. Hence we could deduce that the loess performs best in comparison.


