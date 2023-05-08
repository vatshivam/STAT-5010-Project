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

# my_gamma <- Gamma(link = "log")
data$incfarm[data$incfarm == 0] = 0.01

res = train_test_split(data)

# Continuing with the reduced optimal model
glm_mod = glm(data=res$train,incfarm~fplotarea1+sqrt(farmsalev)+s1p1c1sold+`hhsize6-7`+`incnfarm80-100`,family=Gamma(link="log"))
summary(glm_full)
test_pred = predict(glm_mod, newdata = res$test, type = "response")
mspe = mean((res$test$incfarm - test_pred)^2)
cat("The mean squared prediction error (MSPE) for GLM is", round(mspe, 2))
cat("AIC for glm on original data: ",AIC(glm_mod))
# The extremely high MSPE is because of the fact that GLM didn't converge.

#Removing outliers from target:
temp = data %>% filter(incfarm <= 1.5*quantile(incfarm,0.75),incfarm >= 1.5*quantile(incfarm,0.25))
res = train_test_split(temp)

glm_mod_filtered = glm(data=res$train,incfarm~fplotarea1+sqrt(farmsalev)+s1p1c1sold+`hhsize6-7`+`incnfarm80-100`,family=Gamma(link="log"))
summary(glm_mod_filtered)
test_pred = predict(glm_mod_filtered, newdata = res$test, type = "response")
mspe = mean((res$test$incfarm - test_pred)^2)
cat("The mean squared prediction error (MSPE) for GLM on filtered target is", round(mspe, 2))
cat("AIC for glm on filtered target: ",AIC(glm_mod_filtered))

# Getting rid of outliers in the target reduced the MSPE by the multiple of 10^150.

# Let's try to get rid of the outliers present inside the predictor columns as well:

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

res=train_test_split(data_reduced)

glm_mod_clean = glm(data=res$train,incfarm~fplotarea1+sqrt(farmsalev)+s1p1c1sold+`hhsize6-7`+`incnfarm80-100`,family=Gamma(link="log"))
summary(glm_mod_clean)
test_pred = predict(glm_mod_clean, newdata = res$test, type = "response")
mspe = mean((res$test$incfarm - test_pred)^2)
cat("The mean squared prediction error (MSPE) for GLM is", round(mspe, 2))
cat("AIC for glm on filtered target: ",AIC(glm_mod_filtered))

# Weighted GLM on reduced data:

prelim_mod = lm(incfarm~fplotarea1+sqrt(farmsalev)+s1p1c1sold+`hhsize6-7`+`incnfarm80-100`,data=data_reduced)
residuals = resid(prelim_mod)
variances = residuals^2 / prelim_mod$fitted.values
# Calculate the weights
weights <- 1/variances

# Replacing negative weights
weights[weights < 0] = 0

# Using WLSS with glm
glm_mod_wlss = glm(data=data_reduced,weights=weights,incfarm~fplotarea1+sqrt(farmsalev)+s1p1c1sold+`hhsize6-7`+`incnfarm80-100`,family=Gamma(link="log"))
summary(glm_mod_wlss)

# AIC jumped from 








------------------------------------------------------------------------
ggplot(data=data_reduced, aes(x = fplotarea1, y = incfarm)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log")), se = FALSE, color = "brown") +
  labs(title = "Gamma curve", x = "Plot Area", y = "Income from farm")

par(mfrow=c(2,2))
plot(glm_mod)

# From the residuals and fitted plot, we could clearly infer that there is a pattern
# where all the residuals are above the xaxis. In addition, they lie on a single
# line. Hence the GLM model is not able to uncover the pattern hidden behind the data.


