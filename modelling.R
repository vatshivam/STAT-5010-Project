library(ggcorrplot)
library(car)
library(ggplot2)
library(leaps)
library(MASS)
library(caret)

# data<-read.csv("~/Documents/Github/STAT-5010-Project/clean_data.csv")
# saveRDS and readRDS function saves the datatype information as well, unlike read.csv 
data<-readRDS(file="./clean_data.rds")

head(data)

str(data)

ggcorrplot(cor(data[c('fplotarea1','farmsalev',
                      's1p1c1qharv','s1p1c1sold',
                      'incfarm')]),method='circle')

# We can see from the correlation plot that s1p1c1sold and s1p1c1qharv are correlated with fplotarea1
# and there is a little correlation between incfarm and farmsalev
# As we want to predict farmers income which is in incfarm column our model will take incfarm as
# response variable and other as predictors

# removing adm0 and adm1

lm_full = lm(incfarm ~ .,data=subset(data, select = -c(adm0,adm1)))
summary(lm_full)

# Taking a subset of categorical variable for simplicity:
data = subset(data,select = -c(adm0,adm1,farmtype,hhhdocc1,yearsuse1,pc1,extc))

# New full model:
lm_full = lm(incfarm ~ .,data=data)
summary(lm_full)

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

str(data)

# New full model:
lm_full = lm(incfarm ~ .,data=data)
summary(lm_full)




# From Variance inflation factor we can see that the s1p1c1sold has vif value greater than 5
# which indicates high multicollinearity.

ggplot(data,aes(x=farmsalev,y=incfarm))+
  geom_smooth(method="lm")
# Here the relationship looks linear between incfarm and farmsalev

# Now to check the significance of the predictors we will proceed with performing t-test and ANOVA
str(data)
anova(lm_full)

reduced_model <-lm(incfarm ~  `hhsize4-6`+`hhsize6-7`+`hhsize7-8` +`s1p1c1area20-35.20999908`+
                   `incnfarm0-0.1000000015`+`incnfarm0.1000000015-0.5` +`incnfarm0.5-5`+`incnfarm5-20`
                    + fplotarea1 + farmsalev + `incnfarm35-52`+ `incnfarm52-80`+
                     s1p1c1qharv + s1p1c1sold, data = data)

summary(reduced_model)
anova(reduced_model)

vif(reduced_model)
# From vif values we can see that s1p1c1sold is having value greater than 5 which suggests
# multicollinearity
uptd_reduced_model <-lm(incfarm ~  `hhsize4-6`+`hhsize6-7`+`hhsize7-8` +`s1p1c1area20-35.20999908`+
                     `incnfarm0-0.1000000015`+`incnfarm0.1000000015-0.5` +`incnfarm0.5-5`+`incnfarm5-20`
                   + fplotarea1 + farmsalev + `incnfarm35-52`+ `incnfarm52-80`+
                     s1p1c1qharv , data = data)

summary(uptd_reduced_model)
anova(uptd_reduced_model)

vif(uptd_reduced_model)

final_reduced_model <-lm(incfarm ~  `hhsize4-6`+`hhsize6-7` +
                          `incnfarm0-0.1000000015`+`incnfarm0.1000000015-0.5` +`incnfarm0.5-5`+`incnfarm5-20`
                        + fplotarea1 + farmsalev + `incnfarm35-52`+
                          s1p1c1qharv , data = data)

summary(final_reduced_model)
anova(final_reduced_model)




             

data.red_model = data.frame(yhat = fitted(final_reduced_model), r = resid(final_reduced_model), y =data$incfarm,
                            `hhsize4-6`=data$`hhsize4-6`,`hhsize6-7`=data$`hhsize6-7`,
                            `incnfarm0-0.1000000015` = data$`incnfarm0-0.1000000015`,
                            `incnfarm0.1000000015-0.5` = data$`incnfarm0.1000000015-0.5`, 
                            `incnfarm0.5-5`=data$`incnfarm0.5-5`,`incnfarm5-20`=data$`incnfarm5-20`,
                            s1p1c1qharv=data$s1p1c1qharv
                         )
head(data.red_model)

# We will calculate the RSS, TSS and ESS for our reduced model

rss <- sum(data.red_model$r^2)
tss <- sum((data.red_model$y - mean(data.red_model$y))^2)
ess <- tss - rss



ggplot(data.red_model, aes(x = yhat, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(se=F,col="black")+
  labs(x = "Fitted Values", y = "Observed Values", title = "Fitted vs Observed Plot") 


# Now we will train and test our model to check if it is able to predict income of farmers accurately

set.seed(11111)
n = floor(0.8 * nrow(data)) #find the number corresponding to 80% of the data
index = sample(seq_len(nrow(new_data)), size = n) #randomly sample indicies to be included in the training set

train = data[index, ] #set the training set to be the randomly sampled rows of the dataframe
test = data[-index, ] #set the testing set to be the remaining rows
cat("There are", dim(train)[1], "rows and",dim(train)[2],"columns in the training set. ")  #check the dimensions
cat("There are", dim(test)[1], "rows and",dim(test)[2],"columns in the testing set.")  #check the dimensions



# Backward Selection


backward_model <- step(lm_full, direction = "backward", k = log(nrow(train)))

summary(backward_model)

# Forward Selection

str(data)

forward_model <- regsubsets(incfarm ~ fplotarea1+ farmsalev + s1p1c1qharv + 
                              s1p1c1sold + incfarm + `hhsize1-4`+ `hhsize4-6`
                            +`hhsize6-7`+ `hhsize7-8`+`hhsize8-48`+ 
                              hhelectricYes+ hhelectricNo + `s1p1c1area0-20`+
                              `s1p1c1area20-35.20999908`+ `s1p1c1area35.20999908-50`
                            +`s1p1c1area50-60`+ `s1p1c1area60-80`+ `s1p1c1area80-100`
                            + `s1p1c1area100-100`+`incnfarm0-0`+ `incnfarm0-0.1000000015`
                            + `incnfarm0.5-5`+ `incnfarm20-35`+ `incnfarm5-20`
                            + `incnfarm35-52`+ `incnfarm52-80`+ `incnfarm80-100`, data = train, 
                    method = "forward")


summary(forward_model, scale = "bic")

# Print the coefficients of the model with the lowest BIC
coef(forward_model, id = which.min(summary(forward_model)$bic))

forward_selected_model <- lm(incfarm~ fplotarea1+ farmsalev+ s1p1c1sold + `hhsize6-7`
                  +`incnfarm80-100` + `hhsize8-48`, data=train)

summary(forward_selected_model)

n = dim(data)[1]; 
reg1 = regsubsets(incfarm ~ fplotarea1+ farmsalev + s1p1c1qharv + 
                    s1p1c1sold + incfarm + `hhsize1-4`+ `hhsize4-6`
                  +`hhsize6-7`+ `hhsize7-8`+`hhsize8-48`+ 
                    hhelectricYes+ hhelectricNo + `s1p1c1area0-20`+
                    `s1p1c1area20-35.20999908`+ `s1p1c1area35.20999908-50`
                  +`s1p1c1area50-60`+ `s1p1c1area60-80`+ `s1p1c1area80-100`
                  + `s1p1c1area100-100`+`incnfarm0-0`+ `incnfarm0-0.1000000015`
                  + `incnfarm0.5-5`+ `incnfarm20-35`+ `incnfarm5-20`
                  + `incnfarm35-52`+ `incnfarm52-80`+ `incnfarm80-100`, data = train)
rs = summary(reg1)
rs$which

mspe_array = list()
for(i in c(1:9)){
  models = rs$which[i,-1]
  predictors = names(which(models == TRUE))
  # print(predictors)
  predictors <- lapply(predictors, function(x) gsub("1$", "", x))
  predictors[1] <- "fplotarea1"
  my_vector <- unlist(predictors)
  predictors_transformed <- paste(my_vector, collapse = "+")
  input_column = append("incfarm",my_vector)
  for (column in grep("`", input_column, value = TRUE)){
    input_column[input_column==column]=substring(column,2, nchar(column) - 1)
  }
  print(input_column)
  formula = as.formula(paste0("incfarm", "~", predictors_transformed))
  model = lm(formula=formula,data=train[,input_column])
  mspe_array = append(mspe_array,(mean((test$incfarm - predict(model, newdata = test[,input_column]))^2)))
}

print(mspe_array)

head(train)
# Transformation on the best model

new_data<-data


train$sqrtfplotarea1<- sqrt(train$fplotarea1)
train$sqrtfarmsalev <- sqrt(train$farmsalev)
train$sqrts1p1c1qharv <- sqrt(train$s1p1c1sold)



sqrt_red_model <- lm(incfarm ~ sqrtfplotarea1 + sqrtfarmsalev + s1p1c1sold + 
                       `hhsize6-7` +
                     +`incnfarm80-100`, data = train)


summary(sqrt_red_model)
vif(sqrt_red_model)
anova(sqrt_red_model)

data.final_red_model = data.frame(yhat = fitted(sqrt_red_model), r = resid(sqrt_red_model), y =new_data$incfarm,
                            `hhsize6-7`=new_data$`hhsize6-7`,
                            s1p1c1sold=new_data$s1p1c1sold,
                            sqrtfplotarea1=new_data$sqrtfplotarea1, 
                            sqrtfarmsalev=new_data$sqrtfarmsalev,
                            `incnfarm80-100` = new_data$`incnfarm80-100`
)
head(data.final_red_model)


ggplot(data=as.data.frame(new_data))+
  geom_point(aes(x=fitted(sqrt_red_model),y=resid(sqrt_red_model)))+
  ggtitle("Residuals vs Fitted")+
  xlab("Predicted")+
  ylab("Residuals")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_smooth(aes(x=fitted(sqrt_red_model),y=resid(sqrt_red_model)),se=FALSE)+
  geom_hline(yintercept=0)


ggplot(data=as.data.frame(new_data),aes(sample=resid(sqrt_red_model)))+
  stat_qq() +
  stat_qq_line()+
  ggtitle("QQ plot for Residuals")+
  theme(plot.title = element_text(hjust=0.5))

ggplot(data=as.data.frame(new_data))+
  geom_point(aes(x=incfarm,y=fitted(sqrt_red_model)),alpha=0.5,position="jitter")+
  ggtitle("Fitted vs Observed")+
  xlab("Observed")+
  ylab("Predicted")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_smooth(aes(x=incfarm,y=fitted(sqrt_red_model)),se=FALSE,color='red')+
  geom_abline(slope=1,intercept = 0,color='black',linewidth=1)

n = dim(as.data.frame(new_data))[1]; 
x = head(resid(sqrt_red_model), n-1)
y = tail(resid(sqrt_red_model), n-1)
srp = data.frame(x,y)

ggplot(srp, aes(x = x, y = y)) + 
  geom_point() + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x=x,y=y),se=F) +
  xlab(expression(hat(epsilon)[i])) +
  ylab(expression(hat(epsilon)[i+1])) + 
  ggtitle("Successive Residual Plot") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.7))

# We detected non-constant variance and we will try to make it constant using WLSS

weights <- 1 / (fitted(sqrt_red_model)^2)

sqrt_red_model_wls <- lm(incfarm ~ sqrtfplotarea1 + sqrtfarmsalev + s1p1c1sold + 
                       `hhsize6-7` +
                       +`incnfarm80-100`, data = new_data,weights=weights)




ggplot(data=as.data.frame(new_data))+
  geom_point(aes(x=fitted(sqrt_red_model),y=resid(sqrt_red_model)))+
  ggtitle("Residuals vs Fitted")+
  xlab("Predicted")+
  ylab("Residuals")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_smooth(aes(x=fitted(sqrt_red_model),y=resid(sqrt_red_model)),se=FALSE)+
  geom_hline(yintercept=0)

ggplot(data=as.data.frame(new_data))+
  geom_point(aes(x=fitted(sqrt_red_model_wls),y=resid(sqrt_red_model_wls)))+
  ggtitle("Residuals vs Fitted w WLSS")+
  xlab("Predicted")+
  ylab("Residuals")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_smooth(aes(x=fitted(sqrt_red_model_wls),y=resid(sqrt_red_model_wls)),se=FALSE)+
  geom_hline(yintercept=0)


summary(sqrt_red_model_wls)


ggplot(data=as.data.frame(new_data),aes(sample=resid(sqrt_red_model_wls)))+
  stat_qq() +
  stat_qq_line()+
  ggtitle("QQ plot for Residuals w WLSS")+
  theme(plot.title = element_text(hjust=0.5))

ggplot(data=as.data.frame(new_data))+
  geom_point(aes(x=incfarm,y=fitted(sqrt_red_model_wls)),alpha=0.5,position="jitter")+
  ggtitle("Fitted vs Observed w WLSS")+
  xlab("Observed")+
  ylab("Predicted")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_smooth(aes(x=incfarm,y=fitted(sqrt_red_model_wls)),se=FALSE,color='red')+
  geom_abline(slope=1,intercept = 0,color='black',linewidth=1)

n = dim(as.data.frame(new_data))[1]; 
x = head(resid(sqrt_red_model_wls), n-1)
y = tail(resid(sqrt_red_model_wls), n-1)
srp = data.frame(x,y)

ggplot(srp, aes(x = x, y = y)) + 
  geom_point() + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x=x,y=y),se=F) +
  xlab(expression(hat(epsilon)[i])) +
  ylab(expression(hat(epsilon)[i+1])) + 
  ggtitle("Successive Residual Plot w WLSS") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.7))


sqrt_red_model <- lm(incfarm ~ sqrtfplotarea1 + sqrtfarmsalev + s1p1c1sold + 
                       `hhsize6-7` +
                       +`incnfarm80-100`, data = train)

summary(sqrt_red_model)

weights <- 1 / (fitted(sqrt_red_model)^2)

sqrt_red_model_wls <- lm(incfarm ~ sqrtfplotarea1 + sqrtfarmsalev + s1p1c1sold + 
                           `hhsize6-7` +
                           +`incnfarm80-100`, data = train,weights=weights)

summary(sqrt_red_model_wls)

predictions <- predict(sqrt_red_model_wls, newdata = test)
mspe <- mean(test$incfarm - predictions)^2  
print(mspe)
