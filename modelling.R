library(ggcorrplot)
library(car)
library(ggplot2)
library(leaps)
library(MASS)

data<-read.csv("~/Documents/Github/STAT-5010-Project/clean_data.csv")
head(data)



str(data)

ggcorrplot(cor(data[c('fplotarea1','farmsalev',
                      's1p1c1qharv','s1p1c1sold','pc1','extc',
                      'incfarm')]),method='circle')

# We can see from the correlation plot that s1p1c1sold and s1p1c1qharv are correlated with fplotarea1
# and there is a little correlation between incfarm and farmsalev
# As we want to predict farmers income which is in incfarm column our model will take incfarm as
# response variable and other as predictors


lm_model <- lm(incfarm ~ farmtype + hhsize + hhelectric + hhhdocc1 + fplotarea1 + 
                 yearsuse1 + farmsalev + s1p1c1area + s1p1c1qharv + pc1+ s1p1c1sold, data = data)

summary(lm_model)

# From Variance inflation factor we can see that the s1p1c1sold has vif value greater than 5
# which indicates high multicollinearity.

ggplot(data,aes(x=farmsalev,y=incfarm))+
  geom_smooth(method="lm")
# Here the relationship looks linear between incfarm and farmsalev

# Now to check the significance of the predictors we will proceed with performing t-test and ANOVA

anova(lm_model)

reduced_model <-lm(incfarm ~  hhsize + fplotarea1 + 
                 farmsalev +  s1p1c1qharv + pc1+ s1p1c1sold, data = data)

summary(reduced_model)
anova(reduced_model)

vif(reduced_model)
# From vif values we can see that s1p1c1sold is having value greater than 5 which suggests
# multicollinearity
uptd_reduced_model <-lm(incfarm ~  hhsize + fplotarea1 + 
                     farmsalev +  s1p1c1qharv + pc1, data = data)

summary(uptd_reduced_model)
anova(uptd_reduced_model)

vif(uptd_reduced_model)




             

data.red_model = data.frame(yhat = fitted(uptd_reduced_model), r = resid(uptd_reduced_model), y =data$incfarm,
                            hhsize =data$hhsize,fplotarea1 = data$fplotarea1,
                            farmsalev = data$farmsalev, s1p1c1qharv=data$s1p1c1qharv,
                            pc1=data$pc1, s1p1c1sold=data$s1p1c1sold
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


# Using transformation to improve the linear model performance

#hhsize + fplotarea1 + farmsalev +  s1p1c1qharv + pc1
new_data<-data


new_data$sqrtfplotarea1<- sqrt(data$fplotarea1)
new_data$sqrtfarmsalev <- sqrt(data$farmsalev)
new_data$sqrts1p1c1qharv <- sqrt(data$s1p1c1qharv)
new_data$sqrtpc1 <- sqrt(data$pc1)


sqrt_red_model <- lm(incfarm ~ sqrtfplotarea1 + sqrtfarmsalev + sqrts1p1c1qharv + 
                      sqrtpc1 + hhsize, data = new_data)


summary(sqrt_red_model)


# Backward Selection


backward_model <- step(lm_model, direction = "backward", k = log(nrow(data)))

summary(backward_model)

# Forward Selection

forward_model <- regsubsets(incfarm ~ farmtype + hhsize + hhelectric + hhhdocc1 + fplotarea1 + 
                      yearsuse1 + farmsalev + s1p1c1area + s1p1c1qharv + pc1+ s1p1c1sold, 
                    data = data, 
                    method = "forward")


summary(forward_model, scale = "bic")

# Print the coefficients of the model with the lowest BIC
coef(forward_model, id = which.min(summary(forward_model)$bic))
