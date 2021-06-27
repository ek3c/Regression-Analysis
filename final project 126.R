library(datarium)
library(leaps)
data(marketing)
sales = marketing$sales
youtube = marketing$youtube
facebook = marketing$facebook
newspaper = marketing$newspaper

fit = lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(fit)
fit.final = lm(sales ~ youtube + facebook)
plot(fit.final, resid(fit.final), which = 1, main = "Residuals vs fitted value")
qqnorm(resid(fit.final))
qqline(resid(fit.final))
# since the residual vs fitted plot shows that its non linear hence we will compare

mod.0 = lm(sales~1)
mod.upper = fit
step(mod0, scope = list(lower = mod.0, upper = mod.upper))
#by using the step fucntion we can concluded that the model
#with two predictors which are youtube and facebook give us the 
#least AIC value hence is the best model 
# full model = sales ~ youtube + facebook

mod.1 = regsubsets(cbind(youtube,facebook,newspaper),sales)
summary.mod.1 = summary(mod.1)
summary.mod.1$which
summary.mod.1$adjr2
# by using best subset regression method give us that 
#including facebook given that youtube is already in the model has the 
#largest increase in adjusted R^2 value.

plot(youtube, resid(fit.final), main = "Residuals vs. Youtube",
     xlab = "Youtube", ylab = "Residuals")
abline(0,0)
# by looking at the residual vs youtube plot we can concluded there is non linear problem and unequal variance since it has fanning out pattern.
plot(facebook, resid(fit.final), main = "Residuals vs. Facebook",
     xlab = "Facebook", ylab = "Residuals")
abline(0,0)

mod.e<-lm(log(sales) ~ youtube + facebook)
plot(mod.e,residual,which = 1)
residual<-residuals(mod.e)

qqnorm(residual)
qqline(residual)
# to check if the resdiual of the model has the normal distribution.



mod0 = lm(sales ~ 1)
mod.full = fit
step(mod0, scope = list(lower = mod0, upper = mod.full))
# the step function give us the result that the final model is 

data = data.frame(sales,youtube,facebook,newspaper)
pairs(data,col="purple",main="Plotting Pairs Against Each Other")

install.packages("caTools")
install.packages("ggplot2") 
install.packages("GGally")

plot(marketing, col="purple", main="Plotting Pairs Against Each Other")



#by looking at the scatterplot we can tell how various advertising channel budgets impact the salDes.
#We can clearly see that youtube and facebook sales (1st two plots in the highlited row) increase linearly with increase in the advertising budget. 
#The newspaper sales however show no particular trend.

ggpairs(marketing)
#by looking at the plot we can see the correlation coefficients between the variables and clearly Youtube has the highest correlation which is 0.782  , facebook's correlation number is 0.576 and newpaper's correlation is 0.228


vif(fit)
# by running the vif we can concluded that all the value are lower than 3, which suggesting that there is no multicollinearity in the predictors.


#research question.

anova(mod.0,fit)
# since p value is less than 0.05 hence we reject null hypothesis. 
