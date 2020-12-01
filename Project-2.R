## ---- include= FALSE----------------------------------------------------------
knitr::purl('Project-2.Rmd')


## ---- include=FALSE-----------------------------------------------------------
library(AER)
library(dplyr)
library(ROCR)
library(nnet)
library(leaps)
library(faraway)
library(MASS)
library(glmnet)

data(TeachingRatings)
data <- TeachingRatings


## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
#How many times is the same professor evaluated
prof_rated <- data %>%
  distinct() %>%
  count(prof)

#Count each professor to aggregate to histogram
prof_rated_again <- prof_rated %>%
  distinct() %>%
  count(n)


## ----echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"-----------
#Histogram of how many times professors were rated
bp <- barplot(prof_rated_again$nn, col = 'skyblue3', main="Histogram for the Number of Times a Professor was Evaluated", ylab = "Number of Professors", xlab="Number of Times Evaluated")
axis(1, at = bp, cex.axis=1.2, labels = prof_rated_again$n)


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#boxplot on evals without the transformation

par(mfrow=c(1,5))
boxplot(data$eval~data$gender, ylab= 'Evaluation Score', xlab= 'Gender')
boxplot(data$eval~data$minority, ylab= 'Evaluation Score', xlab= 'Minority')
boxplot(data$eval~data$native, ylab= 'Evaluation Score', xlab= 'Native')
boxplot(data$eval~data$tenure, ylab= 'Evaluation Score', xlab= 'Tenure')
boxplot(data$eval~data$division, ylab= 'Evaluation Score', xlab= 'Division')


## ---- include=FALSE-----------------------------------------------------------
#Turn Division Numeric
data['division'] <- lapply(data['division'], as.numeric)
data['division'] <- data['division'] - 1

#Group Variables by Professor
group.data <- summarize(group_by(tibble::as_tibble(data), prof), evals = mean(eval), beaut = mean(beauty), age = mean(age), gender = first(gender), age = first(age), minority = first(minority), native = first(native), tenure = first(tenure), division = mean(division), students = mean(students), allstudents = mean(allstudents))
group.data <- subset(group.data, select = -c(prof))

attach(group.data)



## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#scatter plot of evaluation score against beauty
plot(data$eval~data$beauty)

#no clear linear pattern


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#now time to re-plot beauty and eval to see if there is a linear relationship
plot(evals~beaut, data = group.data)

#seems to be more of a linear relationship


## ---- echo=FALSE--------------------------------------------------------------
slr.result <- lm(evals~beaut, data = group.data)
anova(slr.result)
#the coefficient is only significant at alpha = .10


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(evals~beaut, data = group.data)
abline(slr.result, col = 'red')


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(slr.result$residuals~slr.result$fitted.values)
abline(h=0, col='red')
#constant variance assumptions seems to be met


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
acf(slr.result$residuals)
#correlation issue at lag = 5


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
qqnorm(slr.result$residuals)
qqline(slr.result$residuals, col="red")
#linearity assumption is not quite met, but is the least important


## ---- include=FALSE-----------------------------------------------------------
#confidence interval for non-transformed data
confint(slr.result, level = 0.95)


## ---- include=FALSE-----------------------------------------------------------
##intercept only model
regnull <- lm(evals~1, data=group.data)
##model with all predictors
regfull <- lm(evals~., data=group.data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#The model created is evals ~division + beaut + gender + tenure + native by both the forward and step-wise


## ---- include=FALSE-----------------------------------------------------------
#########################################################
#model used is the one created by forward and step-wise 
#Multpile linear regression 
#Partial F-tests to verify the model 

#This is the full model that includes all predictors except Prof since that is grouped 
fullresult<- lm(evals ~division + beaut + gender + tenure + native+age+students+allstudents+minority)
summary(fullresult)
anova(fullresult)


## ---- include=FALSE-----------------------------------------------------------
#result1 is the model created by the Forward and step-wise functions 
result1<- lm(evals ~division + beaut + gender + tenure + native)
summary(result1)
anova(result1)


## ---- echo=FALSE--------------------------------------------------------------
##Partial F test for reduced model created by model building steps 
anova(result1, fullresult)


## ---- include=FALSE-----------------------------------------------------------
#Result2 is the model but we in addition drop native from the result1 model 
result2<-lm(evals~division+beaut+gender+tenure)
summary(result2)
anova(result2)


## ---- echo=FALSE--------------------------------------------------------------
##Partial F-test to see if we can drop native
anova(result2,fullresult)

#Based on the partial F-test the final model is evals~division+beaut+gender+tenure


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#########################################################################
######################################################################
###Checking regression assumptions are met for Model 1evals ~division + beaut + gender + tenure + native 
##residual plot
plot(result1$fitted.values,result1$residuals, main="Plot of Residuals against Fitted Values for Model 1")
abline(h=0,col="red")


## ----echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"-----------
##acf plot of residuals
acf(result1$residuals)


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
##qq plot of residuals
qqnorm(result1$residuals)
qqline(result1$residuals, col="red")


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
##########################################################################
######################################################################
###Checking regression assumptions are met for Model 1evals ~division + beaut + gender + tenure
##residual plot
plot(result2$fitted.values,result2$residuals, main="Plot of Residuals against Fitted Values for Model 2")
abline(h=0,col="red")


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
##acf plot of residuals
acf(result2$residuals)


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
##qq plot of residuals
qqnorm(result2$residuals)
qqline(result2$residuals, col="red")


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
######################################################
####################################################33
#Looking for potential outliers
##residuals
#Based on model 1/reduced1 

residuals1<-result1$residuals 

##studentized residuals
student.res<-rstandard(result1) 

##externally studentized residuals
ext.student.res<-rstudent(result1) 

par(mfrow=c(1,3))
plot(result1$fitted.values,residuals1,main="Residuals")
plot(result1$fitted.values,student.res,main="Studentized Residuals")
plot(result1$fitted.values,ext.student.res,main="Externally Studentized Residuals")


## ---- include=FALSE-----------------------------------------------------------
n<-length(evals)
p<-6 #one intercept and 5 predictors 

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")


## ---- include=FALSE-----------------------------------------------------------
ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

##leverages
lev<-lm.influence(result1)$hat 

sort(lev)
2*p/n


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")


## ---- include=FALSE-----------------------------------------------------------

lev[lev>2*p/n]

##influential observations
DFFITS<-dffits(result1)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(result1)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(result1)
COOKS[COOKS>qf(0.5,p,n-p)]


## ---- include=FALSE-----------------------------------------------------------
#using gender as the response variable and age, beauty, and division as predictors
log.result <- glm(gender~age+beaut+division, family='binomial', data= group.data)
summary(log.result)


## ---- include=FALSE-----------------------------------------------------------
#test if all 3 coefficients are zero
1-pchisq(log.result$null.deviance-log.result$deviance, 3)

#fail to reject the null hypothesis, all the coefficients are zero


## ---- include=FALSE-----------------------------------------------------------
#can a subset of the variables predict whether the professor is tenured?
log.result2 <- glm(tenure~evals+age+gender+minority+native+division, family='binomial', data=group.data)
summary(log.result2)


## ---- include=FALSE-----------------------------------------------------------
#test if all 4coefficients are zero
1-pchisq(log.result2$null.deviance-log.result2$deviance, 3)

#reject the null hypothesis, at least one coefficient is not zero


## ---- include=FALSE-----------------------------------------------------------
#using forward model selection to find the best model
regfull <- glm(tenure~evals+age+gender+minority+native+division, family='binomial', data=group.data)
regnull <- glm(tenure~1, family='binomial', data=group.data)

step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

#using backward model selection to find the best model
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")

#using stepwise model selection to find the best model
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")



## ---- include=FALSE-----------------------------------------------------------
#all selection methods arrive at the same conclusion
bestlog.result <- glm(tenure~evals+age, family='binomial', data=group.data)
summary(bestlog.result)


## ---- include=FALSE-----------------------------------------------------------
# since age is not significant, let's check to see if it should be dropped
reduced <- glm(tenure~evals, family='binomial', data=group.data)
1-pchisq(reduced$deviance-bestlog.result$deviance,1)

#fail to reject the null hypothesis, thus we can safely remove age as a predictor


## ----include=FALSE------------------------------------------------------------
finalLog.result <- glm(tenure~evals, family='binomial', data=group.data)
summary(finalLog.result)

#hypothesis test to see if the result is significant
1-pchisq(finalLog.result$null.deviance-finalLog.result$deviance,1)


## ---- include=FALSE-----------------------------------------------------------
#set seed
set.seed(111)

#split the data into train and test
sample<-sample.int(nrow(group.data), floor(.50*nrow(group.data)), replace = F)
train<-group.data[sample, ]
test<-group.data[-sample, ]

#fit the model with the training data
val.result <- glm(tenure~evals, family='binomial', data=train)

##predicted survival rate for testing data based on training data
preds<-predict(val.result,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$tenure)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Titanic")
lines(x = c(0,1), y = c(0,1), col="red")


## ---- include=FALSE-----------------------------------------------------------
##compute the AUC
aucvalues <- performance(rates, measure = "auc")
auc <- aucvalues@y.values[[1]]
auc

#auc is 0.7616959 which is halfway between perfect (1.0) and random (0.5)


## ---- include=FALSE-----------------------------------------------------------
##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$tenure, preds>0.5)

table(test$tenure, preds>0.7)


## ---- include=FALSE-----------------------------------------------------------
#Turn Into Matrix
x<-model.matrix(evals ~ division + beaut + gender + tenure + native + students,group.data)[,-1]
y<-group.data$evals

#Split data into train and test
set.seed(2020)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test<-y[test]

#Fit Ridge Regression
set.seed(4630)
cv.ridge.out<-cv.glmnet(x[train,],y[train],alpha=0)
coefficients(cv.ridge.out)
bestlam.ridge<-cv.ridge.out$lambda.min
bestlam.ridge


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(cv.ridge.out)


## ---- include=FALSE-----------------------------------------------------------
##fit ridge regression using training data
ridge.mod<-glmnet(x[train,],y[train],alpha=0,lambda=bestlam.ridge, thresh = 1e-14)
##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,s=bestlam.ridge,newx=x[test,])
mean((ridge.pred-y.test)^2)


## ---- include=FALSE-----------------------------------------------------------
#Fit Lasso Regression
set.seed(4630)
cv.lasso.out<-cv.glmnet(x[train,],y[train],alpha=1)
bestlam.lasso<-cv.lasso.out$lambda.min
bestlam.lasso


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
plot(cv.lasso.out)


## ---- include=FALSE-----------------------------------------------------------
##fit lasso regression using training data
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda=bestlam.lasso, thresh = 1e-14)
coefficients(lasso.mod)
##Test MSE with best lambda
lasso.pred<-predict(lasso.mod,s=bestlam.lasso,newx=x[test,])
mean((lasso.pred-y.test)^2)

#Fit OLS
OLS<-glmnet(x[train,],y[train],alpha=0,lambda=0, thresh = 1e-14)
OLS.pred<-predict(OLS,newx=x[test,])
mean((OLS.pred-y.test)^2)

##Compare ridge with OLS using best lambda and all observations
cbind(coefficients(ridge.mod), coefficients(OLS))


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#Create Ridge Plots
#Ridge Plot
grid<-10^seq(10,-2,length=100)
out.ridge.all<-glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-14)
plot(out.ridge.all, xvar = "lambda")
abline(v=log(bestlam.ridge), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
#Lasso Ridge Plot
grid<-10^seq(10,-2,length=100)
out.lasso.all<-glmnet(x,y,alpha=1,lambda=grid,thresh = 1e-14)
plot(out.lasso.all, xvar = "lambda")
abline(v=log(bestlam.lasso), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

