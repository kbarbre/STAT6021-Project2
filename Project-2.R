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
#########################
#boxplot on evals without the transformation

par(mfrow=c(1,5))



boxplot(data$eval~data$gender, ylab= 'Evaluation Score', xlab= 'Gender', main= 'Boxplot by Gender')
#Based on the results in the above figure, we can conclude that on average the true mean value of evaluation score is higher for women than more men.

boxplot(data$eval~data$minority, ylab= 'Evaluation Score', xlab= 'Minority', main= 'Boxplot by Minority')
#Based on the box plot about, we can conclude that the evaluation score for a minority is slightly lower, on average.

boxplot(data$eval~data$native, ylab= 'Evaluation Score', xlab= 'Native', main= 'Boxplot by Native')
#Based on the box plot, we can conclude that a professor whose native language is English will have a slightly higher evaluation score. 

boxplot(data$eval~data$tenure, ylab= 'Evaluation Score', xlab= 'Tenure', main= 'Boxplot by Tenure')
#Based on the results, a professor that is not tenured will on average have a slightly higher evaluation score than their counterparts. 

boxplot(data$eval~data$division, ylab= 'Evaluation Score', xlab= 'Division', main= 'Boxplot by Division')
#Based on this scatter plot, the professors teaching lower level courses will have, on average, a slightly higher evaluation score than the professors teachign upper level course. 
#However, it should be noted the difference is minimal. 

#################################################


## ---- include=FALSE-----------------------------------------------------------
#Turn Division Numeric
data['division'] <- lapply(data['division'], as.numeric)
data['division'] <- data['division'] - 1

#Group Variables by Professor
group.data <- summarize(group_by(tibble::as_tibble(data), prof), evals = mean(eval), beaut = mean(beauty), age = mean(age), gender = first(gender), age = first(age), minority = first(minority), native = first(native), tenure = first(tenure), division = mean(division), students = mean(students), allstudents = mean(allstudents))
group.data <- subset(group.data, select = -c(prof))

attach(group.data)



## ---- include=FALSE-----------------------------------------------------------
#scatter plot of evaluation score against beauty
plot(data$eval~data$beauty)

#no clear linear pattern


## ---- include=FALSE-----------------------------------------------------------
#now time to re-plot beauty and eval to see if there is a linear relationship
plot(evals~beaut, data = group.data)

#seems to be more of a linear relationship


## ---- echo=FALSE,fig.height = 4, fig.width = 6, fig.align = "center"----------
slr.result <- lm(evals~beaut, data = group.data)
plot(evals~beaut, data = group.data, xlab = 'Beauty Rating', ylab = 'Evaluation Score', main = 'Evaluation Score Against Beauty with Regression Line')
abline(slr.result, col = 'red')


## ---- echo=FALSE, fig.center = 'center'---------------------------------------
summary(slr.result)
#the coefficient is only significant at alpha = .10


## -----------------------------------------------------------------------------
#######################################################################################################
## Since the model does not fit the data well, the assumptions do not necessarily need to be reported.
#######################################################################################################

plot(slr.result$residuals~slr.result$fitted.values)
abline(h=0, col='red')
#constant variance assumptions seems to be met

acf(slr.result$residuals)
#correlation issue at lag = 5

qqnorm(slr.result$residuals)
qqline(slr.result$residuals, col="red")
#linearity assumption is not quite met, but is the least important

#confidence interval for non-transformed data
confint(slr.result, level = 0.95)


##BoxPlots 
group.data <- summarize(group_by(tibble::as_tibble(TeachingRatings), prof), evals = mean(eval), beaut = mean(beauty), age = mean(age), gender = first(gender), age = first(age), minority = first(minority), native = first(native), tenure = first(tenure), students = mean(students), allstudents = mean(allstudents), division = sum(division == 'upper')/(sum(division == 'upper') + sum(division == 'lower')))
group.data<- subset(group.data, select = -c(prof))

##BoxCox Before transformation 
library(MASS)
boxcox(result.mlr.reduced, lambda = seq(-1,4,0.05))
#because 1 is not in the 95% quarter- this is indication we need to transform the response variable 

##BoxCox After transformation 
library(MASS)

sqr.evals<-group.data.mlr$evals^2
log.evals<-log(evals)

boxcox.result.log <-lm(sqr.evals~beaut)
summary(boxcox.result.log)

sqevals<-sqrt(evals)
boxcox.result.sqrt <-lm(sqr.evals~beaut)
summary(boxcox.result.sqrt)

result.sqr<-lm(sqr.evals ~ beaut + gender + tenure + native, data= group.data)

boxcox.result.sqr <-lm(sqr.evals~beaut)
summary(boxcox.result.sqr)

summary(result.sqr)

boxcox(result.sqr, lambda = seq(0,3,0.01,))

## ---- include=FALSE-----------------------------------------------------------
alt.slr1 <- lm(evals~age)
summary(alt.slr1)
alt.slr2 <- lm(evals~division)
summary(alt.slr2)
alt.slr3 <- lm(evals~students)
summary(alt.slr3)


###################### Model building steps 
##intercept only model
regnull <- lm(evals~1, data=group.data)
##model with all predictors
regfull <- lm(evals~., data=group.data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#The model created is evals ~division + beaut + gender + tenure + native by both the forward and step-wise


#########################################################
#model used is the one created by forward and step-wise 
#Multpile linear regression 
#Partial F-tests to verify the model 

#This is the full model that includes all predictors except Prof since that is grouped 
fullresult<- lm(evals ~division + beaut + gender + tenure + native+age+students+allstudents+minority)
summary(fullresult)
anova(fullresult)

#This is the full model that is the best R^2
fullresult_2<- lm(evals ~division + beaut + gender + tenure + native+students+allstudents+minority)
summary(fullresult_2)
anova(fullresult_2)

plot(fullresult_2$fitted.values,fullresult_2$residuals, main="Plot of Residuals against Fitted Values for Model 1")
abline(h=0,col="red")
acf_1<-acf(fullresult_2$residuals)
print(acf_1)
qqnorm(fullresult_2$residuals)
qqline(fullresult_2$residuals, col="red")


press <- sum((fullresult_2$residuals/(1-(lm.influence(fullresult_2)$hat)))^2)
r2.pred1 <- 1 - (press/(1.3268+0.6342+1.1092+0.7399+0.4658+0.2125+0.7777+0.0265+14.4819))
#Best R^2 model
print(r2.pred1)

#This is the full model that is the best Adjusted R^2
fullresult_3<- lm(evals ~division + beaut + gender + tenure + native+students+allstudents)
summary(fullresult_3)
anova(fullresult_3)

plot(fullresult_3$fitted.values,fullresult_3$residuals, main="Plot of Residuals against Fitted Values for Model 1")
abline(h=0,col="red")
acf_1<-acf(fullresult_3$residuals)
print(acf_1)
qqnorm(fullresult_3$residuals)
qqline(fullresult_3$residuals, col="red")

press <- sum((fullresult_3$residuals/(1-(lm.influence(fullresult_3)$hat)))^2)
r2.pred2 <- 1 - (press/(1.3268+0.6342+1.1092+0.7399+0.4658+0.2125+0.7777+14.5084))
#Model 3
print(r2.pred2)

#result1 is the model created by the Forward and step-wise functions 
result1<- lm(evals ~division + beaut + gender + tenure + native)
summary(result1)
anova(result1)

plot(result1$fitted.values,result1$residuals, main="Plot of Residuals against Fitted Values for Model 1")
abline(h=0,col="red")
acf_1<-acf(result1$residuals)
print(acf_1)
qqnorm(result1$residuals)
qqline(result1$residuals, col="red")

press <- sum((result1$residuals/(1-(lm.influence(result1)$hat)))^2)
r2.pred3 <- 1 - (press/(1.3268+0.6342+1.1092+0.7399+0.4658+15.4986))
#Model 2 
print(r2.pred3)


#result1 is the model created by the backward functions 
result3<- lm(evals ~beaut + gender + native + tenure + students + allstudents + division)
summary(result3)
anova(result3)

##Partial F test for reduced model created by model building steps 
anova(result1, fullresult)

#Result2 is the model but we in addition drop native from the result1 model 
result2<-lm(evals~division+beaut+gender+tenure)
summary(result2)
anova(result2)

##Partial F-test to see if we can drop native
anova(result2,fullresult)

#Based on the partial F-test the final model is evals~division+beaut+gender+tenure


##################################################################
###################################################################
#Looking at the best regression 
##perform all possible regressions (1st order)
allreg <- regsubsets(evals ~., data=group.data, nbest=9)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2),]
best[order(best$adjr2),]
best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]




######################################################
####################################################33
#Looking for potential Leverage points
##residuals
#Based on forward/step-wise model/Model 2 

residuals1<-result1$residuals 

##studentized residuals
student.res<-rstandard(result1) 

##externally studentized residuals
ext.student.res<-rstudent(result1) 

par(mfrow=c(1,3))
plot(result1$fitted.values,residuals1,main="Residuals")
plot(result1$fitted.values,student.res,main="Studentized Residuals")
plot(result1$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

n<-length(evals)
p<-6 #one intercept and 5 predictors 

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

##leverages
lev<-lm.influence(result1)$hat 

sort(lev)
2*p/n

plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

##identify data points on plot
identify(lev)

lev[lev>2*p/n]

##influential observations
DFFITS<-dffits(result1)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

DFBETAS<-dfbetas(result1)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

COOKS<-cooks.distance(result1)
COOKS[COOKS>qf(0.5,p,n-p)]

######################################################
####################################################33
#Looking for potential leverage points 
##residuals
#Based on backward/best adjusted R2 

residuals3<-result3$residuals 

##studentized residuals
student.res3<-rstandard(result3) 

##externally studentized residuals
ext.student.res3<-rstudent(result3) 

par(mfrow=c(1,3))
plot(result3$fitted.values,residuals3,main="Residuals")
plot(result3$fitted.values,student.res3,main="Studentized Residuals")
plot(result3$fitted.values,ext.student.res3,main="Externally  Studentized Residuals")

n2<-length(evals)
p2<-9 #one intercept and 8 predictors 

##critical value using Bonferroni procedure
qt(1-0.05/(2*n2), n2-p2-1)

sort(ext.student.res3)

plot(ext.student.res3,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res3[abs(ext.student.res3)>qt(1-0.05/(2*n2), n2-p2-1)]

##leverages
lev3<-lm.influence(result3)$hat 

sort(lev3)
2*p2/n2

plot(lev3, main="Leverages", ylim=c(0,0.4))
abline(h=2*p2/n2, col="red")

##identify data points on plot
identify(lev3)

lev3[lev3>2*p2/n2]

##influential observations
DFFITS2<-dffits(result3)
DFFITS2[abs(DFFITS2)>2*sqrt(p2/n2)]

DFBETAS2<-dfbetas(result3)
DFBETAS2[abs(DFBETAS2)>2/sqrt(n2)]

COOKS2<-cooks.distance(result3)
COOKS2[COOKS2>qf(0.5,p2,n2-p2)]

#### VIF ####
backwards.model<-lm(evals ~ beaut + gender + native + tenure + students + allstudents, data=group.data)
vif(backwards.model)

###########################################################################
#Check For Interactions for Gender
#Subset data into male and female
male.data<-subset(group.data,gender=="male") 
female.data<-subset(group.data,gender=="female") 

#Build regressions for each subset of data
male.reg<-lm(evals~beaut,data=male.data)
female.reg<-lm(evals~beaut,data=female.data)

#Plot the regressions
plot(group.data$beaut,group.data$evals, main="Evaluation against Beauty Rating, by Gender", ylab = "Teacher Evaluation", xlab = "Beauty Score")
points(male.data$beaut,male.data$evals, pch=2, col="blue") 
points(female.data$beaut,female.data$evals, pch=12, col="red")
abline(male.reg,lty=1, col="blue")
abline(female.reg,lty=2, col="red") 
legend("topleft", c("Male","Female"), lty=c(1,2), pch=c(2,12), col=c("blue","red")) 

##fit regression with interaction between the 2 predictors
result.gender<-lm(evals~beaut*gender, data=group.data)
summary(result)

##fit regression with no interaction and test if we need interaction
gender.reduced<-lm(evals~beaut+gender, data=group.data)
anova(gender.reduced,result.gender) #Large p-value, means we don't need interaction

#####################################################################
#Check For Interactions for tenure
#Subset data into tenured and not tenured
tenure1<-subset(group.data,tenure=="yes") 
tenure2<-subset(group.data,tenure=="no") 

#Build regressions for each subset of data
tenure.reg1<-lm(evals~beaut,data=tenure1)
tenure.reg2<-lm(evals~beaut,data=tenure2)

#Plot the regressions
plot(group.data$beaut,group.data$evals, main="Evaluation against Beauty Rating, by Tenure", ylab = "Teacher Evaluation", xlab = "Beauty Score")
points(tenure1$beaut,tenure1$evals, pch=2, col="blue") 
points(tenure2$beaut,tenure2$evals, pch=12, col="red")
abline(tenure.reg1,lty=1, col="blue")
abline(tenure.reg2,lty=2, col="red") 
legend("topleft", c("Yes","No"), lty=c(1,2), pch=c(1,12), col=c("blue","red")) 

##fit regression with interaction between the 2 predictors
result.tenure<-lm(evals~beaut*tenure, data=group.data)
summary(result.tenure)

##fit regression with no interaction and test if we need interaction
reduced.tenure<-lm(evals~beaut+tenure, data=group.data)
anova(reduced.tenure,result.tenure) #Large p-value, means we don't need interaction

######################################################################
#Check for Interactions for Minority
#Subset data into minority and not minority
minority1<-subset(group.data,minority=="yes") 
minority2<-subset(group.data,minority=="no") 

#Build regressions for each subset of data
minority.reg1<-lm(evals~beaut,data=minority1)
minority.reg2<-lm(evals~beaut,data=minority2)

#Plot the regressions
plot(group.data$beaut,group.data$evals, main="Evaluation against Beauty Rating, by Minority", ylab = "Teacher Evaluation", xlab = "Beauty Score")
points(minority1$beaut,minority1$evals, pch=2, col="blue") 
points(minority2$beaut,minority2$evals, pch=12, col="red")
abline(minority.reg1,lty=1, col="blue")
abline(minority.reg2,lty=2, col="red") 
legend("topleft", c("Yes","No"), lty=c(1,2), pch=c(1,12), col=c("blue","red")) 

##fit regression with interaction between the 2 predictors
result.minority<-lm(evals~beaut*minority, data=group.data)
summary(result.minority)

##fit regression with no interaction and test if we need interaction
reduced.minority<-lm(evals~beaut+minority, data=group.data)
anova(reduced.minority,result.minority) #Large p-value, means we don't need interaction

#####################################################################
#Check for Interactions for Native
#Subset data into native and non native
native1<-subset(group.data,native=="yes") 
native2<-subset(group.data,native=="no") 

#Build regressions for each subset of data
native.reg1<-lm(evals~beaut,data=native1)
native.reg2<-lm(evals~beaut,data=native2)

#Plot the regressions
plot(group.data$beaut,group.data$evals, main="Evaluation against Beauty Rating, by Native", ylab = "Teacher Evaluation", xlab = "Beauty Score")
points(native1$beaut,native1$evals, pch=2, col="blue") 
points(native2$beaut,native2$evals, pch=12, col="red")
abline(native.reg1,lty=1, col="blue")
abline(native.reg2,lty=2, col="red") 
legend("topleft", c("Yes","No"), lty=c(1,2), pch=c(1,12), col=c("blue","red")) 

##fit regression with interaction between the 2 predictors
result.native<-lm(evals~beaut*native, data=group.data)
summary(result.native)

##fit regression with no interaction and test if we need interaction
reduced.native<-lm(evals~beaut+native, data=group.data)
anova(reduced.native,result.native) #Large p-value, means we don't need interaction

###############################################################

## ---- echo=FALSE--------------------------------------------------------------
#using gender as the response variable and age, beauty, and division as predictors
log.result <- glm(gender~age+beaut+division, family='binomial', data= group.data)
summary(log.result)


## -----------------------------------------------------------------------------
1-pchisq(log.result$null.deviance-log.result$deviance, 3)



## ---- echo=FALSE--------------------------------------------------------------
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



## ---- echo=FALSE--------------------------------------------------------------
#all selection methods arrive at the same conclusion
bestlog.result <- glm(tenure~evals+age, family='binomial', data=group.data)
summary(bestlog.result)


## -----------------------------------------------------------------------------
reduced <- glm(tenure~evals, family='binomial', data=group.data)
1-pchisq(reduced$deviance-bestlog.result$deviance,1)


## ----echo=FALSE---------------------------------------------------------------
finalLog.result <- glm(tenure~evals, family='binomial', data=group.data)
summary(finalLog.result)


## -----------------------------------------------------------------------------
#hypothesis test to see if the result is significant
1-pchisq(finalLog.result$null.deviance-finalLog.result$deviance,1)


## -----------------------------------------------------------------------------
exp(-1.4510)


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
plot(roc_result, main="ROC Curve for Tenure")
lines(x = c(0,1), y = c(0,1), col="red")


## ---- include=FALSE-----------------------------------------------------------
##compute the AUC
aucvalues <- performance(rates, measure = "auc")
auc <- aucvalues@y.values[[1]]
auc

#auc is 0.7616959 which is halfway between perfect (1.0) and random (0.5)


## ---- echo=FALSE--------------------------------------------------------------
table(test$tenure, preds>0.87)
table(test$tenure, preds>0.88)


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

