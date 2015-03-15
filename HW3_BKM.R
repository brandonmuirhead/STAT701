#STAT701 Homework 3
#Brandon Muirhead

rm(list=ls()) # Remove all the existing variables

#dir=c("/Users/brandonmuirhead/OneDrive/R/STAT701") # school
#setwd(dir)

library(aod)
library(pROC)

data <- read.table("Framingham.dat", sep=",", header=T, as.is=T)
names(data)
names(data)[1] <- "HD"
data$HD=as.factor(data$HD)
data$SEX=as.factor(data$SEX)

data.new=data[1407,] # Entering Lisa's data
data.new[2] <- 50
data.new[4] <- 110
data.new[5] <- 80
data.new[6] <- 180
data.new[7] <- 105
data.new[8] <- 0
data=data[-1407,]  # take out the last row 

sum(is.na(data)) # 13 missing data points
summary(data)

###########################
# Question 1
###########################
#######1. a. 

fit1=glm(HD~SBP, data, family=binomial(logit))
summary(fit1)  # deviance is -2log(lik)

fitall <- glm(HD~., data, family = binomial(logit))  # model with all variables

#chi.sq= 1485.9-1432.8     # get the Chi-square stat
#pchisq(chi.sq, 1, lower.tail=FALSE)  # p-value: from the likelihood Ratio test

#confint.default(fit1)  # usual z-intervals for the coeff's
#confint(fit1)           # through likelihood ratio tests. Both are similar in this case.
#anova(fit1, test="Chisq") # to test if the model is useful

#######1. a. i. Select the next most important variable to add using stepwise forward selection
step(fit1, scope = list(lower = fit1, upper = fitall), direction = "forward")
#stepwise indicates that the next variable to include is gender
fit2 <- glm(HD ~ SBP + SEX, data, family = binomial(logit))
summary(fit2)

#######1. a. ii.
fit2$deviance < fit1$deviance
#fit2 residual deviance is smaller because you add significant variable. Additional variables reduce the variance of the residuals.

#######1. a. iii.
wald <- wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 2:3)
wald$result
#The chi-squared test statistic of 89.5, with two degrees of freedom is associated with a p-value of 0.00000
#indicating that the overall effect of gender is statistically significant.
#alternate calc: 6.609 - 0 / 0.139288 = 47.448. P-value of 
chi.sq <- 1485.9 - 1387.1
pvalue <- pchisq(chi.sq, 2, lower.tail=FALSE)
pvalue
#Likelihood ratio test gives p-value of 3.51e-22, showing significance at level of 0.01. 

#The p-values are not the same, but both approximate the same results. The Wald test only uses one model whereas
#the likelihood ratio uses both models

#######1. a. iv.
fit1.roc <- roc(data$HD, fit1$fitted, plot = T, col = 'blue')
fit2.roc <- roc(data$HD, fit2$fitted, plot = T, col = 'blue')
auc(fit1.roc)     #0.637
auc(fit2.roc)     #0.6825

plot(1-fit2.roc$specificities, fit2.roc$sensitivities, col="red", pch=16,
     xlab=paste("AUC(fit5)=",round(auc(fit2.roc),2),"  AUC(fit1)=",round(auc(fit1.roc),2) ), 
     ylab="Sensitivities")

points(1-fit1.roc$specificities, fit1.roc$sensitivities, col="blue", pch=16)
legend("topleft", legend=c("fit2 w two variables", "fit1 w one variable"),
       lty=c(1,1), lwd=c(2,2), col=c("red", "blue"))
title("Comparison of two models: one with x1, the other with x1-x2")

#curve with more significant variables contains the smaller curve, because fit1 is actually contained within fit2
#the AUC in fit2 is always larger or equal to fit1 because it contains fit1 and adds upon it

#######1. b. 
# Model building
summary(fitall)

fit3 <- update(fitall, .~. -DBP)  # Backward selection by kicking DBP out
summary(fit3)

fit4 <- update(fit3, .~. -FRW)   #remove FRW
summary(fit4)

#although CIG is close, I left it in because it seems like cigarettes should be considered when analyzing heart disease

#######1. b. i.
# my final model
# fit4 <- glm(HD ~ AGE + SEX + SBP + CHOL + CIG, family = binomial(logit), data = data)
chi.sq.2 <- fit4$deviance-fitall$deviance      #check to see if fit4 is better than using fitall
pchisq(chi.sq.2, 2, lower.tail=FALSE)          #p-value of 0.000999 - significant at 0.01

#Final model includes Age, Gender, SBP, Cholestoral, and # of Cigerattes smoked

#######1. b. ii.
# The final model includes Age, Gender, Blood Pressure (SBP), Cholesterol level, and # of Cigerattes smoked. 
# 'Important factors' is defined as variables that are significant at alpha of 0.05

# Liz is someone with the following readings: AGE=50, GENDER=FEMALE, SBP=110, DBP=80, CHOL=180, FRW=105, CIG=0. 

#######1. c. prediction for the subject
fit4.predict <- predict(fit4, data.new, type="response")
fit4.predict    #gives a probability of 4.84% of heart disease based on her characteristics

#######1. d. i.
# Clissifier 1: Y=1 is p(y=1|x) >2/3
fit4.pred <- rep("0", 1406)   # prediction step 1
fit4.pred[fit4$fitted > 2/3] <- "1"  #prediction step 2 to get a classifier. Using 2/3 as a boundary

#[I'm not sure how to do the alternate equation for the Bayes part]

#######1. d. ii.
cm <- table(fit4.pred, data$HD) # confusion matrix
cm

false_pos <- cm[1,2] / sum(cm[1,]) # false positive: 0.2186
false_neg <- cm[2,1] / sum(cm[2,])
overall_miss <- (cm[1,2] + cm[2,1]) / sum(cm[1,], cm[2,])
sensativity <- 1 - false_neg       # 0.5454

#the model has a relatively high Type I error rate (21.9%), meaning it predicted positive for heart disease when it was
#actually negative. The model sensativity is high - nearly half of the actual positives were predicted
#to be nagative.


#######1. d. iii.
error.training <- (cm[1,2]+cm[2,1])/length(fit4.pred) # training error
error.training

#######1. e.
fit4.roc <- roc(data$HD[-c(1090,1138)], fit4$fitted, plot=T, col="blue") #1090 and 1138 were NA, and were causing the length of the response to be different from the predictor variable
summary(fit4.roc)
auc(fit4.roc)     #0.704
names(fit4.roc)

##### False Positive vs. Sensitivity curve is called ROC 
plot(1-fit1.roc$specificities, fit1.roc$sensitivities, col="red", 
     xlab="False Positive", 
     ylab="Sensitivity")

#### Given a False positive rate, locate the prob threshold
plot(1-fit1.roc$specificities, fit1.roc$thresholds, col="green", pch=16)

# EXPLANATION: The ROC curve plots the False Positive rate (1-specificity) against the true positive rate ("Sensitivity") 
# Ideally the curve would rise steeply immediately, then level out.


###########################
# Question 2
###########################
#######2. a.
N <- length(data$HD)
set.seed(1) #ensures random numbers will be generated in the same way for reproducability
index.train <- sample(N, 800) # Take a random sample of n=800 from 1 to N=1406
data.train <- data[index.train,] # Split the 800 randomly chosen subjects as a training data
data.test <- data[-index.train,] # The remaining subjects will be reserved for testing
dim(data.train)
dim(data.test)

fitall.train <- glm(HD~., data=data.train, family=binomial)
summary(fitall.train)

fit2.train <- update(fitall.train, .~. -FRW)  # Backward selection by kicking FRW out
summary(fit2.train)

fit3.train <- update(fit2.train, .~. -DBP)   #remove DBP
summary(fit3.train)

fit4.train <- update(fit3.train, .~. -CHOL)  #remove CHOL
summary(fit4.train)

#My final model includes Age, Gender, SBP and # of Cigs. It does not include cholestoral, as fit4 did in part 1b.

#######2. b.
fit4.fitted.test <- predict(fit4.train, data.test, type="response")
fit4.test.roc <- roc(data.test$HD, fit4.fitted.test, plot=F )
auc(fit4.test.roc)      #AUC = 0.7191

plot(1-fit4.test.roc$specificities, fit4.test.roc$sensitivities, col="red", pch=16,
     xlab=paste("AUC(fit4.test)=",round(auc(fit4.test.roc),3)), 
     ylab="Sensitivities") 

#######2. c.
fit4.train.predict <- predict(fit4.train, data.new, type="response")
fit4.train.predict    #gives a probability of 7.22% of heart disease based on her characteristics
#I trust this one more because it not only uses unseen data as a test, it also has greater AUC than 
#the fit4 model from part #1. The previous model may have been overfit, and using test data allows us
#to use a model on 'unseen' data
