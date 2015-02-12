
### Regression
###  Read Chapter 3.2-3.4!!!!

Topics:
1) Simple regression
Model specification
LS estimates and properties
R-squared and RSE
Confidence intervals for coef.
Prediction intervals
Caution about reverse regression

2) Multiple regression
Model: Linearity, Equal Variances and Normality
Interpretation of the coefficients (Causality vs. Associations)
LS estimates, their properties
t-intervals and F tests
Misconception of what p-values can say about each predictor
R-squared
Confidence and Prediction intervals

3) More about Multiple regressions
Categorical predictors (features)
Transformation of predictors and responses
Outliers, Leverage points

4) Which model to use
Model selection
Post Model Inference (POSI)
"Everything is done by minimizing IN SAMPLE ERROS"


5) libarary
library(leaps) # regsubsets(): model selection
library(boot)  # bootstrap method


### Set working directory

rm(list=ls()) # Remove all the existing variables

dir=c("/Users/brandonmuirhead/OneDrive/R/STAT701") # school

setwd(dir)


### Multiple Regression
  Topics:
  
  2) Multiple regression
      Model: Linearity, Equal Variances and Normality
      Interpretation of the coefficients (Causality vs. Associations)
      LS estimates, their properties
      t-intervals and F tests
        Misconception of what p-values can say about each predictor
      R-squared
      Confidence and Prediction intervals

  3) More about Multiple regressions
      Categorical predictors (features)
      Transformation of predictors and response
      Outliers, Leverage points

  4) Which model to use
      Model selection
      Post Model Inference (POSI)
      "Everthing is done by minizing IN SAMPLE or TRAINING ERROS"
      

### Cars 04: New design: Horsepower=225, Weight=4, Seating=5, Length=180, Width=80,
### Transmission=automatic
### Question of the interests: Response is Y=MPG_City
###     1) Effects of each feature on Y
###     2) Estimate the mean MPG for all such cars
###     3) Predict Y for this particular car


### Data: Cars_04, we will exclude some fancy cars

data = read.csv("Cars_04.csv", header=T, as.is=F)
str(data)   # There are 241 cars in the data

na.fail(data)  # errors if there are some missing values
data.comp=na.omit(data)  # It deletes any car with a missing value - not a good idea.
str(data.comp)  #182 cars with complete records for all the variables. 


### Append the new design at the bottom of the data so it can be used for prediction

newcar = c(NA,NA,NA,225,4,5,180,NA, NA, NA, NA, 80, rep(NA,5))
data= rbind(data,newcar)   # we have one more row at the end now
data[nrow(data),"Transmission"]="automatic"
data[nrow(data),"Continent"]="Am"

### Explore the data

head(data)
summary(data)
names(data)

par(mgp=c(1.8,.5,0), mar=c(3,3,2,1)) 
hist(data$Horsepower, breaks=20, col="blue")     # notice that there are some super powerful cars


### Let's find out which are those super powerful cars

attach(data)
Make.Model[data$Horsepower > 400]
Make.Model[data$Horsepower > 390]
Make.Model[data$Horsepower <= 100]

### Let's concentrate on  "regular" cars and exclude those super expensive ones 
datacar=data[(data$Horsepower <=390) & (data$Horsepower > 100), ]


### From now on we use datacar

variables = c("Make.Model", "Continent", "MPG_City", "MPG_Hwy",
				"Horsepower","Weight","Length", "Width",
				"Seating","Cylinders","Displacement",
				"Make", "Transmission")
				
data1=datacar[, variables]  # include only useful variables
str(data1)

### Make all the categorical var's as factors
data1$Continent=as.factor(data1$Continent)
data1$Make=as.factor(data1$Make)
data1$Transmission=as.factor(data1$Transmission)

### Get pairwise scatter plots, except Make.Model

plot(data1[,-1])  #pair-wise scatter plots
pairs(data1[,-1])	#pair-wise scatter plots
#cor(data1)    	# pair-wise correlations, only takes numerical var's though
#cor(na.omit(data1[,3:10])) # doesn't seem to take missing values either

### model1: Simple reg, MPG_City vs. Length 


fit1=lm(MPG_City~ Length, data=data1)    # fit model 1

par(mgp=c(1.8,.5,0), mar=c(3,3,2,1))    # catter plot with the LS line
plot(data1$MPG_City~ Length, data=data1, pch=16)
abline(fit1, lwd=4, col="red")

summary(fit1)


plot(fit1$fitted.values, fit1$residual, pch=16)   # Residual plot
abline(h=0, lwd=5, col="red")

predict(fit1, data1[nrow(data1),],  interval="confidence", se.fit=TRUE) # Mean intervals
predict(fit1, data1[nrow(data1),],  interval="predict", se.fit=TRUE) # future prediction intervals


### model 2: two features Length and Horsepower

fit2=lm(MPG_City~ Length+Horsepower, data=data1) # fit model 2
summary(fit2)

par(mgp=c(1.8,.5,0), mar=c(3,3,2,1)) 
plot(fit2$fitted.values, fit2$residuals, pch=16)
abline(h=0, lwd=6, col="red")

### F-test, or in this case it is same as the t test: H0: beta_HP=0
anova(fit1, fit2) # Notice that the F stat = t^2 for HP



### Model with all four var's

fit3=lm(MPG_City~Horsepower+ Weight+Seating+Length, data=data1)
summary(fit3) # all four var's are significant

confint(fit3) # provides CI's for each coef's

# inference for the model through F test and Rsquare. RSE is another important measurement
(summary(fit3))$r.square
RSquare=(cor(fit3$fitted.values,fit3$model[,1] ))^2  # Rsquare is again the cor^2 of y and fitted y
RSquare

# Visualize the goodness of fit by ploting y vs. hat y       
plot(fit3$fitted.values, fit3$model[,1], pch=16,   # looking for a evenly spread dots around the line
     main="Y vs. hat Y",
     xlab="fitted Y",
     ylab="Y") 
abline(a=0,b=1, lwd=5, col="red")


# Visualize the goodness of fit by ploting the residuals vs. fitted y 

par(mgp=c(1.8,.5,0), mar=c(3,3,2,1)) 
plot(fit3$fitted.values, fit3$residuals, pch=16)
abline(h=0, lwd=6, col="red")

### this session is to reproduce the LS estimates together with cov. matrix etc. ####
############## skip this ############################################################

design.x= model.matrix(fit3)  #fit3$model gives us Y and x1 x2...xp 
y=fit3$model[, 1]
design.x[,1]

beta=  (solve( t(design.x) %*% design.x)) %*% t(design.x) %*% y # reconstruct LS estimates
rse=(summary(fit3))$sigma
cov.beta= (rse^2) * solve((t(design.x) %*% design.x))
sd.beta=sqrt(diag(sigma.beta))    # check to see this agrees with the sd for each betahat

summary(fit3)$cov.unscaled   # inverse (X' X)
cov.beta=(rse^2) * (summary(fit3)$cov.unscaled)  # alternatively we can get the cov matrix this way
################### Skip #####################################################################

### What does the F test do here
### What does a t-test do?
### R squared and RSE


### a quick look at an F distribution
hist(rf(10000, 4, 200), freq=FALSE, breaks=200)   # pretty skewed to the right




### Test if adding a bunch of var's will be helpful. 
### H0: beta Length=beta Width=0

fit4=lm(MPG_City~Horsepower+ Weight+Seating+Length+Width, data=data1)
fit5=lm(MPG_City~Horsepower+ Weight+Length, data=data1)
# To test if H0: beta_seating=beta_width=0
# use F test

anova(fit5,fit4)   # this gives us the F test of beta_ and beta_Width are both 0

### Let's say at the moment model 5 is good. Let's get confidence intervals and prediction
### intervals for the new design

### The last row in the data is the new design

newdesign.mean = predict(fit5, data1[nrow(data1),], interval="confidence", se.fit=TRUE)
newdesign.mean

newdesign.predic=predict(fit5, data1[nrow(data1), ], interval="prediction", se.fit=TRUE)
newdesign.predic

### Model diagnostics
### Let's take a closer look at model 5: HP, Wt and Seats. 
### Residual plots tell a lot: linearity, homoscedastcicity? qqplot for normality
### Since it is a multiple reg. we plot residuals vs. fitted values

### to check linearity: fitted values vs. residuals and look for symmetry pattern 
plot(fit5$fitted.values, fit5$residuals, pch=16)
abline(h=0, lwd=4, col="red")   # seems to have some problems 

### to check equal variances, plot fitted vs. abs(residuals) and look for a band

plot(fit5$fitted.values, abs(fit5$residuals), pch=16)
abline(h=0, lwd=4, col="red") 

### to check normality
qqnorm(fit5$residuals)
qqline(fit5$residuals, lwd=4, col="blue")

hist(fit5$residuals, breaks=20) # normality is ok




### Multiple regression with categorical variables
### Let's use Continent as one variable. It has three categories.
### Are Asian cars more efficient? 

attach(data1)
levels(Continent)

### Model with a categorical variable. This is same as a One Way ANOVA

### Get the sample means and sample sd for each group
tapply(MPG_City, Continent, mean) # Oops, missing values
tapply(MPG_City, Continent, mean, na.rm=T)
tapply(MPG_City, Continent, sd, na.rm=T)
plot(Continent, MPG_City) 


fit6=lm(MPG_City~Continent, data1)
summary(fit6)  		# The F test here is to test there are no differences among the three regions
fit6.1=lm(MPG_City~Continent+0, data1)  # Cell mean model
summary(fit6.1)

model.matrix(fit6)





### To estimate the mean difference between two regions, I installed two packages: contrast and sandwich
library(contrast)
contrast(fit6, list(Continent='Am'), list(Continent='As'))  ###compares two vars. tests Mu_Am - Mu_E
contrast(fit6, list(Continent='As'), list(Continent='E'))   ###compares two vars. tests Mu_As - Mu_E = B_As - B_E
###this also gives you the SE of (B_As - B_E)



### Fit 6 ignores other important variables: Let's add HP in addition to Continent

### Model without interaction: assume the effects of HP are the same across from different regions
fit7=lm(MPG_City~Continent+Horsepower, data1)  

### The plots help us to understand the model assumption


attach(data1)
plot(Horsepower, MPG_City, pch=16, col=as.factor(Continent), )
fit7=lm(MPG_City~Continent+Horsepower, data1)
coefs=(summary(fit7))$coefficients[, 1]
abline(a=coefs[1], b=coefs[4], col=1, lwd=3)
abline(a=coefs[1]+coefs[2], b=coefs[4], col=2, lwd=3)
abline(a=coefs[1]+coefs[3], b=coefs[4], col=3, lwd=3)
legend("topright", legend=c("Am", "As", "E"),
       lty=c(1,1), lwd=c(2,2), col=as.factor(sort(unique(Continent))))

summary(fit7)

fit8=lm(MPG_City~Horsepower, data1) ##test to see if they are significantly different
anova(fit7, fit8)		# F(2,222) 3,806
pvalue=1-pf(3.806, 2,222)
pvalue


### Model with interactions: the slopes for HP might be different for each region
fit9=lm(MPG_City~Continent*Horsepower, data1)
summary(fit9)
fit9.1=lm(MPG_City~Continent*Horsepower+0, data1)
summary(fit9.1)
### Plot fit9: model with interactions

plot(Horsepower, MPG_City, pch=16, col=as.factor(Continent), )
fit9=lm(MPG_City~Continent*Horsepower, data1)
coefs=(summary(fit9))$coefficients[, 1]
abline(a=coefs[1], b=coefs[4], col=1, lwd=3)
abline(a=coefs[1]+coefs[2], b=coefs[4]+coefs[5], col=2, lwd=3)
abline(a=coefs[1]+coefs[3], b=coefs[4]+coefs[6], col=3, lwd=3)
legend("topright", legend=c("Am", "As", "E"),
       lty=c(1,1), lwd=c(2,2), col=as.factor(sort(unique(Continent))))


###  test interaction
anova(fit7, fit9)   # quite small p-value!



### Nonlinearity: x variable may relate to y through a function of x, say log(x) or x^2
### We can handle this by transforming x 
### First let's find out if there is some non-linear relationship there by looking at 
### pairwise scatter plots

data2=data1[, -1]  # take the model out of the data
pairs(data2) # Horsepower may need a transformation

plot(Horsepower, MPG_City, pch=16) # oops, a curve would have fitted the data better! 1/HP?
fit10=lm(MPG_City~Horsepower)
plot(fit10$fitted, fit10$residuals, pch=16) # Use residual plots 
abline(h=0, lwd=4, col="red")  # We see a curve would have fitted the data better

plot(1/Horsepower, MPG_City, pch=16)  #looks better!

fit11=lm(MPG_City~ I(1/Horsepower))
plot(fit11$fitted, fit11$residuals, pch=16)
abline(h=0, lwd=4, col="red") # The residuals are evenly spread out without patterns and symmetric to 0


### Colinearity: When some x's are highly correlated we can't seperate the effect. But
### it is still fine for the purpose of prediction.

### A simulation to illustrate some consequences of colinearity
###  Each p-value for x1 and x2 is large but the null of both
### coef's =0 are rejected....  Because x1 and x2 are highly correlated.

par(mfrow=c(2,1))
x1=runif(100)
x2=2*x1+rnorm(100, 0,.1)    # x1 and x2 are highly correlated
y=1+2*x1+rnorm(100,0, .7)   # model

plot(x1, y)
summary(lm(y~x1))
summary(lm(y~x2))
summary(lm(y~x1+x2))
cor(x1, x2)
plot(x1, x2)




### Model diagnosis. Here is a case the all the linear model assumptions 
### are met. 

par(mfrow=c(3,1))

x1=runif(100)
y=1+2*x1+rnorm(100,0, .7)
fit.perfect=lm(y~x1)
plot(x1, y, pch=16, 
     xlab="a perfect linear model")
abline(fit.perfect, lwd=4, col="red")

plot(fit.perfect$fitted, fit.perfect$residuals, pch=16)
abline(h=0, lwd=4, col="red")

qqnorm(fit.perfect$residuals)
qqline(fit.perfect$residuals, lwd=4, col="blue")




### Bootstrap: take a random sample of size M from the Data with replacement. 
### Repeat this N times
### source("d:/stat/bootstrap.txt", echo=T): http://statistics.ats.ucla.edu/stat/r/library/bootstrap.htm

datacar = read.csv("Cars_04_Subset", header=TRUE)
names(datacar)
b1=sample(datacar, 100, replace=TRUE)

library(leaps)
help(leaps)
data <- round(rnorm(100, 5, 3), 2)
data=matrix(data, ncol=4)
hist(data, breaks=20)
y=rnorm(25,6,1)
plot(data[,1], y)
leaps(data, y)

mean(data)
sd(data)   #Not too bad

resamples <- lapply(1:200, 
    function(i)
    sample(data, 50, replace = T))

r.median <- sapply(resamples, median)
r.median

hist(r.median, breaks=20)

mean(r.median)
sd(r.median)


library(boot)
data(city)
help(boot)


### Post Model Selection (POSI)





### A complete analysis for Car's data. The goal of the study
### 1) Are Asian cars more efficent?
### 2) What are the effects of features on MPG?
### 3) Estimate the mean and an individual MPG similar to the new design




------------------------------------------------------------



### 1. Load the data and exam any abnormality

rm(list=ls()) # Remove all the existing variables

dir=c("/Users/lindazhao/Desktop/Dropbox/STAT471/Data") # school
dir=c("/Users/lzhao/Dropbox/STAT471/Data")   # my laptop
setwd(dir)

data = read.csv("Cars_04.csv", header=T, as.is=T)
str(data)

head(data)
summary(data)
names(data)

var.no=names(data) %in% c("Origin","EPA_Class", "Make", "Model", "Turndiam")
data1=data[!var.no]  # only take var's needed


hist(data1$Horsepower)     # notice that there are some super powerful cars


### Let's find out which are those super powerful cars

Make.Model[data1$Horsepower > 400]
Make.Model[data1$Horsepower > 390]
Make.Model[data1$Horsepower <= 100]

### Let's concentrate on  "regular" cars and exclude those super expensive ones 
data1=data1[(data1$Horsepower <=390) & (data1$Horsepower > 100), ]


### 2. Explore the function forms for y and x's


### Make all the categorical var's as factors
data1$Continent=as.factor(data1$Continent)
data1$Transmission=as.factor(data1$Transmission)

str(data1)
### Take a look at the pairwise scatter plots

attach(data1)
pairs(data1[-1], pch=16)    # without the Make.Model
### Notice several variables seem to present curvitures
### Let's try to transform y=MPG_City to log y. It didn't seem to help
### I then tried 1/PMG_City=GPM. Better!

attach(data1)
par(mfrow=c(1,2))
plot(Horsepower, log(data1$MPG_City), pch=16)
plot(Horsepower, 1/MPG_City, pch=16)


GPM=1/MPG_City
plot(Horsepower, GPM)
data2=cbind(GPM, data1)
names(data2)
pairs(data2[-2])   # pairwise scatter plots but Make.Model

str(data2)


### 3. Start to build a model
  # a) Y=GPM
  # b) Tansmission is a very unbalance varible. Will not use it here
  # c) Since one goal is to see the effect of Continent so I will keep it in all the time
  # d) Will do a backward selection

attach(data2)
str(data2)
fit1=lm(GPM~Continent+Horsepower+Weight+Seating+Length+Width+Displacement+Cylinders, data2)
summary(fit1)
plot(fit1)   

# plotting the fit gives us a few useful plots. Number 126 seems to be an outlier
data2[126,]   # oops, Maserati.... What about we take it out.
data2=data2[-126,] # updates the data without Maserati
fit1=lm(GPM~Continent+Horsepower+Weight+Seating+Length+Width+Displacement+Cylinders, data2)
plot(fit1)
summary(fit1)

fit2=lm(GPM~Continent+Horsepower+Weight+Seating+Length+Displacement+Cylinders, data2)
summary(fit2)

fit3=lm(GPM~Continent+Horsepower+Weight+Seating+Length+Cylinders, data2)
summary(fit3)

fit4=lm(GPM~Horsepower+Weight+Seating+Length+Cylinders, data2)
anova(fit4, fit3)    # Unfortunately there is no evidence that Continent is significant after accounting for some features!!!

summary(fit4)   # Seating is no longer sig. 

fit5=lm(GPM~Horsepower+Weight+Length+Cylinders, data2)    # This is our final model!
summary(fit5)

### Model diagnoses for fit5

plot(fit5)

### Create our new design poit as the same data.frame
newdesign= data2[1,]
newdesign[1,1] = NA
newdesign[1,2] = NA
newdesign[1,3] = "Am"
newdesign[1,4] = NA
newdesign[1,5] = 225
newdesign[1,6] = 4
newdesign[1,7] = 5
newdesign[1,8] = 180
newdesign[1,9] = NA
newdesign[1,10] = "automatic"
newdesign[1,11] = 70
newdesign[1,12] = 3.5
newdesign[1,13] = 4

newdesign

### Finally predictions!

newdesign.mean=predict(fit5, newdesign, interval="confidence")
newdesign.mean

newdesign.predit=predict(fit5, newdesign, interval="prediction")
newdesign.predit

#### End of the ANALYSIS ####











### Let's try model seleciton function regsubsets() which is part of package: leaps
library(leaps)
fit.f=regsubsets(GPM~Continent+Horsepower+Weight+Seating+Length+Width+Displacement+Cylinders, data2, 
                force.in=c( "ContinentAs","ContinentE"), method="forward")
      # method=c("exhaustive", "backward", "forward", "seqrep")
fit.b=regsubsets(GPM~Continent+Horsepower+Weight+Seating+Length+Width+Displacement+Cylinders, data2, 
                force.in=c( "ContinentAs","ContinentE"), method="backward")

fit.all=regsubsets(GPM~Continent+Horsepower+Weight+Seating+Length+Width+Displacement+Cylinders, data2, 
                    nvmax=10, method="exh")   #by default nvmax=8!


######## When the linear model shows strong heteroscedasticity, all the inference made 
######## based on the model assumption may not be trust worthy. We may try to fix it by
######## transformations on y etc. But we could also use Bootstrap!!!!

######## Bootstrap  #####
# A simple way of estimating the distribution of a statistic from a random sample
# Useful to produce confidence intervals of a stat.

# The following example shows how to produce ci's for linear coef's and r-square
library(boot)

fit=lm(GPM~Horsepower+Weight+Length, data2) # let's consider the linear model with three x's
confint(fit)
fit$coef

# first define the function to produce LS est's of beta's
coef=function(formula, data, indices)
    {data.temp=data[indices,]
     fit=lm(formula, data=data.temp)
     return(fit$coef)}

# Take 100 bootstrap samples and produces b-confidence intervals for each ceof's
est.boot=boot(data=data2, statistic=coef, formula=GPM~Horsepower+Weight+Length, R=100)

# view est.boot
est.boot
plot(est.boot, index=2) # beta HP
plot(est.boot, index=3) # beta Wt
plot(est.boot, index=4) # beta Length


boot.ci(est.boot, type="perc", index=2)




# function to obtain R-Square from the data 
rsq <- function(formula, data, indices) 
  {
  data.temp <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=data.temp)
  return(summary(fit)$r.square) # return the r.square
  } 

# bootstrapping with R replications 
b.rsq <- boot(data=data2, statistic=rsq, 
                R=100, formula=GPM~Horsepower+Weight+Length)

b.rsq
plot(b.rsq)
hist(b.rsq$t, breaks=20)
boot.ci(b.rsq, type="perc")
boot.ci(b.rsq, type="norm")

### Try bootstrap 
dim(data2)
indices=sample(1:228, 228, replace=T)
data.b=data2[indices,]
b=(lm(GPM~Horsepower+Weight+Length))$coef
b