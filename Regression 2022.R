# 1.Loading the  Dataset LifeExpectancy
LifeExp<-read.csv("./LifeExpectancyData.csv")
head(LifeExp)


# 2. Regression models
# checking the type of the variable
class(LifeExp)
class(LifeExp[,1])
# lapply function is used to inspect the class of each column
lapply(LifeExp,class)
# indexing all columns that are numeric
Numeric_num <- unlist(lapply(LifeExp,class)) == "numeric"
Numeric_num

# 3. Data Exploration
# correlation between the variables by default pearson(linear) correlation coefficient 
cor(LifeExp[,Numeric_num])
# checking the correlation between the variables and removing the NA values
cor(LifeExp[,Numeric_num],use='complete.obs')
cor(LifeExp$infant.deaths,LifeExp$Life.expectancy,use='complete.obs')
cor(LifeExp$Adult.Mortality,LifeExp$Life.expectancy,use='complete.obs')
# calculating spearman correlation coefficient
cor(LifeExp[,Numeric_num],method="spearman",use='complete.obs')
# plotting the correlation
plot(LifeExp[,Numeric_num])
plot(LifeExp$infant.deaths,LifeExp$Life.expectancy,pch=20,col='green')
plot(LifeExp$Adult.Mortality,LifeExp$Life.expectancy,pch=20,col='green')


# 4. Building a regression model
# building a regression using lm function

regmodel1 <- lm( Life.expectancy ~ Adult.Mortality, data=LifeExp) 
regmodel1
# checking the summary and storing the result
result1<-summary(regmodel1)
# getting all  the varibles using the function names
names(result1)
# Get p-values and evaluate with a = 0.05 (5% significance)
result1$coefficients
pval <- result1$coefficients[,4]
pval <= 0.05
# extract the R-square of  regmodel1
regmodel1r2 <- result1$r.squared
regmodel1r2
# calculate the Akaike Information Criteria (similar to accuracy metrics) using the AIC function
regmodel1aic <- AIC(regmodel1)
regmodel1aic

# fit the regression model2 using life expectancy vs population
regmodel2 <- lm( Life.expectancy ~ Population, data=LifeExp) 
regmodel2
# checking the summary 
result2<-summary(regmodel2)
result2


# extract the R-square of  regmodel2
regmodel2r2 <- result2$r.squared
regmodel2r2

regmodel2aic <- AIC(regmodel2)
regmodel2aic

result2$coefficients
pval2 <- result2$coefficients[,4]
pval2 <= 0.05
# comparing the models using  R square and AIC

# merging both values into a single vector using the function c the 
#first value will be the R square of first model(regmodel1) and regmodel2
merger2 <- c(regmodel1r2,regmodel2r2)
merger2

round(merger2,2)

names(merger2) <-c("Adult mortality","population")
round(merger2,3)

aic <- c(regmodel1aic,regmodel2aic)
aic
names(aic) <- names(merger2)
round(aic,3)

# fit the regression model3 using life expectancy vs infant deaths
regmodel3 <- lm( Life.expectancy ~ infant.deaths, data=LifeExp) 
regmodel3
# checking the summary 
result3<-summary(regmodel3)
result3

# extract the R-square of  regmodel3
regmodel3r2 <- result3$r.squared
regmodel3r2

regmodel3aic <- AIC(regmodel3)
regmodel3aic

# model with intercept
lm(Life.expectancy ~ Adult.Mortality, data=LifeExp)
lm(Life.expectancy ~ Population, data=LifeExp)
lm(Life.expectancy ~ infant.deaths, data=LifeExp)


# model without intercept
lm(Life.expectancy ~ 0 + Adult.Mortality, data=LifeExp)
lm(Life.expectancy ~ 0 + Population, data=LifeExp)
lm(Life.expectancy ~ 0 + infant.deaths, data=LifeExp)

# 5 Multiple regression using Adult mortality, Population and infant deaths
regmodel4<-lm( Life.expectancy ~ Adult.Mortality+Population+infant.deaths, data=LifeExp) 
regmodel4

result4<-summary(regmodel4)
result4

# Create a variable yy that includes the first 10 values of
# Adult.Mortality, our target variable
yy <- LifeExp[,colnames(LifeExp)=="Life.expectancy"]
yy <- yy[1:10]

# Now create a matrix with 9 columns of random data
# The function runif() creates random draws from a uniform
# distirbution
xx <- matrix(runif(90),ncol=9) # Draw 100 values and put them
# in a matrix with 10 columns
# Loop for all regressions from 1 to 9 inputs
ftemp <- list() # Pre-allocate a list to save the results
for (i in 1:9){
  ftemp[[i]] <- lm(yy ~xx[,1:i])
}

# Get R-squared from all models
r2temp <- unlist(lapply(ftemp,function(LifeExp){summary(LifeExp)$r.squared}))
plot(1:9,r2temp,xlab="Number of random inputs",ylab="R-squared",main="R squared model")


# Extract the AIC of these models
sapply(ftemp,AIC)

#Calculating the error for the 10 data points
yy - ftemp[[9]]$fitted.values# all errors are zero

ftemp[[10]] <- lm(yy~1) # This means just fit a constant
sapply(ftemp,AIC)
#The last AIC value corresponds to the model only with the constant.

# We check again R2
unlist(lapply(ftemp,function(LifeExp){summary(LifeExp)$r.squared}))

# The model with the constant has an R2of zero, as it does not explain anything more than the mean.

# used tidyverse to omit the non-numeric columns
install.packages("tidyverse")
library(tidyverse)
LifeExp <- LifeExp %>% 
  drop_na()

#Create and compare models
regmodel5 <- lm(Life.expectancy ~.,data= LifeExp)# build a full model
summary(regmodel5)
fitmin <- lm( Life.expectancy ~ 1, data=LifeExp)
fitmin

# build the stepwise routines using the function step
# We use the argument direction to control how the stepwise evolves and the argument scope to define the full model.
regmodel6 <- step(fitmin,direction="both",scope=formula(regmodel5)) 
regmodel6
summary(regmodel6)
regmodel7 <- step(fitmin,direction="forward",scope=formula(regmodel5))
regmodel7
summary(regmodel7)
regmodel8 <- step(fitmin,direction="backward",scope=formula(regmodel5))
regmodel8
summary(regmodel8)
# Observe that in setting the scope argument we used the function formula(). This is to tell R that this is not a
#simple text and that it should look for variables named like that.
#Let us compare the solutions from earlier on (regmodel1 and regmodel2) with the full model regmodel5 and a stepwise result regmodel6.
#We use AIC for that purpose.
aic <- c(AIC(regmodel1),AIC(regmodel2),AIC(regmodel5),AIC(regmodel6))
names(aic) <- c(formula(regmodel1),formula(regmodel2),"Full model","Stepwise")
round(aic,4)

plot(regmodel6)

#To get all 4 plots in one screen
#we ask R to split the plot in a 2 by 2 matrix:
  par(mfrow=c(1,1)) # Split into 2 by 2
plot(regmodel6) # Plot

par(mfrow=c(1,1)) # Revert to a single plot. Otherwise it will keep on plotting on a 2 by 2 matrix.

# Alternatively, we can extract the residuals and produce any plot we want manually.
resid <- regmodel6$residuals
resid
fitted <- regmodel6$fitted.values
fitted

par(mfrow=c(1,1))
plot(fitted,resid) # Scatter plot fitted vs. residuals
plot(LifeExp$Adult.Mortality,resid) # Scatter plot lstat vs. residuals
plot(LifeExp$infant.deaths,resid) # Scatter lat vs. residuals
hist(resid,100) # Historgram of residuals with 100 bins

# 7 Predicting with regression
#split
#the initial dataset into a training and a test set, build the model in the training set, and generate predictions for the
#test set.

idx <- sort(sample(1:nrow(LifeExp),100))
idx
LifeExpTest <- LifeExp[idx,]
LifeExpTest
LifeExpTrain <- LifeExp[-idx,]
LifeExpTrain

# Using these let us build a simple regression
fitTrain <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths, data=LifeExpTrain)
fitTrain
# prediction
predict(fitTrain,newdata=LifeExpTest)

predict(fitTrain) # No newdata input uses the training set


fitTrain$fitted.values

#8 Using regression for hypothesis testing
set.seed(1)
x1 <- rnorm(50,mean=20,sd=10)
x2 <- rnorm(50,mean=30,sd=10)
x3 <- rnorm(50,mean=21,sd=10)

x1x2 <- c(x1,x2)
id12 <- c(rep(1,length(x1)),rep(2,length(x2))) # rep() repeats a number as many times as the 2nd argument instructs.
id12 <- as.factor(id12)
id12

# Now we build the regression
summary(lm(x1x2~id12))

mean(x1)
mean(x2)
mean(x2)-mean(x1)

x1x3 <- c(x1,x3)
id13 <- c(rep(1,length(x1)),rep(3,length(x3)))
id13 <- as.factor(id13)
summary(lm(x1x3~id13))

x1x2x3 <- c(x1,x2,x3)
id123 <- c(rep(1,length(x1)),rep(2,length(x1)),rep(3,length(x3)))
id123 <- as.factor(id123)
summary(lm(x1x2x3~id123))

x4 <- rnorm(10,mean=30,sd=10) # Only 10 observations
x1x4 <- c(x1,x4)
id14 <- c(rep(1,length(x1)),rep(2,length(x4))) # I can use any numbers I want, they do not mean anything!
id14 <- as.factor(id14)
summary(lm(x1x4~id14))

# Answer to the Questions
# calculating the errors
install.packages("Metrics")
library(Metrics)
predict(fitTrain,newdata=LifeExpTest)
# To find the range 
range(LifeExp$Life.expectancy)
# Mean squared error (MSE) used to finding how close a regression line is to the set of points
mse(LifeExp$Life.expectancy, predict(regmodel7 , LifeExp))
# Root Mean Square Error (RMSE) is the standard deviation of the errors prediction
rmse(LifeExp$Life.expectancy, predict(regmodel7 , LifeExp))
# Mean absolute error (MAE) is the loss function used in regression.
mae(LifeExp$Life.expectancy, predict(regmodel7 , LifeExp))
# Mean Absolute Percentage Error (MAPE) is a statistical measure to finding the accuracy of a machine learning algorithm
mape(LifeExp$Life.expectancy, predict(regmodel7 , LifeExp))



# population
cor(LifeExp$Population,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Population,LifeExp$Life.expectancy,pch=20,col='green')
# BMI(Body Mass Index)
cor(LifeExp$BMI,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$BMI,LifeExp$Life.expectancy,pch=20,col='red')
# Alcohol
cor(LifeExp$Alcohol,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Alcohol,LifeExp$Life.expectancy,pch=20,col='red')
# Schooling
cor(LifeExp$Schooling,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Schooling,LifeExp$Life.expectancy,pch=20,col='blue')
# infant and adult mortality
cor(LifeExp$infant.deaths,LifeExp$Life.expectancy,use='complete.obs')
cor(LifeExp$Adult.Mortality,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$infant.deaths,LifeExp$Life.expectancy,pch=20,col='green')
plot(LifeExp$Adult.Mortality,LifeExp$Life.expectancy,pch=20,col='green')

#Expenditure
cor(LifeExp$Total.expenditure,LifeExp$Life.expectancy,use='complete.obs')
cor(LifeExp$percentage.expenditure,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Total.expenditure,LifeExp$Life.expectancy,pch=20,col='green')
plot(LifeExp$percentage.expenditure,LifeExp$Life.expectancy,pch=20,col='green')


# Immunisation

cor(LifeExp$Hepatitis.B,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Hepatitis.B,LifeExp$Life.expectancy,pch=20,col='red')
cor(LifeExp$Measles,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Measles,LifeExp$Life.expectancy,pch=20,col='red')
cor(LifeExp$Polio,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Polio,LifeExp$Life.expectancy,pch=20,col='red')
cor(LifeExp$Diphtheria,LifeExp$Life.expectancy,use='complete.obs')
plot(LifeExp$Diphtheria,LifeExp$Life.expectancy,pch=20,col='red')
