#Problem 1
# Data set bostonhousing.txt, created by Harrison and Rubinfeld [1978], concerns housing values in suburbs
# of Boston. The attributes include
# MEDV Median value of owner-occupied homes in $1000's
# CRIM per capita crime rate by town
# ZN proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS proportion of non-retail business acres per town
# CHAS Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# NOX nitric oxides concentration (parts per 10 million)
# RM average number of rooms per dwelling
# AGE proportion of owner-occupied units built prior to 1940
# DIS weighted distances to five Boston employment centres
# RAD index of accessibility to radial highways
# TAX full-value property-tax rate per $10,000
# PTRATIO pupil-teacher ratio by town
# B 1000(Bk 􀀀 0:63)2 where Bk is the proportion of blacks by town
# LSTAT % lower status of the population,
# in which MEDV is the response variable. The summary of the data set is below.
# Name of the data set bostonhousing
# Number of observations 506
# Number of attributes 14 (1 response variable and 13 explanatory variables)

# a)Build regression model reg and display summary() of the model. Pick two explanatory variables that are
# least likely to be in the best model, and support your suggestion in one sentence.
bostonhousingdata <- read.delim("bostonhousing.txt")
reg <- lm(MEDV~.,data=bostonhousingdata)
summary(reg)
#indus age

# b)Build regression model reg.picked by excluding the two explanatory variables selected in problem 1(a).
# Display summary() of the model.
reg.picked <- lm(MEDV~CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT,data=bostonhousingdata)
summary(reg.picked)

# c)For a regression model, the mean squared error (MSE) is defined as SSE/n-1-p
# in which p is the number of
# explanatory variables used in the model. The mean absolute error (MAE) is similarly defined: SAE/n-1-p
# Display MSE and MAE for regression models reg and reg.picked from the previous problems. Based on
# MSE and MAE, pick one model you prefer.
#MSE reg
mean(reg$residuals^2)
#MSE reg.picked
mean(reg.picked$residuals^2)
#MAE reg
mean(abs(reg$residuals))
#MAE reg.picked
mean(abs(reg.picked$residuals))

#d)Run step() using regression model reg in problem 1(a). Compare the model with reg.picked in problem
#1(b).
library(MASS)
reg.step = stepAIC(object=reg, direction="both")
summary(reg.step)

#Problem 2
# Import labdata.txt. The summary of the data set is below.
# Name of the data set labdata
# Number of observations 400
# Number of attributes 9 (1 response variable and 8 explanatory variables)
# Column y is the response variable and remaining attributes x1,x2,... are the explanatory variables.
# labdata <- read.delim("labdata.txt")

#a)Build regression model reg and display summary() of the model
reg <- lm(y~.,data=labdata)
summary(reg)

#b)For each explanatory variable, plot it against the response variable. Based on the scartter plots, pick one
# variable that is most likely to be used in a piecewise regression model. Attach one plot associated with
# the variable you pick.
plot(labdata)
reg.piecewise <- lm(y~x1,data=labdata)
library(segmented)
reg.seg = segmented(reg.piecewise, seg.Z = ~x1, psi=13)
plot(labdata$x1,labdata$y)
plot(reg.seg,add=T)
summary(reg.seg)

# c)Calculate the mean of the variable you pick in problem 2(b) and build piecewise regression model reg.piece
# using the mean. Is model reg.piece better than model reg in problem 2(a)? Support your argument in
# one sentence.
meanx1 <- mean(labdata$x1)
reg.piece = segmented(reg.piecewise, seg.Z = ~x1, psi=meanx1)
plot(labdata$x1,labdata$y)
plot(reg.piece,add=T)
summary(reg.piece)
