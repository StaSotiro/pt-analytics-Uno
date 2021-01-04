require(glmnet)
require(corrplot)
library(dplyr)
library(nortest)
library(car)
library(randtests); 
library(lmtest);
library(dplyr)
library(tidyverse)
library(caret)

nonNumeric = function(x){
  return(!is.numeric(x))
}

existsIn = function (item, array, arrayAsString){
  if(item %in% array | grepl(item, arrayAsString)){
    return(item)
  }
}
notExistsIn = function (item, array, arrayAsString){
  if(!(item %in% array | grepl(item, arrayAsString))){
    return(item)
  }
}

randPoly = function(x) { 
  un = length(unique(x))
  rn = floor(runif(1,2,5))
  if(is.numeric(x)){
    if(un<=rn){
      print(c(col, un-1))
      return(poly(x, un-1))
    }else{
      print(c(col, rn))
      return(poly(x, rn))
    }
  }
  else{
    return(x)}
  }

getAIC = function(lassoModel, testData){
  
  selectedLasso = c(names(lassoModel$coefficients), "SalePrice")
  collapsedLassoNames = paste(selectedLasso, collapse= " ")
  newLassoSelectedNames = sapply(colnames(testData), existsIn, selectedLasso, collapsedLassoNames)
  newLassoSelectedNames = newLassoSelectedNames[!sapply(newLassoSelectedNames, is.null)]
  
  aicBaseModel = lm(SalePrice ~ ., data = select(testData,names(newLassoSelectedNames)))
  aicModel = step(aicBaseModel, direction='both')
  
  summary(aicModel)
  return(aicModel)
}

getCleanLasso = function(lassoModel, NaValues, testData){
  
  newSelectedNames = paste(lassoModel$coefficients, collapse = " ")
  
  selectedLasso = c(names(lassoModel$coefficients), "SalePrice")
  collapsedLassoNames = paste(selectedLasso, collapse= " ")
  newLassoSelectedNames = sapply(colnames(testData), existsIn, selectedLasso, collapsedLassoNames)
  newLassoSelectedNames = newLassoSelectedNames[!sapply(newLassoSelectedNames, is.null)]
  
  postRemoveNames = sapply(newLassoSelectedNames, notExistsIn, NaValues, paste(NaValues, collapse = " "))
  postRemoveNames = postRemoveNames[!sapply(postRemoveNames, is.null)]
  
  lassoModel2 = lm(SalePrice ~ . , data = select(houseData,names(postRemoveNames)))
  
  return(lassoModel2)
}

getCleanModelColumns = function(model, testData){
  
  selectedLasso = c(names(model$coefficients), "SalePrice")
  collapsedLassoNames = paste(selectedLasso, collapse= " ")
  newLassoSelectedNames = sapply(colnames(testData), existsIn, selectedLasso, collapsedLassoNames)
  newLassoSelectedNames = newLassoSelectedNames[!sapply(newLassoSelectedNames, is.null)]

  return(select(houseData,names(newLassoSelectedNames)))

}

getCleanNames = function(model, NaValues, testData){
  
  newSelectedNames = paste(model$coefficients, collapse = " ")
  
  selectedLasso = c(names(model$coefficients), "SalePrice")
  collapsedLassoNames = paste(selectedLasso, collapse= " ")
  newLassoSelectedNames = sapply(colnames(testData), existsIn, selectedLasso, collapsedLassoNames)
  newLassoSelectedNames = newLassoSelectedNames[!sapply(newLassoSelectedNames, is.null)]
  
  postRemoveNames = sapply(newLassoSelectedNames, notExistsIn, NaValues, paste(NaValues, collapse = " "))
  postRemoveNames = postRemoveNames[!sapply(postRemoveNames, is.null)]
  
  return(postRemoveNames)
}


assumptionsTests = function(model){
  par(mfrow=c(1,1))
  
  plot(model, which = 2, main="QQ Plot")
  print(shapiro.test(rstandard(model)))
  # Constant variance
  
  # What is this ? 
  
  print("NCV Test")
  print(ncvTest(model))
  # Computes a score test of the hypothesis of constant error variance against the alternative
  # that the error variance changes with the level of the response (fitted values), or with a linear combination of predictors.
  # small P value, meaning that the error variance is not constant, but changes based on predictors
  
  print("Stud Residuals")
  StudResiduals = rstudent(model)
  yhat = fitted(model)
  par(mfrow=c(1,2))
  plot(yhat, StudResiduals, main="Residuals 1")
  abline(h=c(-2,2), col=2, lty=2)
  plot(yhat, StudResiduals^2, main="Residuals^2")
  abline(h=4, col=2, lty=2)
  plot(model, which=3, main="Residuals 2")
  
  # ------------------
  yhatQuantiles=cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
  print("Yhat Quantiles Table")
  print(table(yhatQuantiles))
  
  print("Levene test of Homogenity")
  print(leveneTest(rstudent(model)~yhatQuantiles))
  boxplot(rstudent(model)~yhatQuantiles, main="YhatQuantile Plot")
  
  # Non linearity
  print("Non linearity")
  residualPlot(model, type='rstudent', main="Linearity")
  residualPlots(model, plot=F, type = "rstudent", main="Linearity 2")
  
  # Independence 
  plot(rstudent(model), type='l',main="Independence")
  runs.test(model$res) # p > 0.5 we go for non Randomness
  print("DW test")
  print(dwtest(model)) # # p > 0.5 True auto Correlation > 0
  print("Durbin Watson Test")
  print(durbinWatsonTest(model))
}

getLasso = function(testData, startingModel){
  if(!is.null(startingModel)){
    print("Model Provided")
    model = startingModel
  }else{
    model = lm(SalePrice~.,data=testData)
  }
  X = model.matrix(model)[,-1]
  lasso = cv.glmnet(X, testData$SalePrice,alpha = 1)
  plot(lasso)
  
  min = coef(lasso, s = "lambda.min") # How do I interpret this? 
  lse = coef(lasso, s = "lambda.1se")
  
  plot(lasso$glmnet.fit, xvar = "lambda")
  abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)
  
  selected = min[min[,1]!=0,]
  
  selectedNames = c(names(selected)[-1],"SalePrice")
  
  collapsedNames = paste(selectedNames, collapse= " ")
  newSelectedNames = sapply(colnames(houseData), existsIn, selectedNames, collapsedNames)
  newSelectedNames = newSelectedNames[!sapply(newSelectedNames, is.null)]
  
  lassoModel = lm(SalePrice ~ ., data = select(houseData,names(newSelectedNames)))
  summary(lassoModel)
  
  lassoModel1 = lm(SalePrice ~ . -1, data = select(houseData,names(newSelectedNames)))
  summary(lassoModel1)
  
  return(lassoModel)
}


setwd("/Users/stavros/Documents/BI/Statistics I/Final assignment")
houseData = read.csv("ames_iowa_housing_52.csv", sep = ";")

# Code starts Here 

str(houseData)
summary(houseData)

# Data cleaning & Transforming
houseData = houseData[, -c(1,3)]


houseData$Lot.Frontage = as.numeric(houseData$Lot.Frontage)
houseData$Mas.Vnr.Area = as.numeric(houseData$Mas.Vnr.Area)
houseData$BsmtFin.SF.1 = as.numeric(houseData$BsmtFin.SF.1)
houseData$BsmtFin.SF.2 = as.numeric(houseData$BsmtFin.SF.2)
houseData$Bsmt.Unf.SF = as.numeric(houseData$Bsmt.Unf.SF)
houseData$Total.Bsmt.SF = as.numeric(houseData$Total.Bsmt.SF)
houseData$Garage.Yr.Blt = as.numeric(houseData$Garage.Yr.Blt)
houseData$Garage.Yr.Blt = as.numeric(houseData$Garage.Yr.Blt)

# integerCols = c("Order", "PID", "Lot.Frontage", "Lot.Area", "Overall.Qual", "Overall.Cond", "Year.Built", "Year.Remod.Add", "Mas.Vnr.Area",
#              "BsmtFin.SF.1", "BsmtFin.SF.2", "X1st.Flr.SF", "X2nd.Flr.SF", "Low.Qual.Fin.SF", "Gr.Liv.Area", "Bsmt.Full.Bath", "Bsmt.Half.Bath",
#              "Full.Bath", "Half.Bath", "Bedroom.AbvGr", "Kitchen.AbvGr","TotRms.AbvGrd","Fireplaces","Garage.Yr.Blt","Garage.Cars","Garage.Area",
#              "Wood.Deck.SF","Open.Porch.SF","Enclosed.Porch","X3Ssn.Porch","Screen.Porch","Pool.Area","Misc.Val","Mo.Sold","Yr.Sold","Yr.Sold","SalePrice")

# CHange NAs for numeric and non numeric 

index = sapply(houseData, class) == "character"  
numIndex = sapply(houseData, class) != "character"  

houseData[,numIndex][is.na(houseData[,numIndex])] = 0
houseData[,index][is.na(houseData[,index])] = "NotAvailable"

houseData$MS.SubClass = as.factor(houseData$MS.SubClass)
houseData$Overall.Qual = as.factor(houseData$Overall.Qual)
houseData$Overall.Cond  = as.factor(houseData$Overall.Cond )
houseData[index] = lapply(houseData[index], as.factor)

# Double Triple checks for NAs 
sum(apply(houseData,2, is.nan))
sum(apply(houseData,2, is.na))
lapply(houseData, function(x){length(which(is.na(x)))})

str(houseData)

numIndex = sapply(houseData, class) != "factor"

houseNums= houseData[,numIndex]

par(mfrow=c(1,1))

# 
class(as.numeric(gsub(",", "", houseNums$SalePrice)))
numPrice = as.numeric(gsub(",", "", floor(houseNums$SalePrice)))
typeof(numPrice)
for(col in colnames(houseNums)){
  plot(numPrice ~ as.numeric(houseNums[,col]), ylab = "Sale Price", xlab = col, main = paste("Sales Over ",col), type = "h" )
}
# Descrete mostly 

# Garage cars Interesting 
# Tot rooms above ground
# Tot bedrooms above ground
# Full bath
# year built

# grouped = houseNums %>% 
#    group_by(Fireplaces, Yr.Sold) %>% 
#    summarise_all(sum)
# grouped

par(mfrow = c(1,1))
hist(houseNums$Lot.Frontage) # outliers right skewed -> affects mean -> mean!=median -> normal distribution .. maybe log would work
hist(houseNums$Year.Built) # Mostly ppl purchase recently built houses rather than older constructions 
hist(houseData$Garage.Cars) # Mostly ppl would purchase 

par(mfrow = c(1,1))
qqPlot(lm(SalePrice ~ Gr.Liv.Area, data = houseData), id.n=2, main='QQplot for Outliers')

options(scipen=999)
plot(numPrice ~ houseNums$Year.Built, ylab = "Sale Price", xlab = "Year Built", main = paste("Sales Over ","year built"), type = "h" )
# some ppl tend to not purchase super high proices also ccould be raisex ? 

plot(table(houseData$Sale.Type), type = "h", ylim = c(0,1000)) # Interesting

plot(table(houseData$Sale.Condition), type = "h", ylim = c(0,1000))
plot(table(houseData$Foundation), type = "h", ylim = c(0,1000))
plot(houseNums$Gr.Liv.Area ~houseNums$Year.Built, ylab = "Living", xlab = "Year Built", main = "Sale Price over Fireplaces", type = "h")
corrplot(cor(houseNums))

# Looks like there is a relation between Price and Garage Area and Cars, The built and modification year, Ground living area, 1st floor sf, Baths and 
# Basically people are looking to be above ground and have a parking space for their cars. Which actually shows what's important in that area. 
# Could be for security reasons (stealing, damages), or more probably a very crowded area, and parking spots are vital.

##Anova foe Sale price among factor categories
anova1<-aov(SalePrice~Overall.Qual,data=houseData)
summary(anova1)

lillie.test(anova1$res);shapiro.test(anova1$res) 
leveneTest(SalePrice~Overall.Qual,data=houseData) ## all are rejected sale is skewwed so we will proceed with median
kruskal.test(SalePrice~Exter.Cond,data=houseData)## it is rejected as we expected
TukeyHSD(anova1,conf.level=0.95) # Median of pairs are equal or not factorial stuff

par(mfrow=c(2,2))

houseNonNums = houseData[,unlist(lapply(houseData, nonNumeric))]

for(col in colnames(houseNonNums)){
  print(col)
  plot(houseNonNums[,col],houseData$SalePrice, xlab = col, ylab = "Sale price",cex.lab=1.5, main=col)
  abline(houseNonNums$SalePrice~lm(houseNonNums[,col]))
}


# Pairwise comparisons Should we do in all data? not only price ? E.g.  Condition & Quality? 
# From cor plot we can see collinearity. E.g. total rooms above ground and living space above ground 

# What type of plot should we use for what variable?  And why -- Boxplots no need for smething else

# How would I judge if a factorial variable is important?

# Should I pick coefficients that are somewhat linear to price? Via Correlation Plots for instance? and remove the rest?
# Same with factorial variables?

# Start with Models
# Plan is to first do a LASSO to get a subset of Variables that matter and then go for step-wise processes
# Start with a model that includes everything 

# set.seed(123)
# split <- sample(nrow(houseData), size = floor(0.75 * nrow(houseData)))
# testData <- houseData[-split, ]
# houseData <- houseData[split, ]


model = lm(SalePrice ~ ., data = houseData)
summary(model)

# Prepare the Lasso

X = model.matrix(model)[,-1]
lasso = cv.glmnet(X, houseData$SalePrice,alpha = 1)
plot(lasso)

min = coef(lasso, s = "lambda.min") # How do I interpret this? 
lse = coef(lasso, s = "lambda.1se")
plot(lasso$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)

selected = min[min[,1]!=0,]

# Do we keep all these?

selectedNames = c(names(selected)[-1],"SalePrice")

collapsedNames = paste(selectedNames, collapse= " ")
newSelectedNames = sapply(colnames(houseData), existsIn, selectedNames, collapsedNames)
newSelectedNames = newSelectedNames[!sapply(newSelectedNames, is.null)]

lassoModel = lm(SalePrice ~ ., data = select(houseData,names(newSelectedNames)))
summary(lassoModel)

# Getting some NAs in some of the Factorial variables. This leads us to believe that the coefficient is not estimable. 
# Either we do not have many observations, or there could be exact Collinearity between them and Pricing.

# These values are not significant in our model
# NaValues = c("Bldg.Type", "Roof.Style", "Bsmt.Cond","BsmtFin.Type.1")
# 
# 
# postRemoveNames = sapply(newSelectedNames, notExistsIn, NaValues, paste(NaValues, collapse = " "))
# postRemoveNames = postRemoveNames[!sapply(postRemoveNames, is.null)]
# 
# lassoModel2 = lm(SalePrice ~ . , data = select(houseData,names(postRemoveNames)))
# 
# summary(lassoModel2)

# Prepare for backwards AIC
selectedLasso = c(names(lassoModel$coefficients), "SalePrice")
collapsedLassoNames = paste(selectedLasso, collapse= " ")
newLassoSelectedNames = sapply(colnames(houseData), existsIn, selectedLasso, collapsedLassoNames)
newLassoSelectedNames = newLassoSelectedNames[!sapply(newLassoSelectedNames, is.null)]

aicBaseModel = lm(SalePrice ~ ., data = select(houseData,names(newLassoSelectedNames)))
aicModel = step(aicBaseModel, direction='both')
summary(aicModel)

plot(aicModel, which = 2)
shapiro.test(rstandard(aicModel)) 
# p value very small -> Linearity rejected

# Do Collinearity here

vif(aicModel) # -> Fails Misserably there are aliased coefficients in the model
round(vif(aicModel),1)

NaValues = c("Roof.Style","Mas.Vnr.Type", "Bsmt.Cond","BsmtFin.Type.1","Functional","Garage.Qual","Mo.Sold")

selectedNames = c(names(aicModel$coefficients),"SalePrice")

cleanAIC = getCleanLasso(aicModel, NaValues, houseData)

aicModel = lm(SalePrice ~ . , data = select(houseData,names(newSelectedNames)))
summary(aicModel)

assumptionsTests(aicModel) # Everything Rejected

# Attempt non linear transformations 
# Exponential model
selectedNames = c(names(cleanAIC$coefficients),"SalePrice")

baseDataExpo = getCleanModelColumns(cleanAIC, houseData)
str(baseDataExpo)
baseDataExpo$SalePrice = log(houseData$SalePrice + 1 )

expoModel = lm(log(SalePrice)~., data = baseDataExpo)
summary(expoModel)
assumptionsTests(expoModel)

# Quadratic addition

quadModel = lm(SalePrice ~ poly(Year.Built, 2) + poly(Full.Bath, 2) + poly(Bedroom.AbvGr, 3) + poly(Gr.Liv.Area, 3)
               + poly(Total.Bsmt.SF, 3) + Kitchen.Qual + Bsmt.Exposure + Bsmt.Qual + Overall.Cond , data = baseDataExpo)

assumptionsTests(quadModel) # Fixes some things more

# Quadratic Next values

quadModel2 = lm(log(SalePrice) ~ poly(baseDataExpo$Year.Built, 2) + poly(baseDataExpo$Gr.Liv.Area, 3)
                + poly(baseDataExpo$Total.Bsmt.SF, 3) + Overall.Cond + Neighborhood, data = baseDataExpo)

assumptionsTests(quadModel2) # Similar visual results with this simpler model

summary(quadModel2)

length(quadModel2$coefficients) > quadModel2$rank
length(quadModel$coefficients) > quadModel$rank

# Training 
# Define training control
set.seed(2021)
trainControl = trainControl(method = "cv", number = 10)
# Train the model

trainedModelBase = train(SalePrice ~1+Lot.Area+Land.Contour+Neighborhood+Overall.Qual+Year.Built+Year.Remod.Add+Mas.Vnr.Area+Exter.Qual+Bsmt.Exposure+BsmtFin.SF.1+Total.Bsmt.SF+Gr.Liv.Area+Kitchen.AbvGr+Kitchen.Qual+Fireplaces+Fireplace.Qu+Garage.Area, data = houseData, method = "lm",trControl = trainControl)
trainedExpoModel = train(log(SalePrice) ~., data = houseData, method = "lm",trControl = trainControl)
trainedQuadModel1 = train(SalePrice ~ poly(Year.Built, 2) + poly(Full.Bath, 2) + poly(Bedroom.AbvGr, 3) + poly(Gr.Liv.Area, 3)
                          + poly(BsmtFin.SF.1, 3) + Kitchen.Qual + Bsmt.Exposure + Bsmt.Qual + Overall.Cond , data = houseData, method = "lm",trControl = trainControl)
trainedQuadModel2 = train(log(SalePrice) ~ poly(Year.Built, 2) + poly(Gr.Liv.Area, 3) + poly(Total.Bsmt.SF, 3) + Overall.Cond , data = houseData, method = "lm",trControl = trainControl)

# Getting some collinearity issues, revisiting Model to remove Neihgbourhood as an estimator

# Summarize the results
print(trainedModelBase)
print(trainedExpoModel)
print(trainedQuadModel1)
print(trainedQuadModel2)
# RMSE & MAE << smaller

# LOOCV
# Define training control
trainControlLOOCV = trainControl(method = "LOOCV")
# Train the model
trainedModelLOOCV = train(SalePrice ~1+Lot.Area+Land.Contour+Neighborhood+Overall.Qual+Year.Built+Year.Remod.Add+
                           Mas.Vnr.Area+Exter.Qual+Bsmt.Exposure+BsmtFin.SF.1+Total.Bsmt.SF+Gr.Liv.Area+Kitchen.AbvGr+Kitchen.Qual+Fireplaces+Fireplace.Qu+Garage.Area, data = houseData, method = "lm",trControl = trainControlLOOCV)
trainedExpoModelLOOCV = train(log(SalePrice) ~., data = houseData, method = "lm",trControl = trainControlLOOCV)
trainedQuadModel1LOOCV = train(SalePrice ~ poly(Year.Built, 2) + poly(Full.Bath, 2) + poly(Bedroom.AbvGr, 3) + poly(Gr.Liv.Area, 3)
                               + poly(BsmtFin.SF.1, 3) + Kitchen.Qual + Bsmt.Exposure + Bsmt.Qual + Overall.Cond + Neighborhood, data = houseData, method = "lm",trControl = trainControlLOOCV)
trainedQuadModel2LOOCV = train(log(SalePrice) ~ poly(baseDataExpo$Year.Built, 2) + poly(baseDataExpo$Gr.Liv.Area, 3)
                               + poly(baseDataExpo$Total.Bsmt.SF, 3) + Overall.Cond + Neighborhood, data = houseData, method = "lm",trControl = trainControlLOOCV)
# Summarize the results
print(trainedModelLOOCV)
print(trainedExpoModelLOOCV)
print(trainedQuadModel1LOOCV)
print(trainedQuadModel2LOOCV)


testData = read.csv("ames_iowa_housing_test.csv", sep = ";")

testData$Lot.Frontage = as.numeric(testData$Lot.Frontage)
testData$Mas.Vnr.Area = as.numeric(testData$Mas.Vnr.Area)
testData$BsmtFin.SF.1 = as.numeric(testData$BsmtFin.SF.1)
testData$BsmtFin.SF.2 = as.numeric(testData$BsmtFin.SF.2)
testData$Bsmt.Unf.SF = as.numeric(testData$Bsmt.Unf.SF)
testData$Total.Bsmt.SF = as.numeric(testData$Total.Bsmt.SF)
testData$Garage.Yr.Blt = as.numeric(testData$Garage.Yr.Blt)
testData$Garage.Yr.Blt = as.numeric(testData$Garage.Yr.Blt)
index = sapply(testData, class) == "character"  
numIndex = sapply(testData, class) != "character"  
testData[,numIndex][is.na(testData[,numIndex])] = 0
testData[,index][is.na(testData[,index])] = "NotAvailable"
testData$MS.SubClass = as.factor(testData$MS.SubClass)
testData$Overall.Qual = as.factor(testData$Overall.Qual)
testData$Overall.Cond  = as.factor(testData$Overall.Cond )
testData[index] = lapply(testData[index], as.factor)

# Removing the row with the non existing training observation(s)
formatedTestData = subset(testData, Overall.Qual!=1)
formatedTestData = subset(formatedTestData, Roof.Matl!="ClyTile")
formatedTestData = subset(formatedTestData, Heating!="OthW")
formatedTestData = subset(formatedTestData, Heating.QC!="Po")
nrow(formatedTestData) 

# From this one we will get a RMSE 
predict(expoModel, newdata = formatedTestData)
quadPre = predict(quadModel, newdata = formatedTestData)
predict(quadModel2, newdata = formatedTestData)

rmse(actual, predicted)

# COmpare rmse with above models


# Note that, prediction interval relies strongly on the assumption that the residual errors are normally distributed
# with a constant variance. So, you should only use such intervals if you believe that the assumption is approximately met
# for the data at hand.



# ---------------------------------------------------------------------------------------------------------------------------------
# Playing more with Data <---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------


# Checking for Aliases
# ldVars = attributes(alias(aicModel)$Complete)$dimnames[[1]]
# aicBaseModel2 = lm(SalePrice ~ . -Roof.StyleShed -BsmtFin.Type.1NotAvailable, data = select(houseData,names(newLassoSelectedNames)))
# formulaNew = as.formula(paste(paste(deparse(aicModel),collapse = ""),paste(ldVars, collapse = "-"), sep = "-"))

# Hightest value Year.Built 

selectedLasso1 = c(names(lassoModel2$coefficients), "SalePrice")

newLassoSelectedNames1 = sapply(newLassoSelectedNames, notExistsIn, c("Year.Built"), "Year.Built")
newLassoSelectedNames1 = newLassoSelectedNames1[!sapply(newLassoSelectedNames1, is.null)]


aicBaseModel1 = lm(SalePrice ~ ., data = select(houseData,names(newLassoSelectedNames1)))
aicModel1 = step(aicBaseModel1, direction='both')

summary(aicModel1)

round(vif(aicModel1),2)

plot(aicModel1, which = 2)
shapiro.test(rstandard(aicModel1)) 

ncvTest(aicModel1)
# Computes a score test of the hypothesis of constant error variance against the alternative
# that the error variance changes with the level of the response (fitted values), or with a linear combination of predictors.

# small P value, meaning that the error variance is not constant, but changes based on predictors


# Constant variance
StudResiduals = rstudent(aicModel1)
yhat = fitted(aicModel1)
par(mfrow=c(1,2))
plot(yhat, StudResiduals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, StudResiduals^2)
abline(h=4, col=2, lty=2)
plot(aicModel1, which=3)


# Non linearity

residualPlot(aicModel1, type='rstudent')
residualPlots(aicModel1, plot=F, type = "rstudent")

# Independence 
plot(rstudent(aicModel1), type='l')
runs.test(aicModel1$res) # p > 0.5 we go for non Randomness
dwtest(aicModel1) # # p > 0.5 True auto Correlation > 0
durbinWatsonTest(aicModel1)

# No value > 3.14, so we decide we keep moving with these variables

# Explore

plot(lm(SalePrice~Lot.Area,data=houseData),2, main='Price')
plot(lm(log(SalePrice)~Lot.Area,data=houseData),2, main='log of price')

# Observing here that log on both numeric vars, gives us a smoother qq plot. So let's test the assumption that log on bigger numberrs
# will give us a nice result
# Adding logs to "high" value params

logHouseData = houseData
# Power transformation
# Before loging.. since 0 will give us -inf, we need to remove these 0s. In square feet, 1 or 2 extra feet make little to no difference
# so let's add 1 to these, just to avoid ending up with 0 everywhere

logHouseData$SalePrice = log(houseData$SalePrice + 1)
logHouseData$Lot.Frontage = log(houseData$Lot.Frontage + 1)
logHouseData$Lot.Area = log(houseData$Lot.Area + 1)
logHouseData$Mas.Vnr.Area = log(houseData$Mas.Vnr.Area + 1)
logHouseData$BsmtFin.SF.1 = log(houseData$BsmtFin.SF.1 + 1)
logHouseData$BsmtFin.SF.2 = log(houseData$BsmtFin.SF.2 + 1)
logHouseData$Bsmt.Unf.SF = log(houseData$Bsmt.Unf.SF + 1)
logHouseData$Total.Bsmt.SF = log(houseData$Total.Bsmt.SF + 1)
logHouseData$Low.Qual.Fin.SF = log(houseData$Low.Qual.Fin.SF + 1)
logHouseData$Gr.Liv.Area = log(houseData$Gr.Liv.Area + 1)
logHouseData$X1st.Flr.SF = log(houseData$X1st.Flr.SF + 1)
logHouseData$X2nd.Flr.SF = log(houseData$X2nd.Flr.SF + 1)
logHouseData$Garage.Area = log(houseData$Garage.Area + 1)
logHouseData$Wood.Deck.SF = log(houseData$Wood.Deck.SF + 1)
logHouseData$Open.Porch.SF = log(houseData$Open.Porch.SF + 1)
logHouseData$Enclosed.Porch = log(houseData$Enclosed.Porch + 1)
logHouseData$X3Ssn.Porch = log(houseData$X3Ssn.Porch + 1)
logHouseData$Screen.Porch = log(houseData$Screen.Porch + 1)
logHouseData$Screen.Porch = log(houseData$Screen.Porch + 1)
logHouseData$Pool.Area = log(houseData$Pool.Area + 1)
logHouseData$Misc.Val = log(houseData$Misc.Val + 1)

str(logHouseData)
par(mfrow=c(1,1))

lassoModel2 = getLasso(logHouseData)
names(lassoModel2$coefficients)

summary(lassoModel2)

NaValues = c( "Roof.Style", "BsmtFin.Type.1", "Bldg.Type", "Bsmt.Cond", "Garage.Cond", "Garage.Qual")
cleanLassoModel2 = getCleanLasso(lassoModel2,NaValues, logHouseData)

summary(cleanLassoModel2)

aicModel2 = getAIC(cleanLassoModel2, logHouseData)
summary(aicModel2)

round(vif(aicModel2),2) # We get collinearity high on Garage cars

cleanAicModel2Names = getCleanNames(aicModel2, c("Garage.Area"), logHouseData)
aicModel2 = lm(SalePrice ~ ., data = select(logHouseData,names(cleanAicModel2Names)))
round(vif(aicModel2),2)

# Here I get another VIF value high, do I continue? 

assumptionsTests(aicModel2)
# here it seems like we have fixed normality 

anova(aicModel, aicModel2) # Very small p-value indicating model2 is better

# Next POLY & Logs
polyAndLogHouseData = logHouseData
str(polyOnLogHouseData)

# Here let us try to give more value to covariates that are important in our model, and see how it affects it

polyAndLogHouseData$Year.Built =  poly(logHouseData$Year.Built, 2)
polyAndLogHouseData$Year.Remod.Add =  poly(logHouseData$Year.Remod.Add, 2)
polyAndLogHouseData$Bsmt.Full.Bath = poly(logHouseData$Bsmt.Full.Bath,2)
polyAndLogHouseData$Full.Bath = poly(logHouseData$Full.Bath,2)
polyAndLogHouseData$Fireplaces = poly(logHouseData$Fireplaces,2)
polyAndLogHouseData$Kitchen.AbvGr = poly(logHouseData$Kitchen.AbvGr,2)
polyAndLogHouseData$Garage.Cars = poly(logHouseData$Garage.Cars,2)

par(mfrow=c(1,1))

lassoModel3 = getLasso(polyAndLogHouseData)
names(lassoModel3$coefficients)

summary(lassoModel3)

NaValues = c( "Roof.Style", "BsmtFin.Type.1", "Bldg.Type", "Bsmt.Cond", "Garage.Cond", "Garage.Qual")
cleanLassoModel3 = getCleanLasso(lassoModel3,NaValues, polyAndLogHouseData)

summary(cleanLassoModel3)

aicModel3 = getAIC(cleanLassoModel3, polyAndLogHouseData)
summary(aicModel3)

round(vif(aicModel3),2) # We get collinearity high on Garage cars

cleanAicModel3Names = getCleanNames(aicModel3, c("Pool.Area"), polyAndLogHouseData)
aicModel3 = lm(SalePrice ~ ., data = select(polyAndLogHouseData,names(cleanAicModel3Names)))
round(vif(aicModel3),2)

# Here I get another VIF value high, do I continue? 
cleanAicModel3Names = getCleanNames(aicModel3, c("Garage.Area"), polyAndLogHouseData)
aicModel3 = lm(SalePrice ~ ., data = select(polyAndLogHouseData,names(cleanAicModel3Names)))
round(vif(aicModel3),2)

assumptionsTests(aicModel3)
assumptionsTests(aicModel2)
# here it seems like we have fixed normality 

anova(aicModel1,aicModel2, aicModel3) # Very large p-value indicating model2 is better

# Trying poly on all the data (focusing on numerics)
polyHouseDataNums = houseData[,numIndex]
str(polyHouseDataNums)

apply(polyHouseDataNums, 2, poly, 2)

lassoModel4 = getLasso(polyHouseDataNums)
names(lassoModel4$coefficients)

aicModel4 = getAIC(lassoModel4, polyHouseDataNums)
summary(aicModel4)

assumptionsTests(aicModel4)

# Attempt quadratic (focusing on numerics)

par(mfrow=c(1,1))

quadHouseDataNums = houseData[,numIndex]

str(quadHouseDataNums)

quadHouseDataNums$Order2=quadHouseDataNums$Order^2
quadHouseDataNums$PID2=quadHouseDataNums$PID^2
quadHouseDataNums$Lot.Frontage2=quadHouseDataNums$Lot.Frontage^2
quadHouseDataNums$Lot.Area2=quadHouseDataNums$Lot.Area^2
quadHouseDataNums$Year.Built2=quadHouseDataNums$Year.Built^2
quadHouseDataNums$Year.Remod.Add2=quadHouseDataNums$Year.Remod.Add^2
quadHouseDataNums$Mas.Vnr.Area2=quadHouseDataNums$Mas.Vnr.Area^2
quadHouseDataNums$BsmtFin.SF.12=quadHouseDataNums$BsmtFin.SF.1^2
quadHouseDataNums$BsmtFin.SF.22=quadHouseDataNums$BsmtFin.SF.2^2
quadHouseDataNums$Bsmt.Unf.SF2=quadHouseDataNums$Bsmt.Unf.SF^2
quadHouseDataNums$Total.Bsmt.SF2=quadHouseDataNums$Total.Bsmt.SF^2
quadHouseDataNums$X1st.Flr.SF2=quadHouseDataNums$X1st.Flr.SF^2
quadHouseDataNums$X2nd.Flr.SF2=quadHouseDataNums$X2nd.Flr.SF^2
quadHouseDataNums$Low.Qual.Fin.SF2=quadHouseDataNums$Low.Qual.Fin.SF^2
quadHouseDataNums$Gr.Liv.Area2=quadHouseDataNums$Gr.Liv.Area^2
quadHouseDataNums$Bsmt.Full.Bath2=quadHouseDataNums$Bsmt.Full.Bath^2
quadHouseDataNums$Bsmt.Half.Bath2=quadHouseDataNums$Bsmt.Half.Bath^2
quadHouseDataNums$Full.Bath2=quadHouseDataNums$Full.Bath^2
quadHouseDataNums$Half.Bath2=quadHouseDataNums$Half.Bath^2
quadHouseDataNums$Bedroom.AbvGr2=quadHouseDataNums$Bedroom.AbvGr^2
quadHouseDataNums$Kitchen.AbvGr2=quadHouseDataNums$Kitchen.AbvGr^2
quadHouseDataNums$TotRms.AbvGrd2=quadHouseDataNums$TotRms.AbvGrd^2
quadHouseDataNums$Fireplaces2=quadHouseDataNums$Fireplaces^2
quadHouseDataNums$Garage.Yr.Blt2=quadHouseDataNums$Garage.Yr.Blt^2
quadHouseDataNums$Garage.Cars2=quadHouseDataNums$Garage.Cars^2
quadHouseDataNums$Garage.Area2=quadHouseDataNums$Garage.Area^2
quadHouseDataNums$Wood.Deck.SF2=quadHouseDataNums$Wood.Deck.SF^2
quadHouseDataNums$Open.Porch.SF2=quadHouseDataNums$Open.Porch.SF^2
quadHouseDataNums$Enclosed.Porch2=quadHouseDataNums$Enclosed.Porch^2
quadHouseDataNums$X3Ssn.Porch2=quadHouseDataNums$X3Ssn.Porch^2
quadHouseDataNums$Screen.Porch2=quadHouseDataNums$Screen.Porch^2
quadHouseDataNums$Pool.Area2=quadHouseDataNums$Pool.Area^2
quadHouseDataNums$Misc.Val2=quadHouseDataNums$Misc.Val^2
quadHouseDataNums$Mo.Sold2=quadHouseDataNums$Mo.Sold^2
quadHouseDataNums$Yr.Sold2=quadHouseDataNums$Yr.Sold^2
quadHouseDataNums$SalePrice2 = quadHouseDataNums$SalePrice^2


quadModel = lm(SalePrice ~ ., data = quadHouseDataNums)

aicModel5 = getAIC(quadModel, quadHouseDataNums)
summary(aicModel5)

assumptionsTests(aicModel5)

# Logarithmic model
logHouseDataNums = houseData
prevSales = logHouseDataNums$SalePrice

logHouseDataNums = data.frame(lapply(logHouseDataNums, function(x) { if(is.numeric(x)){return(log(x+1))}else{return(x)}}))
logHouseDataNums$SalePrice = prevSales
str(logHouseDataNums)
par(mfrow=c(1,1))

lassoModel6 = getLasso(logHouseDataNums)
names(lassoModel6$coefficients)

summary(lassoModel6)

NaValues = c( "Roof.Style", "BsmtFin.Type.1", "Bldg.Type", "Bsmt.Cond", "Garage.Cond", "Garage.Qual", "Garage.Finish")
cleanLassoModel6 = getCleanLasso(lassoModel6,NaValues, logHouseDataNums)

summary(cleanLassoModel6)

aicModel6 = getAIC(cleanLassoModel6, select(logHouseDataNums,names(cleanAicModel6Names)))
summary(aicModel6)

round(vif(aicModel6),2) # We get collinearity high on Garage cars


anova(aicModel2, aicModel6)

# Random Poly model
randPolyData = houseData
prevSales = houseData$SalePrice

# randPolyData = data.frame(apply(randPolyData, 2, randPoly))
for(col in colnames(randPolyData)){
  randPolyData[col] = randPoly(randPolyData[,col])
}

randPolyData$SalePrice = prevSales
str(randPolyData)
par(mfrow=c(1,1))

lassoModel7 = getLasso(randPolyData)
names(lassoModel7$coefficients)

summary(lassoModel7)

NaValues = c( "Roof.Style", "BsmtFin.Type.1", "Bldg.Type", "Bsmt.Cond", "Garage.Qual", "Garage.Qual", "Total.Bsmt.SF", "Garage.Finish")
cleanLassoModel7 = getCleanLasso(lassoModel7,NaValues, randPolyData)

summary(cleanLassoModel7)

aicModel7 = getAIC(cleanLassoModel7, randPolyData)
summary(aicModel7)

round(vif(aicModel7),2) # We get collinearity high on Garage cars

assumptionsTests(aicModel7)
anova(aicModel2, aicModel7)
summary(aicModel2)

# New tests
plot(SalePrice~Lot.Frontage)
mixModel = lm(log(SalePrice) ~ . + poly(Lot.Frontage, 5) + poly(Lot.Area, 5) + poly(BsmtFin.SF.1, 5) + poly(BsmtFin.SF.2, 5) + poly(Total.Bsmt.SF, 5)
              + poly(X1st.Flr.SF, 5) + poly(X2nd.Flr.SF, 5) + poly(BsmtFin.SF.2, 5) + poly(Gr.Liv.Area, 5) + poly(Full.Bath, 2) + poly(Bedroom.AbvGr , 2)
              + poly(Kitchen.AbvGr, 2) + poly(Fireplaces, 2) + poly(Full.Bath, 2) + poly(Garage.Cars, 2) + poly(Pool.Area, 2), data=houseData)

summary(mixModel)

NaValues = c( "Roof.Style", "BsmtFin.Type.1", "Bldg.Type", "Bsmt.Cond", "Garage.Qual", "Garage.Qual", "Total.Bsmt.SF", "Garage.Finish")
cleanLassoModel7 = getCleanLasso(lassoModel7,NaValues, randPolyData)

lassoModel8 = getLasso(randPolyData,mixModel)
summary(lassoModel8)

# Box Cox Transformation Try
newHouseData = houseData
boxModel = BoxCoxTrans(houseData$SalePrice)
newHouseData = cbind(houseData, dist_new=predict(boxModel, houseData$SalePrice))

boxModel = lm(SalePrice ~ ., data = newHouseData)
summary(boxModel)

assumptionsTests(boxModel)

plot(lm(SalePrice~., data=houseData),2)
