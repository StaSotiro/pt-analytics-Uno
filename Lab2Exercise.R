
setwd('/Users/stavros/Documents/BI/Statistics I');
usdata <- read.csv("usdata", sep = " ")

str(usdata)

# First variables seem to be set correctly as they are numeric. NE & Cor are set as numeric, but do not seem to be so, since we have 
# factorial meaning

# 2 
usdata$NE = factor(usdata$NE)
usdata$COR = factor(usdata$COR)

# 3
summary(usdata)

# Visual analysis
par(mfrow=c(2,2));n=nrow(usdata)

hist(usdata$PRICE, main="Price")
hist(usdata$SQFT, main="SQFT")
plot(table(usdata$AGE)/n, type='h',xlim=range(usdata$AGE)+c(-1,1), main="Age", ylab='Relative frequency')
plot(table(usdata$FEATS)/n, type='h',xlim=range(usdata$FEATS)+c(-1,1), main="Feats", ylab='Relative frequency')

# non numeric 

par(mfrow=c(1,1))
barplot(sapply(usdata[, c(5,6)],table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=1.3)
legend('top', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.5)


# So first view helps us understand we are dealing with mostly "middle class" houses that are not too big, have mostly 3 or 4 main features covered and are relatively older.
# That would explain possibly the price being mostly around 1k.
# Also looks like most houses are inbetween others rather than having a more "open" view and are locaced NE

# 4

pairs(usdata)
require(corrplot)
corrplot(cor(usdata[c(1:4)]))

# Corrplot gives us a very helpful insight on some probable association between price and sqft. It could be something expected as bigger house usually means more $
# There also seems to be some correlation between pricing and house feats, and age playing small to no part in price (as expected age has nothing to do with features or sqft of a house)
# We can probably assume that Price and Sqft would be significant variables we should take in consideration when applying a model further down.
par(mfrow=c(1,5))
for(j in 2:6){
  plot(usdata[,j], usdata[,1], xlab=names(usdata)[j], ylab='Price',cex.lab=1.5)
  abline(lm(usdata[,1]~usdata[,j]))
}

# we can see here additionaly that the man of Yes on NE seems to be higher than the 3rd quantile of No, so it could be a relation between
# price and location of the house

par(mfrow=c(1,1))
boxplot(usdata[,1]~usdata$FEATS, xlab="FEATS", ylab='Price',cex.lab=1.5)
abline(lm(usdata[,1]~usdata$FEATS),col=2)

# From this plot, it's easier to see there could be a trend in price & Feats as well

# Does it differ from doing what was done in the first one?
  
# par(mfrow=c(1,2))
# for(j in 5:6){
#   boxplot(usdata[,1]~usdata[,j], xlab=names(usdata)[j], ylab='Price',cex.lab=2.0)
# }

par(mfrow = c(1,1))
corrplot(cor(usdata[,c(1:4)]), method = "number") 

# 5

model = lm(PRICE ~ ., data = usdata)
summary(model)

# Looks like the most important variants are SQFT, which lines with our original expectations from the graphical interpretation as a significant variable
# We also notice an R2 of 86.4% as a goodness of fit measure that the variance of price is explained well from these selected covariants
# Our Intercept is our base value of getting that house, so the expected amount to be paid and our price would be modeled as 
# Price = -193 + SQFT * 0.67 + AGE * 2.2 + FEAT * 34 + 30 if in NE - 53 if CORner + e
# where e = N(0, 144.8) the e comes from the residual standard error (144.8) ouput


# 6 & 7

finalModel = step(model, direction='both')

summary(finalModel)

# Stepwise AIC gives us our final model ( interpretation similar to above ) and gives us the estimated pricing to be
# Price = -175 + SQFT * 0.68 + FEAT * 40  + e
# Simply put, our estimated price is a measure of how big the house is and how many features it has (which makes total sense)
# where e = N(0, 143.7) the e comes from the residual standard error (143.7) output
# Our R-squared here is 87% which, even if it's not to be relying solely on to compare two models we get an idea we are going to the right direction

# 8

plot(finalModel, which = 2) #Normality of the residuals
shapiro.test(rstandard(finalModel))

#Running shapiro we check normality of our final model, where we get a value >5%, not rejecting the Null hypothesis of linearity

#Costant variance 
Stud.residuals = rstudent(finalModel)
yhat = fitted(finalModel)
par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

par(mfrow=c(1,1))
ncvTest(finalModel)
# very small p value 
# ------------------
yhat.quantiles=cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(finalModel)~yhat.quantiles)
# Levene's test ends up with very small p value, showing there is difference between the variances in population
boxplot(rstudent(finalModel)~yhat.quantiles)

# ------------------
# non linearity
# ------------------

library(car)
residualPlot(finalModel, type='rstudent')
residualPlots(finalModel, plot=F, type = "rstudent")

# Independence of errors

plot(rstudent(finalModel), type='l')

# 9
require(glmnet)
X = model.matrix(model)[,-1]
lasso = cv.glmnet(X, usdata$PRICE,alpha = 1)
plot(lasso)
lasso$lambda
lasso$lambda.min # Minimal error using that L
lasso$lambda.1se # Second L. So you get a simpler model (less vars) that should also be not lose in accuracy compared to min. 
# If it was a very large deviation, we wouldn't choose 
coef(lasso, s = "lambda.min")
coef(lasso, s = "lambda.1se")
plot(lasso$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)

# Looks like we are getting the same variables after conducting LASSO if we choose to go wit the simple lse model of 2 variables.
# Looking at the graph we do not notice a large deviation that would prompt us to avoid using lse as our model
