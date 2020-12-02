require(foreign)
require(psych)
setwd('/Users/stavros/Documents/BI/Statistics I/lab assignment 2');
salary <- read.spss("salary.sav", to.data.frame = T)

#1 
str(salary)

salary = salary[, -1]
# Viewing the variables, we check that it consists of numeric and factorial variables. We do not need to perform any action on transforming
# data from a string value to factorial, for instance. One thing we could remove is the "id" field, since it does not offer any value in any analysis

# 2

index <- sapply(salary, class) == "numeric"
sal_num <- salary[index]
summary(sal_num)
par(mfrow=c(2,3))
apply(sal_num, 2, hist)

# Getting the summary initially we get that no field is following a normal distribution, especially in the salaries we see a lot of outliers (assumption: executives, managers?)

# 3 
# we check the H0: starting salary == 1000 vs H1 starting salary != 1000
# This is a continous variable, where we can not assume normality, have a large number of sample, and checking for symmetry we result
# Mean is not the best for describing central distribution. Histograms show also a skew. So We would test using median, going for 
# Wilcoxon test. The resulting p-value is exteremely small, which results in us rejecting H0 so starting salary is not equal to 1000

salbeg = salary$salbeg
library('nortest')
lillie.test(salbeg) #Normality rejected
shapiro.test(salbeg) #Normality rejected
length(salbeg)

mean(salbeg) 
median(salbeg)
wilcox.test(salbeg, mu=1000)

# 4

# In this case we need to check for two dependent variables of this large sample. 
# Rejected normality of a big sample, so we need to check if mean is a discriptive measure. Not getting a reasonably close diff between mean and median
# Continuing with a Wilcox test, getting a very small p-value, showing us that we employees salaries did change

saldiff = salary$salnow - salary$salbeg
lillie.test(saldiff) #Normality rejected
shapiro.test(saldiff) #Normality rejected

mean(saldiff) 
median(saldiff)
# wilcox.test(salary$salnow, salary$salbeg, paired = T)
wilcox.test(saldiff, mu=0)

# 5
# Here we would check the association between a continous and a categorical var.
# We do not follow a normal distr. but have a large sample size. Proceeding in checking the mean and medians, as well as skewness and kyrtosis
# Resulting in mean not being sufficient for describing central location for Both groups. This leads us to testing for zero difference
# between medians using Wilcox test which ends up with a very small p-value leading to rejecting the Null hypothesis of having 
# equal salaries 

groupA = salary[salary$sex=="MALES", 1]
groupB = salary[salary$sex=="FEMALES", 1]

dataset1 = data.frame( salary=c(groupA, groupB),  sex=factor( rep(1:2, c(length(groupA),length(groupB))), labels=c('M','F') ) ) 

#Test for normality for each group 
by(dataset1$salary, dataset1$sex, lillie.test) #Normality rejected
by(dataset1$salary, dataset1$sex, shapiro.test) #Normality rejected

# Check for mean and median for both 
mean(groupA); mean(groupB)
median(groupA); median(groupB)
skew(groupA); skew(groupB)
kurtosi(groupA);kurtosi(groupB)
par(mfrow=c(1,2))
hist(groupA)
hist(groupB)
symmetry.test(groupA)
symmetry.test(groupB)

wilcox.test(groupA, groupB)
boxplot(groupA, groupB, names=c("Males","Femaies"))

# 6

# Here we want to check the variance of multiple samples, with a numeric and a factorial variable. The initial hypothesis being that
# the variances of the different groups are the same. By performing ANOVA on our dataframe, we result in rejecting that initial hypothesis.
# Meaning there are some means differ within these  groups. Tests show us we can't assume normality, also shown from the QQ plot visually
# as most values are concentrated in the first quantile. Checking the means result in not being a great descriptive measure, which leads
# to testing for equality of medians using KW-test. From there along with the box plots we see the great difference in the middle group
# This could make sense, since older (currently) employees had a starting salary when they joined a few years back, and have not enchanced 
# their skills, along with many outliers that have great starting salary, which would be, probably, executives and managers joining the company
# for mid group, which is people with good experience in the field, they get mostly salaries that reflects their experience and knowledge edge
# Positioning could be on a leading role. As for younger employees, their starting salary is relatively low, since they do not have the experience, could be trainees, etc.

require("Hmisc")
par(mfrow=c(1,1))
age_cut = cut2(salary$age, g=3)

ageDF = data.frame(salbeg= salary$salbeg, agegrp = age_cut)

ageDF = ageDF[order(ageDF$agegrp),]

anova1 <- aov(salary$salbeg~age_cut, data=ageDF )
anova2 <- oneway.test( salary$salbeg~age_cut, data=ageDF )
summary(anova1)
# summary(anova2)

anova1$coefficients

lillie.test(anova1$residuals)
shapiro.test(anova1$residuals)
qqnorm(anova1$residuals)
qqline(anova1$residuals)


mean(ageDF[ageDF$agegrp=='[23.0,29.7)',1])
median(ageDF[ageDF$agegrp=='[23.0,29.7)',1])
symmetry.test(ageDF[ageDF$agegrp=='[23.0,29.7)',1])

#mean(ageDF[ageDF$agegrp=='[29.7,39.8)',1])
#median(ageDF[ageDF$agegrp=='[29.7,39.8)',1])

#mean(ageDF[ageDF$agegrp=='[39.8,64.5]',1])
#median(ageDF[ageDF$agegrp=='[39.8,64.5]',1])

boxplot(salbeg~agegrp,data=ageDF)
boxres<-boxplot(salbeg~agegrp,data=ageDF, plot=F) 
out.index <- which( ageDF$salbeg %in% boxres$out )

pairwise.wilcox.test( ageDF$salbeg, ageDF$agegrp )

# 7 
# Both tests show independence. 

tbl = table(salary$sex, salary$minority)
round(100*prop.table(tbl, 1),1)

prop.test(tbl)
chisq.test(tbl)

