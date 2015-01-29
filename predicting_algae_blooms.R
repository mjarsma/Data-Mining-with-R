### Addressing the problem of predicting the frequency occurance of
### several harmful algae in water samples.

###################################################
### Loading the Data into R
###################################################
install.packages("DMwR")
install.packages("Hmisc")
library()

# returns the first parts of the algae-data frame
head(algae)

# header = F : the file doesn't include headers for columns
# na.strings : serves to indicate a vector of strings that are to be interpreted
# as unknown values.
# dec : numbers use '.' to indicate decimals
algae <- read.table('Analysis.txt', 
                    header=F, 
                    dec='.', 
                    col.names=c('season','size','speed','mxPH','mnO2','Cl', 
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3',
                                'a4','a5','a6','a7'),
                    na.strings=c('XXXXXXX'))


###################################################
### Data Visualization and Summarization
###################################################
summary(algae)

# histrogram of mxPH-variable
hist(algae$mxPH, prob=T)

library(car)
# dividing the graphics output to window into one line per two columns
par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab='',
     main='Histogram of maximum pH value',ylim=0:1)
# kernel density estimate of the distribution of the variable
lines(density(algae$mxPH,na.rm=T))
# the rug performs the plotting while the jitter is used to 
# randomly perturb slightly the original values to plot so that
# we almost eliminate the possibility of two values being equal,
# thus avoiding ticks over each other that would "hide" some
# values from the visual inspection.
rug(jitter(algae$mxPH))
# Q-Q plots the variable values against the theoretical quantiles
# of a normal distribution. The function also plots an envelope with
# the 95% confidence interval of the normal distribution. If the data
# is normally distributed, the points in the QQ-normal plot lie on
# a straight diagonal line.
qq.plot(algae$mxPH,main='Normal QQ plot of maximum pH')
par(mfrow=c(1,1))

# a box plot of the oPO4-variable:
# the box has a horizontal line inside of it that represents
# the median value of the variable.
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
rug(jitter(algae$oPO4),side=2)
# abline draws a horizontal line: lty = 2 obtains a dashed line
abline(h=mean(algae$oPO4,na.rm=T),lty=2)

# the first instruction plots all values of the variable.
# after that, three lines: one with mean value, one with mean + one
# standard deviation, and the 3rd with the median.
# the last instruction is interactive and allows the user to click
# on the plotted dots with the left mouse button.
plot(algae$NH4,xlab='')
abline(h=mean(algae$NH4,na.rm=T),lty=1)
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T),lty=3)
identify(algae$NH4)

# for inspecting the respective observations in the algae data frame,
# use the following snippet.
plot(algae$NH4,xlab='')
clicked.lines <- identify(algae$NH4)
algae[clicked.lines,]

# inspection without the graphics while rejecting observations that
# have "NA" for NH4-variable.
algae[!is.na(algae$NH4) & algae$NH4 > 19000,]

# lattice provides a large set of graphics tools
library(lattice)
# "plot a1 for each value of size"
bwplot(size ~ a1, data=algae,ylab='River Size',xlab='Algal A1')

library(Hmisc)
# dots are the mean value of the frequency of the algal for the
# different river sizes.
# the vertical lines represent the 1st quantile, median, and 3rd quantile.
bwplot(size ~ a1, data=algae,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE,
       ylab='River Size',xlab='Algal A1')

# equal.count creates a factorized version of the continuous variable mn02.
# the number-parameter sets the number of desired bins, while the overlap-
# parameter sets the overlap between the bins near their respective boundaries
# 
minO2 <- equal.count(na.omit(algae$mnO2),
                     number=4,overlap=1/5)

# observing the behavior of the frequency of algal a3 conditioned by
# season and mn02 (continuous variable).
stripplot(season ~ a3|minO2,
          data=algae[!is.na(algae$mnO2),])


###################################################
### Unkwnon Values
###################################################

library(DMwR)
data(algae)

# cases with at least one missing variable
algae[!complete.cases(algae),]
# count of the incomplete cases
nrow(algae[!complete.cases(algae),])

# for removing the 16 cases with NA values from the data frame
algae <- na.omit(algae)

# for removing specific observations from the data frame
algae <- algae[-c(62,199),]

# return the number of unknown values in each row of the dataset.
# the apply-function is a meta-function and allows applying other
# functions to objects under certain conditions. in this case,
# the function will be applied to every row of the data frame.
apply(algae,1,function(x) sum(is.na(x)))

# the call to data is necessary because we previously deleted
# rows with NA values.
data(algae)
# the manyNAs-function returns the row numbers that contain more
# than 20% NA-values. the DMwR-package contains the manyNAs-function.
manyNAs(algae,0.2)

# filling an unknown value with the mean value of the dataset.
# na.rm=T disregards any NA value in the mxPH-vector.
algae[48,'mxPH'] <- mean(algae$mxPH,na.rm=T)

# using the median value to fulfill the missing NAs
algae[is.na(algae$Chla),'Chla'] <- median(algae$Chla,na.rm=T)

data(algae)
algae <- algae[-manyNAs(algae),]
# the function below comes along with the DMwR-package, and
# it uses the median for numeric columns and uses the most
# frequent value (the mode) for nominal variables.
algae <- centralImputation(algae)

# cor() produces a matrix with the correlation values between
# the variables. use="complete.obs" setting disregards observations
# with NA values.
cor(algae[,4:18],use="complete.obs")

# cor() isn't very legible but symnum() improves it. 
symnum(cor(algae[,4:18],use="complete.obs"))


data(algae)
algae <- algae[-manyNAs(algae),]
# lm() is used to obtain linear models of the form
# Y = b0+b1X1+...+bnXn.
lm(PO4 ~ oPO4,data=algae)

# the result of the lm() tells us that PO4 = 42.897 + 1.293*oPO4
algae[28,'PO4'] <- 42.897 + 1.293 * algae[28,'oPO4']


# for applying the linear model above to all unknown values of PO4
data(algae)
algae <- algae[-manyNAs(algae),]
fillPO4 <- function(oP) {
  if (is.na(oP)) return(NA)
  else return(42.897 + 1.293 * oP)
}
algae[is.na(algae$PO4),'PO4'] <- 
  sapply(algae[is.na(algae$PO4),'oPO4'],fillPO4)

# obtaining a histrogram of the values of mxPH for the different
# values of season.
histogram(~ mxPH | season,data=algae)

# order accordingly the seasons for the histogram
algae$season <- factor(algae$season,levels=c('spring','summer','autumn','winter'))


###################################################
### Multiple Linear Regression
###################################################

# Multiple linear regression obtains an additive function 
# relating a target variable to a set of predictor variables.

data(algae)
algae <- algae[-manyNAs(algae), ]

# knnImputation fills in all NA values using the
# k nearest neighbours and obtains a weighted average
# (eucledian distance) of their values to fill in 
# the unknowns. Thus, clean.algae has no missing
# variable values.
clean.algae <- knnImputation(algae, k = 10)

clean.algae

# the first argument of lm() indicates the functional
# form of the model: the model predicts the variable a1
# using all other variables present in the data, which
# is the meaning of the dot character. the data parameter
# sets the data sample to be used to obtain the model.
lm.a1 <- lm(a1 ~ .,data=clean.algae[,1:12])

# * Residuals: the residuals are the difference between
# the actual values of the variable you're predicting and
# predicted values from the regression y-y^. For most regressions,
# you want your residuals to look like a normal distribution
# when plotted. 
# Estimated coefficient: an increase of 1 unit in a1 with 
# an increase of e.g. 1.0526 in mn02.
# * Regression coefficients represent the mean change in 
# the response variable for one unit of change in the predictor 
# variable while holding other predictors in the model constant.
# The key to understanding the coefficients is to think of them 
# as slopes, and they’re often called slope coefficients.
# * t-value: score that measures whether or not the coefficient
# for this variable 
# * The p-value for each term tests the null hypothesis that 
# the coefficient is equal to zero (no effect). A low p-value 
# (< 0.05) indicates that you can reject the null hypothesis. 
# In other words, a predictor that has a low p-value is likely 
# to be a meaningful addition to your model because changes in 
# the predictor's value are related to changes in the response 
# variable.
# * Multiple R-squared: metric for evaluating the goodness of fit of 
# the model. Higher value is better with 1 being the best.
# The adjusted coefficient is more demanding as it takes into
# account the number of parameters of the regression model, as in
# it adds a penalty for adding variables to the model that are
# uncorrelated with the variable you're trying to explain.
# Corresponds with the amount of variability in what is
# being predicted with the model. Correlation does not always
# imply causality. Approximately 37% of variation in a1
# can be explained by all variables (the dot notation).
# * F-statistics and resulting p-value: the statistic of 
# the hypothesis test with null hypothesis. H0: All non-constant 
# coefficients in the regression equation are zero (there is no 
# dependence of the target variable on any of the explanatory 
# variables), and the alternate, Ha: at least one of the non-constant 
# coefficientsin the regression equation is non-zero. If the model has 
# more parameters, the F-test wil have a high p-value. If the model 
# with more parameters is better than the model with fewer parameters, 
# you will have a lower p-value. 
# * DF (degrees of freedom) pertains to how many variables are 
# in the model.
summary(lm.a1)

# The results indicate that the season-variable is the variable
# that least contributes to the reduction of the fitting error
# of the model.
anova(lm.a1)

# The following instruction removes the season-variable from
# the model:
lm2.a1 <- update(lm.a1, . ~ . - season)

summary(lm2.a1)

# a formal comparison of the two models by performing
# an F-test to assess the signinficance of the differences.
anova(lm.a1,lm2.a1)


final.lm <- step(lm.a1)


summary(final.lm)


###################################################
### Regression Trees
###################################################

library(DMwR)
library(rpart)
data(algae)
algae <- algae[-manyNAs(algae), ]
# rpart-instruction obtains the regression tree
rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])

# The regression tree is a hierarchy of logical tests
# on some of the explanatory variables.
rt.a1

prettyTree(rt.a1)
printcp(rt.a1)