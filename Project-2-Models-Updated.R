## Set the current directory to Project-2
setwd("C:/SP-PERSONAL/CollegeWork-UVA-MSDS/STAT-6021/Project-2/")
getwd()


## Load the required libraries
library(faraway) 
library(leaps)

## Diabetes data set after cleaning the unnecessary and removing the blank values.
data = read.csv("diabetes_data_sub.csv")

## Data variables.
##[1] "chol"        "stab.glu"    "hdl"         "glyhb"       "location"    "age"        
##[7] "gender"      "bp.1s"       "bp.1d"       "waist"       "hip"         "time.ppn"   
##[13] "hasDiabetes" "bmi"         "bmiCat"     


# Check and make them Categorical predictors

is.factor(data$location)
is.factor(data$gender)
is.factor(data$bmiCat)
is.factor(data$hasDiabetes)
data$hasDiabetes<-factor(data$hasDiabetes)
is.factor(data$hasDiabetes)

# Check the Indicator variables
levels(data$location)
levels(data$gender)
levels(data$bmiCat)
levels(data$hasDiabetes)

## Check the Dummy variables set for the Categorical vaiables
contrasts(data$location)
contrasts(data$gender)
contrasts(data$bmiCat)
contrasts(data$hasDiabetes)


##############################################################################
## Without hasDiabetes variable in the model
##############################################################################
# Excluding hasDiabetes variable from the dataset
no_hasdiabetes<-data[,-13]


## Check the Regression Model with all predictors
result<-lm(glyhb~.,no_hasdiabetes)
summary(result)


## Check the best R-square, adj R-square, CP, BIC values using
## Regression Subset selection methods

allreg <- regsubsets(glyhb ~., data=no_hasdiabetes, nbest=9)


## Store the values in a data frame for easy access and view
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic


##sort the data frame values by each value to find the corresponding predictor variables.

best[order(best$r2),]    # large R^2 is better 
best[order(best$adjr2),] # large adj R^2 is better 
best[order(best$mse),]   # small MSE is better
best[order(best$cp),]    # small CP is better
best[order(best$bic),]   # small BIC is better

## Using Stepwise Algorithm to choose a model by AIC value

##intercept only model
regnull <- lm(glyhb~1, data=no_hasdiabetes)
##model with all predictors
regfull <- lm(glyhb~., data=no_hasdiabetes)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


## Finding the R-square and adj R-square values with the suggested Models
## BASE Model based on the Stepwise AIC value and Regression Subset selection values
result<-lm(glyhb~chol+stab.glu+hdl+age+location+time.ppn+bmi,data=no_hasdiabetes)
summary(result)


#####################################
## HANDLING OUTLIERS 
####################################

## Residuals
res<-result$residuals

## Studentized Residuals
student.res<-rstandard(result)

## Externally Studentized Residuals
ext.student.res<-rstudent(result)


par(mfrow=c(1,3))
plot(result$fitted.values,res,main="Residuals")
plot(result$fitted.values,student.res,main="Studentized Residuals")
plot(result$fitted.values,ext.student.res,main="Externally  Studentized Residuals")

dim(no_hasdiabetes)[1] # Rows
dim(no_hasdiabetes)[2] # Columns


#n<-length(data) # Count in the data
n<- dim(no_hasdiabetes)[1] # Rows
p<-8 # Number of parameters (7 predicotr + 1 intercept)

##critical value using Bonferroni procedure
# Computing critical value
qt(1-0.05/(2*n), n-p-1)
#3.8613

# To see if any of the sorted residuals are larger than the critical value computed above
sort(ext.student.res) 

## Draw lines above and below to see if any outliers beyond these limits
plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-4,4))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

# other way to detect the outliers with the condition statement
# it returned nothing. that means no outliers.
ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

## To find / detect leverage points
##leverages
lev<-lm.influence(result)$hat 

sort(lev)
(2*p)/n  #  0.04266667

plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")



# Find the leveage point 
lev[lev>2*p/n]

##influential Data points / observations

## DFFITS measures how drastially the fitted value changes with and without the prsence of the data points
## The DFFITS values tells how much the corresponding Fitted values changes for the data point
DFFITS<-dffits(result)
DFFITS[abs(DFFITS)>2*sqrt(p/n)]

## DFBEATAS tells how much the coefficient changes
DFBETAS<-dfbetas(result)
DFBETAS[abs(DFBETAS)>2/sqrt(n)]

## COOK Distance tells how much the fitted value changes for all data points
## rather than for a particular point like ohters.
COOKS<-cooks.distance(result)
COOKS[COOKS>qf(0.5,p,n-p)]


#########################################################################################

## BASE Model
result<-lm(glyhb~chol+stab.glu+hdl+age+location+time.ppn+bmi,data=no_hasdiabetes)
summary(result)


###############################################
##
no_highlevs<-no_hasdiabetes[c(-4,-16,-21,-30,-57,-62,-88,-91,-121,-135,-143,-160,-180,-188,-197,-213,-233,-274,-341,-344,-357,-371,-372,-310,-338),]

## Model Summary With Outliers
result1<-lm(glyhb~chol+stab.glu+hdl+age+location+time.ppn+bmi,data=no_hasdiabetes)
summary(result1)

## Model Summary Without Outliers
result2<-lm(glyhb~chol+stab.glu+hdl+age+location+time.ppn+bmi,data=no_highlevs)
summary(result2)
