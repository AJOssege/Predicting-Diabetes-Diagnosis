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


## Check the Multicollinearity
result<-lm(glyhb~.,data)
summary(result)

# Find the best values for the model selection
allreg <- regsubsets(glyhb ~., data=data, nbest=9)

best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2),]    # large r2 is better 
best[order(best$adjr2),] # large adj r2 is better 
best[order(best$mse),]   # small mse is better
best[order(best$cp),]    # small cp is better
best[order(best$bic),]   # small bic is better



##intercept only model
regnull <- lm(glyhb~1, data=data)
##model with all predictors
regfull <- lm(glyhb~., data=data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


## Finding the R-square and adj R-square values with the suggested Models
result<-lm(glyhb~chol+stab.glu+chol+age+location+hasDiabetes,data=data)
summary(result)

##############################################################################
## Without hasDiabetes variable in the model
##############################################################################
# Excluding hasDiabetes variable from the dataset
no_hasdiabetes<-data[,-13]


## Check the Multicollinearity
result<-lm(glyhb~.,no_hasdiabetes)
summary(result)


# Find the best values for the model selection
allreg <- regsubsets(glyhb ~., data=no_hasdiabetes, nbest=9)

best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2),]    # large r2 is better 
best[order(best$adjr2),] # large adj r2 is better 
best[order(best$mse),]   # small mse is better
best[order(best$cp),]    # small cp is better
best[order(best$bic),]   # small bic is better

##intercept only model
regnull <- lm(glyhb~1, data=no_hasdiabetes)
##model with all predictors
regfull <- lm(glyhb~., data=no_hasdiabetes)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")


## Finding the R-square and adj R-square values with the suggested Models
result<-lm(glyhb~chol+stab.glu+age+location+time.ppn+hdl,data=no_hasdiabetes)
summary(result)

## Initial Model with predictors found with above methods
result<-lm(glyhb~chol+stab.glu+hdl+location+age+bmiCat+time.ppn+waist+bmi,data=data)
summary(result)