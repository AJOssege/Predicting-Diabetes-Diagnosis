library(faraway)
library(vtree)
library(Hmisc)
library(psych) 
library(dplyr)

# David hasDiabetes, bmi, & bmiCat variable creation code
diabetes_data<- diabetes 
diabetes_data$hasDiabetes<-ifelse(diabetes_data$glyhb>=7.0,1,0) 
diabetes_data$bmi <- 703*diabetes_data$weight/(diabetes_data$height)^2 
diabetes_data$bmiCat <- ifelse(diabetes_data$bmi <18.5, "Underweight",
                               ifelse(diabetes_data$bmi <25, "Normal",
                                      ifelse(diabetes_data$bmi <30, "Overweight","Obese")))

# Lauren ageCat variable creation code
diabetes_data$ageCat <- ifelse(diabetes_data$age <45, "Younger",
                               ifelse(diabetes_data$bmi <65, "Mature", "Senior"))

# new df/.csv file with BMI stats included; sans NA values and variables: id, ratio, height, weight,frame, bp.2s, & bp.2d
diabetes_data_sub = subset(diabetes_data, select = -c(id, ratio, height, weight,
                                                      frame, bp.2s, bp.2d))

sum(is.na(diabetes_data_sub))
sum(complete.cases(diabetes_data_sub))
is.na(diabetes_data_sub)
diabetes_data_no_na<-na.omit(diabetes_data_sub)
write.csv(diabetes_data_no_na, "diabetes_data_no_na.csv")

# create variable trees to explore spread/split of hasDiabetes across ageCat, bmiCat, location, and gender (as a precursor to interaction effect analysis)                 
vtree(diabetes_data_no_na_2,"hasDiabetes ageCat",horiz=FALSE,height=250,width=850,showlevels=FALSE,title="hasDiabetes & ageCat", summary="glyhb \n\nglyhb\nmean=%mean%\nSD=%SD% %leafonly%")
vtree(diabetes_data_no_na_2,"hasDiabetes bmiCat",horiz=FALSE,height=250,width=850,showlevels=FALSE,title="hasDiabetes & bmiCat", summary="glyhb \n\nglyhb\nmean=%mean%\nSD=%SD% %leafonly%")
vtree(diabetes_data_no_na_2,"hasDiabetes bmiCat ageCat",horiz=FALSE,height=250,width=850,showlevels=FALSE,title="hasDiabetes & bmiCat", summary="glyhb \n\nglyhb\nmean=%mean%\nSD=%SD% %leafonly%")
vtree(diabetes_data_no_na_2,"hasDiabetes location gender",horiz=FALSE,height=250,width=850,showlevels=FALSE,title="hasDiabetes, location, & gender", summary="glyhb \n\nglyhb\nmean=%mean%\nSD=%SD% %leafonly%")

# new variable / subset for those with BMI categories putting them at higher risk of a positive diabetes diagnosis
# determines the average Glycosolated Hemoglobin for Overweight and Obese subjects                  
diabetes_risk_bmi <- subset(diabetes_data_no_na_2, bmiCat %in% c("Overweight","Obese"))
inference(y = diabetes_risk_bmi$glyhb, data = diabetes_data_no_na_2, statistic = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")  










############################################################
############################################################

set.seed(100)
data<-diabetes_data_no_na
sample<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]


############################################################
############################################################

data <- read.table("diabetes_data_no_na_2.csv", header=TRUE ,sep=",")

diabetes_data = subset(data, select = -c(X))
attach(diabetes_data)

diabetes_data$location <- factor(diabetes_data$location)
is.factor(diabetes_data$location)
contrasts(diabetes_data$location)

diabetes_data$gender <- factor(diabetes_data$gender)
is.factor(diabetes_data$gender)
levels(diabetes_data$gender)
contrasts(diabetes_data$gender)

diabetes_data$hasDiabetes <- factor(diabetes_data$hasDiabetes)
is.factor(diabetes_data$hasDiabetes)
levels(diabetes_data$hasDiabetes) <- c("No", "Yes") 
levels(diabetes_data$hasDiabetes)
contrasts(diabetes_data$hasDiabetes)


#################################################################
#################################################################

attach(diabetes_data_no_na)

result<-lm(glyhb~bmi+age+chol+stab.glu+hdl+bp.1d+bp.1s+time.ppn+waist+hip)
summary(result)

is.factor(gender)
is.factor(location)
is.factor(hasDiabetes)

bmiCat<-factor(bmiCat)
is.factor(bmiCat)
levels(bmiCat)
contrasts(bmiCat)


library(leaps)

##perform all possible regressions (1st order)
allreg <- regsubsets(glyhb~chol+stab.glu+hdl+bmi+age+bp.1d+bp.1s+time.ppn+waist+hip, data=diabetes_data_no_na, nbest=9)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

##sort by various criteria
best[order(best$r2),]
best[order(best$adjr2),]
best[order(best$mse),]
best[order(best$cp),]
best[order(best$bic),]

##intercept only model
regnull <- lm(glyhb~1, data=diabetes_data)
##model with all predictors
regfull <- lm(glyhb~chol+stab.glu+hdl+bmi+age+bp.1d+bp.1s+time.ppn+waist+hip, data=diabetes_data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

## what to do with the blood pressure measurements? (many/most missing second readings; keep just the first of each type?)
levels(location)
is.factor(location)
contrasts(location)


levels(gender)
is.factor(gender)
contrasts(gender)
## 0: MALE; 1: FEMALE

diabetes_data

boxplot(age~diabetes_data$hasDiabetes)
boxplot(diabetes_data$bmi~diabetes_data$hasDiabetes)



full<-glm(hasDiabetes ~ bmiCat+age+chol+stab.glu+hdl+bp.1d+bp.1s, family = "binomial")
summary(full)

