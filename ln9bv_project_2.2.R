library(faraway)
library(vtree)
library(Hmisc)
library(psych) 
library(dplyr)
library(statsr)

# David hasDiabetes, bmi, & bmiCat variable creation code
diabetes_data<- diabetes 
diabetes_data$hasDiabetes<-ifelse(diabetes_data$glyhb>=7.0,1,0) 
diabetes_data$bmi <- 703*diabetes_data$weight/(diabetes_data$height)^2 
diabetes_data$bmiCat <- ifelse(diabetes_data$bmi <18.5, "Underweight",
                               ifelse(diabetes_data$bmi <25, "Normal",
                                      ifelse(diabetes_data$bmi <30, "Overweight","Obese")))

# Lauren ageCat variable creation code
diabetes_data$ageCat <- ifelse(diabetes_data$age <45, "Younger",
                               ifelse(diabetes_data$age <65, "Mature", "Senior"))

# is there an observable/calculable impact on glyhb due to differences in the genders?

inference(y = diabetes_data$glyhb, x = diabetes_data$gender, data = diabetes_data, statistic = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")

# result: no need to separate data by gender
# Response variable: numerical
# Explanatory variable: categorical (2 levels) 
# n_male = 162, y_bar_male = 5.7241, s_male = 2.3878
# n_female = 228, y_bar_female = 5.4943, s_female = 2.1337
# H0: mu_male =  mu_female
# HA: mu_male != mu_female
# t = 0.9781, df = 161
# p_value = 0.3295

# new df/.csv file with BMI stats included; sans NA values and variables: id, ratio, height, weight,frame, bp.2s, & bp.2d
diabetes_data_sub = subset(diabetes_data, select = -c(id, ratio, height, weight,
                                                      frame, bp.2s, bp.2d))

sum(is.na(diabetes_data_sub))
sum(complete.cases(diabetes_data_sub))
is.na(diabetes_data_sub)
diabetes_data_no_na<-na.omit(diabetes_data_sub)
write.csv(diabetes_data_no_na, "diabetes_data_no_na.csv")

diabetes_data_no_na_2 <- read.table("diabetes_data_no_na_2.csv", header=TRUE ,sep=",")

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
diabetes_data_sub_2 = subset(diabetes_data_no_na, select = -c(stab.glu, time.ppn, hasDiabetes))
attach(diabetes_data_sub_2)

location <- factor(location)
is.factor(location)
contrasts(location)

gender <- factor(gender)
is.factor(gender)
levels(gender)
contrasts(gender)

bmiCat <- factor(bmiCat)
is.factor(bmiCat)
levels(bmiCat)
contrasts(bmiCat)

ageCat <- factor(ageCat)
is.factor(ageCat)
levels(ageCat)
contrasts(ageCat)

## create new WHR (waist-hip-ratio) variable (waist/hip)
diabetes_data_sub_2$WHR <- round(waist/hip,2)

#################################################################
# Automatic Selection Procedures (Linear Model)
#################################################################

library(leaps)

##perform all possible regressions (1st order)
allreg <- regsubsets(glyhb~chol+hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+bmiCat+ageCat+WHR, diabetes_data_sub_2, nbest=9)

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
regnull <- lm(glyhb~1, diabetes_data_sub_2)
##model with all predictors
regfull <- lm(glyhb~chol+hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+bmiCat+ageCat+WHR, data=diabetes_data_sub_2)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

## best model via automatic procedures:

allofem <- lm(glyhb~chol+hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+bmiCat+ageCat+WHR)
summary(allofem)

result<-lm(glyhb ~ ageCat + waist + chol + hdl, diabetes_data_sub_2)
summary(result)


# Call:
#   lm(formula = glyhb ~ ageCat + waist + chol + hdl, diabetes_data_sub_2)
# 
# Residuals:
#   Glycosolated Hemoglobin 
# Min      1Q  Median      3Q     Max 
# -3.0547 -1.1444 -0.4126  0.4663  8.9609 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    2.957643   0.931269   3.176 0.001619 ** 
#   ageCatSenior   0.052265   0.306843   0.170 0.864842    
# ageCatYounger -1.262670   0.231124  -5.463 8.62e-08 ***
#   waist          0.052344   0.018821   2.781 0.005695 ** 
#   chol           0.011236   0.002409   4.665 4.32e-06 ***
#   hdl           -0.021011   0.006305  -3.332 0.000948 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.971 on 369 degrees of freedom
# Multiple R-squared:  0.2204,	Adjusted R-squared:  0.2098 
# F-statistic: 20.86 on 5 and 369 DF,  p-value: < 2.2e-16

## automatic selection procedures with interaction terms added

##are regression assumptions met?
  jpeg("glyhb_ResidualPlot.jpg")
plot(result$fitted.values,result$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")
dev.off()

jpeg("glyhb_ACF.jpg")
acf(result$residuals, main="ACF of Residuals")
dev.off()

jpeg("glyhb_QQ.jpg")
qqnorm(result$residuals)
qqline(result$residuals, col="red")
dev.off()

library(MASS)
## save boxcox plot to .jpg file
jpeg("glyhb_boxcox.jpg")
boxcox(result, lambda = seq(-4, 1, 0.02))
dev.off()

##########################################
# Automatic Selection w/ Interaction Terms
##########################################

allreg_2 <- regsubsets(glyhb~hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+chol*ageCat+WHR*bmiCat, diabetes_data_sub_2, nbest=9)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best_2 <- as.data.frame(summary(allreg_2)$outmat)
best_2$p <- as.numeric(substr(rownames(best_2),1,1))+1
best_2$r2 <- summary(allreg_2)$rsq
best_2$adjr2 <- summary(allreg_2)$adjr2
best_2$mse <- (summary(allreg_2)$rss)/(dim(data)[1]-best_2$p)
best_2$cp <- summary(allreg_2)$cp
best_2$bic <- summary(allreg_2)$bic
best_2

##sort by various criteria
best_2[order(best_2$r2),]
best_2[order(best_2$adjr2),]
best_2[order(best_2$mse),]
best_2[order(best_2$cp),]
best_2[order(best_2$bic),]

##intercept only model
regnull_2 <- lm(glyhb~1, diabetes_data_sub_2)
##model with all predictors
regfull_2 <- lm(glyhb~hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+chol*ageCat+WHR*bmiCat, diabetes_data_sub_2)

##forward selection, backward elimination, and stepwise regression
step(regnull_2, scope=list(lower=regnull_2, upper=regfull_2), direction="forward")
step(regfull_2, scope=list(lower=regnull_2, upper=regfull_2), direction="backward")
step(regnull_2, scope=list(lower=regnull_2, upper=regfull_2), direction="both")

## best model via automatic procedures:

allofem_2 <- lm(glyhb~hdl+bmi+age+bp.1d+bp.1s+waist+hip+location+gender+chol*ageCat+WHR*bmiCat)
summary(allofem_2)

result_2<-lm(glyhb ~ ageCat + waist + chol + hdl, diabetes_data_sub_2)
summary(result_2)

anova(result_2, allofem_2)

############################################
# linear Model, Additional Exploration
############################################

attempt_1 <- lm(glyhb~hdl+waist+chol*bmiCat+WHR*ageCat, data = diabetes_data_sub_2)
summary(attempt_1)

attempt_2 <- lm(glyhb~hdl+waist*chol+bmiCat+WHR*ageCat, data = diabetes_data_sub_2)
summary(attempt_2)

attempt_3 <- lm(glyhb~hdl+waist+chol+bmiCat+WHR*ageCat, data = diabetes_data_sub_2)
summary(attempt_3)

attempt_4 <- lm(glyhb ~ hdl + waist + chol + bmi + WHR * ageCat, data = diabetes_data_sub_2)
summary(attempt_4)

Call:
#   lm(formula = glyhb ~ hdl + waist + chol + bmi + WHR * ageCat, 
#      data = diabetes_data_sub_2)
# 
# Residuals:
#   Glycosolated Hemoglobin 
# Min      1Q  Median      3Q     Max 
# -3.6135 -1.0557 -0.3751  0.4223  8.9351 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.999632   2.307381  -0.867  0.38672    
# hdl               -0.020566   0.006271  -3.279  0.00114 ** 
#   waist              0.089931   0.047344   1.900  0.05828 .  
# chol               0.011473   0.002396   4.788 2.45e-06 ***
#   bmi               -0.034711   0.035270  -0.984  0.32569    
# WHR                4.959758   2.844314   1.744  0.08205 .  
# ageCatSenior       6.154387   3.611586   1.704  0.08922 .  
# ageCatYounger      6.834809   2.821502   2.422  0.01590 *  
#   WHR:ageCatSenior  -6.823520   3.970926  -1.718  0.08658 .  
# WHR:ageCatYounger -9.137201   3.196733  -2.858  0.00450 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.955 on 365 degrees of freedom
# Multiple R-squared:  0.2406,	Adjusted R-squared:  0.2219 
# F-statistic: 12.85 on 9 and 365 DF,  p-value: < 2.2e-16

par(mfrow=c(2,2))

##are regression assumptions met?
jpeg("glyhb_ResidualPlot.jpg")
plot(attempt_4$fitted.values,attempt_4$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")
dev.off()

jpeg("glyhb_ACF.jpg")
acf(attempt_4$residuals, main="ACF of Residuals")
dev.off()

jpeg("glyhb_QQ.jpg")
qqnorm(attempt_4$residuals)
qqline(attempt_4$residuals, col="red")
dev.off()

library(MASS)
## save boxcox plot to .jpg file
jpeg("glyhb_boxcox.jpg")
boxcox(attempt_4, lambda = seq(-4, 1, 0.02))
dev.off()


############################################
# Interaction Terms
############################################

# consider each ageCat a subset
a1<-subset(diabetes_data_sub_2, ageCat == "Younger")
a2<-subset(diabetes_data_sub_2, ageCat == "Mature")
a3<-subset(diabetes_data_sub_2, ageCat == "Senior")

# fit 3 separate regressions, one for each age category
ageYounger<-lm(glyhb ~ WHR,data=a1)
ageMature<-lm(glyhb ~ WHR,data=a2)
ageSenior<-lm(glyhb ~ WHR,data=a3)


##create a scatterplot with different colors and symbols for each age
plot(WHR,glyhb, main="Glycosylated Hemoglobin Levels against Waist-Hip Ratio, by Age")
points(a2$WHR,a2$glyhb, pch=2, col="red") 
points(a3$WHR,a3$glyhb, pch=12, col="blue")
abline(ageYounger,lty=1)
abline(ageMature,lty=2, col="red") 
abline(ageSenior,lty=3, col="blue")
legend("topleft", c("18-44","45-64","> 65"), lty=c(1,2,3), pch=c(1,2,12), col=c("black","red","blue"))


###########################################
# Transform
###########################################


pw1.glyhb<-glyhb**-1.5
response.pw1<-lm(pw1.glyhb~hdl + waist + chol + bmi + WHR * ageCat, data = diabetes_data_sub_2)

par(mfrow=c(2,2))


plot(response.pw1$fitted.values,response.pw1$residuals, main="Plot of residuals against fits
     (after transforming response only -- glyhb**-3/2)")
abline(h=0,col="red")


acf(response.pw1$residuals, main="ACF of Residuals")
qqnorm(response.pw1$residuals)
qqline(response.pw1$residuals, col="red")

boxcox(response.pw1, lambda = seq(-1, 4, 0.02))

boxcox(response.pw3, lambda = seq(-1, 4, 0.02))
dev.off()


##########################################################
# Trying Logistic models
##########################################################

set.seed(100)
data<-diabetes_data_no_na
sample<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]


log_diabetes <- glm(hasDiabetes ~age +chol + bmi + hdl, family = "binomial", data=train)
summary(log_diabetes)
preds<-predict(log_diabetes,newdata=test, type="response")

rates<-prediction(preds, test$hasDiabetes)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##ROC curve 
plot(roc_result, main="ROC Curve for Diabetes")
lines(x = c(0,1), y = c(0,1), col="red")

##AUC
auc<-performance(rates, measure = "auc")
auc@y.values[[1]]

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$hasDiabetes, preds>0.1)
