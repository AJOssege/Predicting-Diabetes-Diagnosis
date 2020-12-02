## Set the current directory to Project-2
setwd("C:/SP-PERSONAL/CollegeWork-UVA-MSDS/STAT-6021/Project-2/")
getwd()

## Diabetes data set after cleaning the unnecessary and removing the blank values.
data = read.csv("diabetes_data_sub.csv")

## Check and make them Categorical predictors
is.factor(data$location)
is.factor(data$gender)
is.factor(data$bmiCat)
is.factor(data$hasDiabetes)
data$hasDiabetes<-factor(data$hasDiabetes)
is.factor(data$hasDiabetes)

## MLR Model with selected predictors
result<-lm(glyhb~chol+stab.glu+hdl+age+time.ppn+location+bmi,data=data)
summary(result)

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

dim(data)[1] # Rows
dim(data)[2] # Columns

#n<-length(data) # Count in the data
n<- dim(data)[1] # Rows
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

# Another way to see is to plot and draw a line to see visually
plot(lev, main="Leverages", ylim=c(0,0.4))
abline(h=2*p/n, col="red")

##identify levarage data points on plot
identify(lev) 

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

