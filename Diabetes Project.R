library(faraway)
library(psych)
library(ggplot2)

# read in diabetes data
# do exploratory

# load data
data(diabetes)

# view data frame
View(diabetes)

#response variable
diabetes_data<- diabetes

#Creating additional predictors

diabetes_data$hasDiabetes<-ifelse(diabetes_data$glyhb>=7.0,1,0)
diabetes_data$bmi <- 703*diabetes_data$weight/(diabetes_data$height)^2
diabetes_data$bmiCat <- ifelse(diabetes_data$bmi <18.5, "Underweight",ifelse(diabetes_data$bmi <25, "Normal",ifelse(diabetes_data$bmi <30, "Overweight","Obese")))


# plots of categoricals
plot(diabetes_data$location, main="Location Samples", ylab="Count", col="orange")
plot(diabetes_data$gender, main="Gender", ylab="Count", col="green")
plot(diabetes_data$frame,  main= "Body Frame", ylab="Count", col="blue" )



#changing the order of bmi from smallest to largest for plot
diabetes_data$bmiCat<-factor(diabetes_data$bmiCat, 
                       levels = c("Underweight","Normal","Overweight","Obese", "NA"))
#bmi
values <-c(diabetes_data$hasDiabetes)
p<-ggplot(diabetes_data, aes(bmiCat))
p +geom_bar()

#descriptive Statistics
describe(diabetes_data)

#Missing values analysis
any(is.na(diabetes_data))
sum(is.na(diabetes_data))
sum(complete.cases(diabetes_data))
na_count <-sapply(diabetes_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

myvars<-c("glyhb","frame")
newdata <-diabetes_data[myvars]

# Missing values analysis Summary:
# bp.2s and bp.2d have over half sample as NA. 
# Could the absence of data for the second systolic and diastolic BP be important?
# Why do so many not have a second BP?
# 13 cases do not have 'hasdiabetes' value due to missing glyhb
# 12 cases are missing "frame". Can this be predicted or filled in with mean? 
# frame and glyhb do not have NA in same rows

# new df/.csv file with BMI stats included; sans NA values and variables: id, ratio, height, weight,frame, bp.2s, & bp.2d
diabetes_data_sub = subset(diabetes_data, select = -c(id, ratio, height, weight,
                                                      frame, bp.2s, bp.2d))

sum(is.na(diabetes_data_sub))
sum(complete.cases(diabetes_data_sub))
is.na(diabetes_data_sub)
diabetes_data_no_na<-na.omit(diabetes_data_sub)
write.csv(diabetes_data_no_na, "diabetes_data_no_na.csv")
