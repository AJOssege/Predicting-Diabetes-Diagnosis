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


