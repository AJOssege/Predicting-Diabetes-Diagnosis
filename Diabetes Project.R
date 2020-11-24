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

                  
#######################################################
#Create boxplots to review for outliers               #
# By Different Factors: Gender, HasDiabetes, Location #
#######################################################

#Chol
boxplot(diabetes_data$chol, xlab="chol")

ggplot(diabetes_data, aes(x=gender, y=chol)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=chol)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=chol)) + 
  geom_boxplot()

#Stab.glu
boxplot(diabetes_data$stab.glu,  xlab="stab.glu")

ggplot(diabetes_data, aes(x=gender, y=stab.glu)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=stab.glu)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=stab.glu)) + 
  geom_boxplot()

#hdl
boxplot(diabetes_data$hdl, xlab="hdl")

ggplot(diabetes_data, aes(x=gender, y=hdl)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=hdl)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=hdl)) + 
  geom_boxplot()


#ratio

boxplot(diabetes_data$ratio, xlab="ratio")


ggplot(diabetes_data, aes(x=gender, y=ratio)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(location), y=ratio)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=ratio)) + 
  geom_boxplot()


#age

boxplot(diabetes_data$age, xlab="age")

ggplot(diabetes_data, aes(x=gender, y=age)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=age)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=age)) + 
  geom_boxplot()


#Height
boxplot(diabetes_data$height, xlab="height")

ggplot(diabetes_data, aes(x=gender, y=height)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=height)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=height)) + 
  geom_boxplot()


#Weight

boxplot(diabetes_data$weight, xlab="weight")

ggplot(diabetes_data, aes(x=gender, y=weight)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=weight)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=weight)) + 
  geom_boxplot()

#bp.1s
boxplot(diabetes_data$bp.1s, xlab="bp.1s")

ggplot(diabetes_data, aes(x=gender, y=bp.1s)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(location), y=bp.1s)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=bp.1s)) + 
  geom_boxplot()


#bp.1d
boxplot(diabetes_data$bp.1d, xlab="bp.1d")

ggplot(diabetes_data, aes(x=gender, y=bp.1d)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(location), y=bp.1d)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=bp.1d)) + 
  geom_boxplot()


#bp.2s

boxplot(diabetes_data$bp.2s, xlab="bp.2s")

ggplot(diabetes_data, aes(x=gender, y=bp.2s)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(location), y=bp.2s)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=bp.2s)) + 
  geom_boxplot()

#bp.2d

boxplot(diabetes_data$bp.2d, xlab="bp.2d")

ggplot(diabetes_data, aes(x=gender, y=bp.2d)) + 
  geom_boxplot()



ggplot(diabetes_data, aes(x=as.factor(location), y=bp.2d)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=bp.2d)) + 
  geom_boxplot()

#waist
boxplot(diabetes_data$waist, xlab="waist")

ggplot(diabetes_data, aes(x=gender, y=waist)) + 
  geom_boxplot()



ggplot(diabetes_data, aes(x=as.factor(location), y=waist)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=waist)) + 
  geom_boxplot()

#hip

boxplot(diabetes_data$hip, xlab="hip")

ggplot(diabetes_data, aes(x=gender, y=hip)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(location), y=hip)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=hip)) + 
  geom_boxplot()

#time.ppn

boxplot(diabetes_data$time.ppn, xlab="time.ppn")

ggplot(diabetes_data, aes(x=gender, y=time.ppn)) + 
  geom_boxplot()


ggplot(diabetes_data, aes(x=as.factor(location), y=time.ppn)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=time.ppn)) + 
  geom_boxplot()

#bmi


boxplot(diabetes_data$bmi, xlab="bmi")

ggplot(diabetes_data, aes(x=gender, y=bmi)) + 
  geom_boxplot()



ggplot(diabetes_data, aes(x=as.factor(location), y=bmi)) + 
  geom_boxplot()

ggplot(diabetes_data, aes(x=as.factor(hasDiabetes), y=bmi)) + 
  geom_boxplot()



##########################
# Scatter & Correlations #
##########################


#Create dataframe of only numeric columns for scatterplots and correlations
myvars <- names(diabetes_data) %in% c("id", "location", "gender","frame", "bmiCat", "weight", "height", "hip","waist")
diabetes_num <- diabetes_data[!myvars]

diabetes_data$gender_n<-as.numeric(diabetes_data$gender)
as.numeric(diabetes_data$location)


pairs(diabetes_num, lower.panel = NULL)
diabetes_num.corr<-cor(diabetes_num, use="pairwise.complete.obs")
pheatmap(diabetes_num.corr, display_numbers = T, color = colorRampPalette(c("red", "white", "blue"))(50), cluster_rows = F, cluster_cols = F, fontsize_number = 13)

#Body Measurements
myvarsbody <- c("weight", "height", "hip","waist", "glyhb")
diabetes_body <- diabetes_data[myvarsbody]

pairs(diabetes_body, lower.panel = NULL)
diabetes_body.corr<-cor(diabetes_body, use="pairwise.complete.obs")
pheatmap(diabetes_body.corr, display_numbers = T, color = colorRampPalette(c("red", "white", "blue"))(50), cluster_rows = F, cluster_cols = F, fontsize_number = 13)
                  
                
               
                  
                  
                  
                  
                  
                  
# new df/.csv file with BMI stats included; sans NA values and variables: id, ratio, height, weight,frame, bp.2s, & bp.2d
diabetes_data_sub = subset(diabetes_data, select = -c(id, ratio, height, weight,
                                                      frame, bp.2s, bp.2d))

sum(is.na(diabetes_data_sub))
sum(complete.cases(diabetes_data_sub))
is.na(diabetes_data_sub)
diabetes_data_no_na<-na.omit(diabetes_data_sub)
write.csv(diabetes_data_no_na, "diabetes_data_no_na.csv")
