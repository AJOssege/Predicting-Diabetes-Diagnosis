library(faraway)

diabetes_data<- diabetes
diabetes_data$hasDiabetes<-ifelse(diabetes_data$glyhb>=7.0,1,0)
diabetes_data$bmi <- 703*diabetes_data$weight/(diabetes_data$height)^2
diabetes_data$bmiCat <- ifelse(diabetes_data$bmi <18.5, "Underweight",ifelse(diabetes_data$bmi <25, "Normal",ifelse(diabetes_data$bmi <30, "Overweight","Obese")))

#Lauren subset code
diabetes_data_sub = subset(diabetes_data, select = -c(id, ratio, height, weight, frame, bp.2s, bp.2d))
sum(is.na(diabetes_data_sub))
sum(complete.cases(diabetes_data_sub))
is.na(diabetes_data_sub)
diabetes_data_no_na<-na.omit(diabetes_data_sub)
diabetes_data <- diabetes_data_no_na
###############


CDC_by_age <- read.table("CDC_Age_Percents.csv", header = T, sep=",")
CDC_by_gender <- read.table("CDC_Gender_Percents.csv", header = T, sep=",")
CDC_by_race <- read.table("CDC_Race_Percents.csv", header = T, sep=",")
CDC_overall <- read.table("CDC_Overall_Percents.csv", header = T, sep=",")

#Age exploration
adult_data <- subset(diabetes_data, age<45)
middle_age_data <- subset(diabetes_data, age>=45 & age<65)
senior_data <- subset(diabetes_data, age>=65)
study_adult_positive <-nrow(adult_data[adult_data$hasDiabetes == 1,])
study_adult_percent<- study_adult_positive/nrow(adult_data) *100
study_middle_age_positive <-nrow(middle_age_data[middle_age_data$hasDiabetes == 1,])
study_middle_age_percent<-study_middle_age_positive/nrow(middle_age_data) * 100
study_senior_positive <-nrow(senior_data[senior_data$hasDiabetes == 1,]) * 100
study_senior_percent<-study_senior_positive/nrow(senior_data)


#Gender Exploration
male_data <- subset(diabetes_data, gender=="male")
female_data <- subset(diabetes_data, gender=="female")
study_male_percent<-nrow(male_data[male_data$hasDiabetes == 1,])/nrow(male_data)* 100
study_female_percent<-nrow(female_data[female_data$hasDiabetes == 1,])/nrow(female_data)* 100

#Race exploration-Our data only has African Americans, so we should look at just the AA percent
study_african_american_percent<-nrow(diabetes_data[diabetes_data$hasDiabetes == 1,])/nrow(diabetes_data)* 100


#Diabetes bar chart for age groups
#Reference SO for adding error/bounding bars https://stackoverflow.com/questions/49576344/adding-standard-deviation-to-barplot-in-r
all_age_frame <- cbind(c(study_adult_percent,study_middle_age_percent,study_senior_percent),CDC_by_age)
colnames(all_age_frame)[1]<-"study_data"

barCenters <- barplot(rbind(all_age_frame$study_data,all_age_frame$DiagnosedPercent,all_age_frame$TotalPercent,all_age_frame$Prediabetes),
                      main = "Percent Diabetes by Age", xlab = "Age Group", ylab = "%",names.arg=all_age_frame$AgeGroup,col=c("red","lightblue","darkblue","orange"),
                      beside=TRUE)
#Add lower/upper bound to diagnosed
arrows(barCenters[2,], all_age_frame$DiagnosedPercentLower,
       barCenters[2,], all_age_frame$DiagnosedPercentUpper,angle=90,code=3)
#Add lower/upper bound to total diagnosed
arrows(barCenters[3,], all_age_frame$TotalPercentLower,
       barCenters[3,], all_age_frame$TotalPercentUpper,angle=90,code=3)
#Add lower/upper bound to diagnosed
arrows(barCenters[4,], all_age_frame$PrediabetesLower,
       barCenters[4,], all_age_frame$PrediabetesUpper,angle=90,code=3)

legend<- legend(x="topleft",legend=c("FarawayData","CDC Diagnosed","CDC Total", "CDC PreDiabetes"),fill=c("red","lightblue","darkblue","orange"))


#Diabetes bar chart for sex groups
all_sex_frame <- cbind(c(study_male_percent,study_female_percent),CDC_by_gender)
colnames(all_sex_frame)[1]<-"study_data"

barCenters <- barplot(rbind(all_sex_frame$study_data,all_sex_frame$DiagnosedPercent,all_sex_frame$TotalPercent,all_sex_frame$Prediabetes),
                      main = "Percent Diabetes by Sex", xlab = "Sex", ylab = "%",names.arg=all_sex_frame$Sex,col=c("red","lightblue","darkblue","orange"),
                      beside=TRUE)
#Add lower/upper bound to diagnosed
arrows(barCenters[2,], all_sex_frame$DiagnosedPercentLower,
       barCenters[2,], all_sex_frame$DiagnosedPercentUpper,angle=90,code=3)
#Add lower/upper bound to total diagnosed
arrows(barCenters[3,], all_sex_frame$TotalPercentLower,
       barCenters[3,], all_sex_frame$TotalPercentUpper,angle=90,code=3)
#Add lower/upper bound to diagnosed
arrows(barCenters[4,], all_sex_frame$PrediabetesLower,
       barCenters[4,], all_sex_frame$PrediabetesUpper,angle=90,code=3)

legend<- legend(x="topleft",legend=c("FarawayData","CDC Diagnosed","CDC Total", "CDC PreDiabetes"),fill=c("red","lightblue","darkblue","orange"))


#Diabetes Data African Americans
race_frame <- cbind(c(study_african_american_percent),CDC_by_gender[2,])
colnames(race_frame)[1]<-"study_data"
barCenters <- barplot(rbind(race_frame$study_data,race_frame$DiagnosedPercent,race_frame$TotalPercent,race_frame$Prediabetes),
                      main = "Percent Diabetes for African Americans", xlab = "African American Metrics", ylab = "%",names.arg=race_frame$Race,col=c("red","lightblue","darkblue","orange"),
                      beside=TRUE)
arrows(barCenters[2,], race_frame$DiagnosedPercentLower,
       barCenters[2,], race_frame$DiagnosedPercentUpper,angle=90,code=3)
#Add lower/upper bound to total diagnosed
arrows(barCenters[3,], race_frame$TotalPercentLower,
       barCenters[3,], race_frame$TotalPercentUpper,angle=90,code=3)
#Add lower/upper bound to diagnosed
arrows(barCenters[4,], race_frame$PrediabetesLower,
       barCenters[4,], race_frame$PrediabetesUpper,angle=90,code=3)
legend<- legend(x="topleft",legend=c("FarawayData","CDC Diagnosed","CDC Total", "CDC PreDiabetes"),fill=c("red","lightblue","darkblue","orange"))

#Now compare to the overall metrics
#Overall comparison
overall_frame <-cbind(c(study_african_american_percent),CDC_overall)
colnames(overall_frame)[1]<-"study_data"
barCenters <- barplot(rbind(overall_frame$study_data,overall_frame$DiagnosedPercent,overall_frame$TotalPercent,overall_frame$Prediabetes),
                      main = "Percent Diabetes for population", xlab = "Population Metrics", ylab = "%",col=c("red","lightblue","darkblue","orange"),
                      beside=TRUE)
arrows(barCenters[2,], overall_frame$DiagnosedPercentLower,
       barCenters[2,], overall_frame$DiagnosedPercentUpper,angle=90,code=3)
#Add lower/upper bound to total diagnosed
arrows(barCenters[3,], overall_frame$TotalPercentLower,
       barCenters[3,], overall_frame$TotalPercentUpper,angle=90,code=3)
#Add lower/upper bound to diagnosed
arrows(barCenters[4,], overall_frame$PrediabetesLower,
       barCenters[4,], overall_frame$PrediabetesUpper,angle=90,code=3)
legend<- legend(x="topleft",legend=c("FarawayData","CDC Diagnosed","CDC Total", "CDC PreDiabetes"),fill=c("red","lightblue","darkblue","orange"))




#Additional CDC comparisons for diagnosed diabetes:

#1) Overweight/obesity
#89.0% were overweight or had obesity
#-27.6% were overweight (BMI of 25.0 to 29.9 kg/m2).
#-45.8% had obesity (BMI of 30.0 to 39.9 kg/m2)
#-15.5% had extreme obesity (BMI of 40.0 kg/m2 or higher).

has_diabetes_data <-subset(diabetes_data,hasDiabetes=1)
counts<-table(has_diabetes_data$bmiCat)
barplot(counts, main="BMI Groups of those with diabetes")
hist(has_diabetes_data$bmi, main = "Histogram of BMI's of those with diabetes")
percent_above_normal<- nrow(has_diabetes_data[has_diabetes_data$bmi >= 25.0,])/nrow(has_diabetes_data)* 100
percent_above_normal
percent_overweight<- nrow(has_diabetes_data[has_diabetes_data$bmi >= 25.0 & has_diabetes_data$bmi < 30.0,])/nrow(has_diabetes_data)* 100
percent_overweight
percent_obese <-nrow(has_diabetes_data[has_diabetes_data$bmi >= 30.0 & has_diabetes_data$bmi <40.0,])/nrow(has_diabetes_data)* 100
percent_obese
percent_extreme_obese <-nrow(has_diabetes_data[has_diabetes_data$bmi >= 40.0,])/nrow(has_diabetes_data)* 100
percent_extreme_obese


#has_diabetes_data$col1 <- ifelse(has_diabetes_data$bmi >= 25.0,1,0)
#has_diabetes_data$col2 <- ifelse(has_diabetes_data$bmi >= 25.0 & has_diabetes_data$bmi < 30.0,1,0)
#has_diabetes_data$col3 <- ifelse(has_diabetes_data$bmi >= 30.0 & has_diabetes_data$bmi <40.0,1,0)
#has_diabetes_data$col4 <- ifelse(has_diabetes_data$bmi >= 40.0,1,0)

#2)High blood pressure
#68.4% had a systolic blood pressure of 140 mmHg or higher or diastolic blood pressure of 90 mmHg or
#higher or were on prescription medication for their high blood pressure (Appendix Table 8).
high_sys<- nrow(has_diabetes_data[has_diabetes_data$bp.1s >= 140,])/nrow(has_diabetes_data)* 100
high_sys
high_dia<-nrow(has_diabetes_data[has_diabetes_data$bp.1d >= 90,])/nrow(has_diabetes_data)* 100
high_dia

high_bp_percent<- nrow(has_diabetes_data[has_diabetes_data$bp.1s >= 140 | has_diabetes_data$bp.1d >=90,])/nrow(has_diabetes_data)* 100
high_bp_percent


#3) High Cholesterol
# 43.5% had a non-HDL level of 130 mg/dL or higher.
# -22.4% had a non-HDL level of 130 to 159 mg/dL.
# -11.2% had a non-HDL level of 160 to 189 mg/dL.
# - 9.9% had a non-HDL level of 190 mg/dL or higher.
#Our data is missing units, so unsure if correct comparisons
has_diabetes_data$ldl<- has_diabetes_data$chol - has_diabetes_data$hdl#Approximation of ldl
percent_above_130<- nrow(has_diabetes_data[has_diabetes_data$ldl >= 130,])/nrow(has_diabetes_data)* 100
percent_above_130
percent_between_130_160<- nrow(has_diabetes_data[has_diabetes_data$ldl >= 130 & has_diabetes_data$ldl < 160,])/nrow(has_diabetes_data)* 100
percent_between_130_160
percent_between_160_190<- nrow(has_diabetes_data[has_diabetes_data$ldl >= 160 & has_diabetes_data$ldl < 190,])/nrow(has_diabetes_data)* 100
percent_between_160_190
percent_above_190<- nrow(has_diabetes_data[has_diabetes_data$ldl >= 190,])/nrow(has_diabetes_data)* 100
percent_above_190

