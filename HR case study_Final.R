setwd("F:/Data Science/HR Analytics Case Study")

in_time<-read.csv("in_time.csv", stringsAsFactors = F)
out_time<-read.csv("out_time.csv", stringsAsFactors = F)
employee<-read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager<-read.csv("manager_survey_data.csv", stringsAsFactors = F)
general<-read.csv("general_data.csv", stringsAsFactors = F)

#checking if there are no duplicate employeeIDs in data sets
sum(duplicated(employee$EmployeeID))
sum(duplicated(general$EmployeeID))
sum(duplicated(in_time$X))
sum(duplicated(out_time$X))
sum(duplicated(manager$EmployeeID))
#no data sets have duplicate emloyee IDs

#working with in_time and out_time

library(lubridate)
sum(is.na(in_time)) #109080 records
sum(is.na(out_time)) #109080 records i.e. same number

#removing 1st columns from both time related data frames
in_time<-in_time[,c(-1)]
out_time<-out_time[,c(-1)]

in_time<-data.frame(sapply(in_time, function(x) as.POSIXlt(x,origin="1970-01-01", "%Y-%m-%d %H:%M:%S")))

out_time<-data.frame(sapply(out_time, function(x) as.POSIXlt(x,origin="1970-01-01", "%Y-%m-%d %H:%M:%S")))

time_diff<-out_time-in_time #time difference for each day, each employee
time_diff<-data.frame(sapply(time_diff, function(x) as.numeric(x))) #converting to numeric
time_diff$Avg_time<-rowMeans(time_diff, na.rm = TRUE) #calculating average time for each employee

time_diff<-time_diff[,colSums(is.na(time_diff))<nrow(time_diff)] #removing all columns have all rows as NA, as those days can we weekends or holidays

time_diff$EmployeeID <- seq.int(nrow(time_diff)) #assigning employeeIDs as all were unique

time_diff<-time_diff[,c(251,250)] #keeping only employeeIDs and their average working time in minutes.

#Now checking general dataframe
str(general)

#seems that few columns have only 1 value, converting them to factors to cross-verify their levels

general$EmployeeCount<-as.factor(general$EmployeeCount)
general$Over18<-as.factor(general$Over18)
general$StandardHours<-as.factor(general$StandardHours)

str(general)
# our estimation was correct, hence removing those levels

#removing those columns
general<-general[, -c(8,16,18)]

#checking for employee and manager dataframe also
str(employee)
str(manager)
#they all have various levels/values, hence we are good to go ahead

#merging all dataframes

employee_HRcase<- merge(employee, general, by="EmployeeID")

employee_HRcase<- merge(employee_HRcase, manager, by="EmployeeID")

employee_HRcase<- merge(employee_HRcase, time_diff, by="EmployeeID")


#next we will check the outliers based on boxplots and remove the outliers based on them
library(ggplot2)
library(cowplot)

ggplot(employee_HRcase, aes(x="count",y=Age))+ geom_boxplot(width=0.1)+coord_flip() #no outliers
ggplot(employee_HRcase, aes(x="",y=EnvironmentSatisfaction))+ geom_boxplot(width=0.1)+coord_flip()#no outliers
ggplot(employee_HRcase, aes(x="",y=JobSatisfaction))+ geom_boxplot(width=0.1)+coord_flip() #no outliers
ggplot(employee_HRcase, aes(x="",y=WorkLifeBalance))+ geom_boxplot(width=.5)+coord_flip() #no outliers
ggplot(employee_HRcase, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip() #no outliers
ggplot(employee_HRcase, aes(x="",y=Education))+ geom_boxplot(width=0.1)+coord_flip() #no outliers
ggplot(employee_HRcase, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)+coord_flip() #no Outliers
ggplot(employee_HRcase, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip() #few outliers present
ggplot(employee_HRcase, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip() #no outliers present
ggplot(employee_HRcase, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip() #few outliers present
ggplot(employee_HRcase, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip() #few outliers present
ggplot(employee_HRcase, aes(x="",y=Avg_time))+ geom_boxplot(width=0.1)+coord_flip() # fw outliers present
ggplot(employee_HRcase, aes(x="",y=PerformanceRating))+ geom_boxplot(width=0.1)+coord_flip() # outliers present
ggplot(employee_HRcase, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip() #outliers present
ggplot(employee_HRcase, aes(x="",y=employee_HRcase$YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip() #outliers present
ggplot(employee_HRcase, aes(x="",y=employee_HRcase$TotalWorkingYears))+ geom_boxplot(width=.1)+coord_flip()  # outliers present
ggplot(employee_HRcase, aes(x="",y=employee_HRcase$JobInvolvement))+ geom_boxplot(width=0.1)+coord_flip() #no outliers present
ggplot(employee_HRcase, aes(x="",y=employee_HRcase$PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip() #no outliers present

#for those having outliers, removing them
box<-boxplot.stats(employee_HRcase$MonthlyIncome)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$MonthlyIncome %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$YearsAtCompany)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$YearsAtCompany %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$Avg_time)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$Avg_time %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$PerformanceRating)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$PerformanceRating %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$YearsWithCurrManager)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$YearsWithCurrManager %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$YearsSinceLastPromotion)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$YearsSinceLastPromotion %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$TotalWorkingYears)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$TotalWorkingYears %in% out,]
employee_HRcase<-employee_HRcase1

box<-boxplot.stats(employee_HRcase$NumCompaniesWorked)
out<-box$out
employee_HRcase1<-employee_HRcase[!employee_HRcase$NumCompaniesWorked %in% out,]
employee_HRcase<-employee_HRcase1

sum(sapply(employee_HRcase, function(x) sum(is.na(x))))
#there are 62 NA values in overall data, i.e. 62/2559~2.4% of overall data,hence removing those

employee_HRcase<-na.omit(employee_HRcase)
#The data set is not free of NA and outliers

remove(employee_HRcase1) #removing as its no longer required

#converting attrition column to factor level
employee_HRcase$Attrition<- ifelse(employee_HRcase$Attrition=="Yes",1,0)


str(employee_HRcase$PerformanceRating)
#seems all are same so converting to factor
employee_HRcase$PerformanceRating<-as.factor(employee_HRcase$PerformanceRating)
str(employee_HRcase$PerformanceRating)

#removing the column
employee_HRcase$PerformanceRating<-NULL

#Need to scale the continuous variables, as they are on different scales.
#taking out continuos variables to a different dataframe

employee_HRcase_Cont<-employee_HRcase[,c(5,9,13,16,17,18,20,21,22,23,24,26)] #taking out columns to perform scaling
employee_HRcase_Cont<-data.frame(sapply(employee_HRcase_Cont, function(x) scale(x)))

employee_HRcase<-employee_HRcase[,-c(5,9,13,16,17,18,20,21,22,23,24,26)] #removing to be scaled columns
employee_HRcase<-cbind(employee_HRcase, employee_HRcase_Cont) #adding the scaled columns to the dataframe

str(employee_HRcase)

#converting to factors
employee_HRcase$EnvironmentSatisfaction<-as.factor(employee_HRcase$EnvironmentSatisfaction)
employee_HRcase$JobSatisfaction<-as.factor(employee_HRcase$JobSatisfaction)
employee_HRcase$WorkLifeBalance<-as.factor(employee_HRcase$WorkLifeBalance)
employee_HRcase$Attrition<-as.factor(employee_HRcase$Attrition)
employee_HRcase$Education<-as.factor(employee_HRcase$Education)
employee_HRcase$StockOptionLevel<-as.factor(employee_HRcase$StockOptionLevel)
employee_HRcase$JobInvolvement<-as.factor(employee_HRcase$JobInvolvement)
employee_HRcase$Gender<-as.factor(employee_HRcase$Gender)

str(employee_HRcase)

employee_HRcase$Gender<-ifelse(employee_HRcase$Gender=="Female",0,1)

#employee_HRcase$EnvironmentSatisfaction<-as.numeric(levels(employee_HRcase$EnvironmentSatisfaction))[employee_HRcase$EnvironmentSatisfaction]
#employee_HRcase$JobSatisfaction<-as.numeric(levels(employee_HRcase$JobSatisfaction))[employee_HRcase$JobSatisfaction]
#employee_HRcase$WorkLifeBalance<-as.numeric(levels(employee_HRcase$WorkLifeBalance))[employee_HRcase$WorkLifeBalance]
#employee_HRcase$Education<-as.numeric(levels(employee_HRcase$Education))[employee_HRcase$Education]
#employee_HRcase$JobLevel<-as.numeric(levels(employee_HRcase$JobLevel))[employee_HRcase$JobLevel]
#employee_HRcase$NumCompaniesWorked<-as.numeric(levels(employee_HRcase$NumCompaniesWorked))[employee_HRcase$NumCompaniesWorked]
#employee_HRcase$StockOptionLevel<-as.numeric(levels(employee_HRcase$StockOptionLevel))[employee_HRcase$StockOptionLevel]
#employee_HRcase$TrainingTimesLastYear<-as.numeric(levels(employee_HRcase$TrainingTimesLastYear))[employee_HRcase$TrainingTimesLastYear]
#employee_HRcase$YearsSinceLastPromotion<-as.numeric(levels(employee_HRcase$YearsSinceLastPromotion))[employee_HRcase$YearsSinceLastPromotion]
#employee_HRcase$JobInvolvement<-as.numeric(levels(employee_HRcase$JobInvolvement))[employee_HRcase$JobInvolvement]

str(employee_HRcase)

dummy_1 <- data.frame(model.matrix( ~employee_HRcase$BusinessTravel, data = employee_HRcase))
dummy_1 <- dummy_1[,-c(1)]
employee_HRcase_1<-cbind(employee_HRcase[,-6], dummy_1)

dummy_2 <- data.frame(model.matrix( ~employee_HRcase$Department, data = employee_HRcase_1))
dummy_2 <- dummy_2[,-c(1)]
employee_HRcase_2<-cbind(employee_HRcase_1[,-6], dummy_2)

dummy_3 <- data.frame(model.matrix( ~employee_HRcase$EducationField, data = employee_HRcase_2))
dummy_3 <- dummy_3[,-c(1)]
employee_HRcase_3<-cbind(employee_HRcase_2[,-7], dummy_3)

dummy_4 <- data.frame(model.matrix( ~employee_HRcase$JobRole, data = employee_HRcase_3))
dummy_4 <- dummy_4[,-c(1)]
employee_HRcase_4<-cbind(employee_HRcase_3[,-8], dummy_4)

dummy_5 <- data.frame(model.matrix( ~employee_HRcase$MaritalStatus, data = employee_HRcase_4))
dummy_5 <- dummy_5[,-c(1)]
employee_HRcase_5<-cbind(employee_HRcase_4[,-8], dummy_5)

dummy_6 <- data.frame(model.matrix( ~employee_HRcase$EnvironmentSatisfaction, data = employee_HRcase_5))
dummy_6 <- dummy_6[,-c(1)]
employee_HRcase_6<-cbind(employee_HRcase_5[,-2], dummy_6)

dummy_7 <- data.frame(model.matrix( ~employee_HRcase$JobSatisfaction, data = employee_HRcase_6))
dummy_7 <- dummy_7[,-c(1)]
employee_HRcase_7<-cbind(employee_HRcase_6[,-2], dummy_7)

dummy_8 <- data.frame(model.matrix( ~employee_HRcase$WorkLifeBalance, data = employee_HRcase_7))
dummy_8 <- dummy_8[,-c(1)]
employee_HRcase_8<-cbind(employee_HRcase_7[,-2], dummy_8)

dummy_9 <- data.frame(model.matrix( ~employee_HRcase$Education, data = employee_HRcase_8))
dummy_9 <- dummy_9[,-c(1)]
employee_HRcase_9<-cbind(employee_HRcase_8[,-3], dummy_9)

dummy_10 <- data.frame(model.matrix( ~employee_HRcase$StockOptionLevel, data = employee_HRcase_9))
dummy_10 <- dummy_10[,-c(1)]
employee_HRcase_10<-cbind(employee_HRcase_9[,-4], dummy_10)

dummy_11 <- data.frame(model.matrix( ~employee_HRcase$JobInvolvement, data = employee_HRcase_10))
dummy_11 <- dummy_11[,-c(1)]
employee_HRcase_11<-cbind(employee_HRcase_10[,-4], dummy_11)

str(employee_HRcase_11)
#We will build our model based on employee_HRcase_11 dataframe

library("car")
library(MASS)
library(caTools)
set.seed(100)
employee_Indices<-sample.split(employee_HRcase_11$Attrition, SplitRatio = 0.7)
train_employeeHR<-employee_HRcase_11[employee_Indices,]
test_employeeHR<-employee_HRcase_11[!employee_Indices,]

#Logistic Regression

#Initial model
model_1 = glm(Attrition ~ ., data = train_employeeHR, family = "binomial")
summary(model_1)

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldMedical + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleResearch.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5 + employee_HRcase.JobInvolvement3 + 
               employee_HRcase.JobRoleHuman.Resources, family = "binomial", data=train_employeeHR)
summary(model_3)

#removing EducationFieldMedical with high p-value

model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleResearch.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5 + employee_HRcase.JobInvolvement3 + 
               employee_HRcase.JobRoleHuman.Resources, family = "binomial", data=train_employeeHR)
summary(model_4)

#removing JobRoleHuman.Resources due to high p-value

model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleResearch.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5 + employee_HRcase.JobInvolvement3, 
               family = "binomial", data=train_employeeHR)

summary(model_5)


#removing TrainingTimesLastYear having high p value

model_6<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleResearch.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5 + employee_HRcase.JobInvolvement3, 
             family = "binomial", data=train_employeeHR)

summary(model_6)

#removing JobRoleResearch.Director due to high p-value
model_7<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5 + employee_HRcase.JobInvolvement3, 
             family = "binomial", data=train_employeeHR)
summary(model_7)

#removing JobInvolvement3

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + 
               employee_HRcase.EducationFieldOther + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5, family = "binomial", data=train_employeeHR)

summary(model_8)

#removing EducationFieldOther
model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
               YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
               employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
               employee_HRcase.JobRoleManufacturing.Director + 
               employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
               employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
               employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
               employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
               employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
               employee_HRcase.WorkLifeBalance3 + employee_HRcase.WorkLifeBalance4 + 
               employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_9)
vif(model_9)

#removing WorkLifeBalance3 due to relatively high VIF

model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
                employee_HRcase.JobRoleManufacturing.Director + 
                employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
                employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
                employee_HRcase.WorkLifeBalance4 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_10)

#removing WorkLifeBalance4 due to high value
model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
                employee_HRcase.JobRoleManufacturing.Director + 
                employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction2 + employee_HRcase.JobSatisfaction3 + 
                employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_11)

#removing JobSatisfaction2 due to relatively high p value
model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
                employee_HRcase.JobRoleManufacturing.Director + 
                employee_HRcase.JobRoleSales.Representative + employee_HRcase.MaritalStatusMarried + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + 
                employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_12)
vif(model_12)
#removing MaritalStatusMarried due to relatively high vif

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + 
                employee_HRcase.JobSatisfaction4 + employee_HRcase.WorkLifeBalance2 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_13)
vif(model_13)
#all have low VIF, hence checking p valye and removing WorkLifeBalance2

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + employee_HRcase.EducationFieldTechnical.Degree + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_14)

#removing EducationFieldTechnical.Degree due to high p value
model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.EducationFieldMarketing + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_15)

#removing EducationFieldMarketing
model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4 + 
                employee_HRcase.Education5, family = "binomial", data=train_employeeHR)
summary(model_16)
vif(model_16)
#emoving Education5 due to relatively high p value

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4, 
                family = "binomial", data=train_employeeHR)
summary(model_17)
vif(model_17)
#removing TotalWorkingYears

model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.JobRoleManufacturing.Director + employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4, 
              family = "binomial", data=train_employeeHR)
summary(model_18)
vif(model_18)
#removing employee_HRcase.JobRoleManufacturing.Director

model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                YearsWithCurrManager + Avg_time + employee_HRcase.BusinessTravelTravel_Frequently + 
                employee_HRcase.JobRoleSales.Representative + 
                employee_HRcase.MaritalStatusSingle + employee_HRcase.EnvironmentSatisfaction2 + 
                employee_HRcase.EnvironmentSatisfaction3 + employee_HRcase.EnvironmentSatisfaction4 + 
                employee_HRcase.JobSatisfaction3 + employee_HRcase.JobSatisfaction4, 
              family = "binomial", data=train_employeeHR)
summary(model_19)

#Considering this as finale model
final_model<- model_19


#calculatig predicted probablity for attrition by removing the attrition variable
test_predicted = predict(final_model, type = "response", test_employeeHR[,-2])
summary(test_predicted)

test_employeeHR$probablity <- test_predicted

test_pred_attr <- factor(ifelse(test_employeeHR$probablity >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test_employeeHR$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)


#reducing probablity to 0.40
test_pred_attr <- factor(ifelse(test_predicted >= 0.40, "Yes", "No"))
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")

test_conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_predicted >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_predicted)

# Creating cutoff values from 0.0006987 to 0.92444777 for plotting and initiallizing a matrix of 100 X 3.

s = seq(0.01, 0.9,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen","darkblue","darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

test_cutoff_attr <- as.factor(ifelse(test_predicted >=0.1718, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


library(ROCR)
#on testing  data
pred_test_new<- prediction(test_cutoff_attr, test_actual_attr)

performance_test_measures<- performance(pred_test_new, "tpr", "fpr")

ks_test <- attr(performance_test_measures, "y.values")[[1]] - 
  (attr(performance_test_measures, "x.values")[[1]])

max(ks_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predict_probablity,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predict_probablity)) predict_probablity <- as.integer(as.character(predict_probablity))
  helper = data.frame(cbind(labels , predict_probablity))
  helper[,"bucket"] = ntile(-helper[,"predict_probablity"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attr, test_predicted, groups = 10)
