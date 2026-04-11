rm(list=ls())
student_dropout_dataset_v3 <- read.csv("student_dropout_dataset_v3.csv")
data <- student_dropout_dataset_v3[,-1]

fattori <- c("Gender","Internet_Access","Semester","Department",
             "Parental_Education","Part_Time_Job","Scholarship","Dropout")

data[fattori] <- lapply(data[fattori], as.factor)
summary(data)
colnames(data)
data_numeric <- data[, c("Age","Family_Income", "Study_Hours_per_Day" ,"Attendance_Rate","Assignment_Delay_Days",
                         "Travel_Time_Minutes" ,"Stress_Index" , "GPA","Semester_GPA" ,"CGPA")]
par(mfrow=c(2,5))
for(i in 1:ncol(data_numeric)){
  boxplot(data_numeric[,i] ~ data$Dropout, col=c("green", "red"), ylab=colnames(data_numeric)[i],
       main="", pch=16)
}
par(mfrow=c(1,1))

library("visdat")
library("naniar")
vis_miss(data)
gg_miss_var(data)
library(mice)
md.pattern(data)

cor <- cor(data_numeric, use="complete.obs")
library(ggcorrplot)
ggcorrplot(cor, lab=T, hc.order=T)

mfull <- glm(Dropout ~ . , data=data, family="binomial")
mnull <- glm(Dropout ~ 1 , data=data, family="binomial")
step(mfull, mnull)

data <- data[, (names(data) %in% c("Family_Income", "Internet_Access",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout"))]
