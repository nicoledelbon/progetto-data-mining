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
data_numeric <- data_numeric[, names(data_numeric) %in% c("Family_Income", "Attendance_Rate",
                                                          "Assignment_Delay_Days", "Travel_Time_Minutes",
                                                          "Stress_Index","GPA")]
cor(data_numeric, use="complete.obs")
table(data$Dropout)  

f1_score <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

data0 <- train[train$Dropout==0,]
data1 <- train[train$Dropout==1,]


# imputazione standard ----------------------------------------------------

p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)

models <- list(model1, model2, model3, model4)

eval = function(models, test){
  accuracies <- numeric(length(models))
  f1 <- numeric(length(models))
  
  for (i in seq_along(models)) {
    prediction <- lapply(getfit(models[[i]]), predict, 
                         se.fit = TRUE, newdata = test, type = "response")
    single_prediction <- sapply(prediction, `[[`, "fit")
    final_pred <- apply(single_prediction, 1, mean)
    final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
    confusion_matrix <- table(test$Dropout, final_pred_class)
    accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    f1[i] <- f1_score(confusion_matrix)
  }
  
  rbind(accuracies, f1)
}

eval(models,test)
