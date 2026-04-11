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
<<<<<<< HEAD
=======
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
my_training3 <- mice(train, method = "lasso.norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
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

# imputazione undersampling -----------------------------------------------

set.seed(1)
lab <- sample(1:nrow(data0), nrow(data1))
train_und <- rbind(data0[lab,], data1)

table(train_und$Dropout)

p <- dim(train_und)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
my_training1 <- mice(train_und, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_und, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_und, method = "lasso.norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train_und, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)

models_und <- list(model1, model2, model3, model4)

eval(models_und,test)

# imputazione oversampling ------------------------------------------------

set.seed(1)
lab <- sample(1:nrow(data1), nrow(data0), replace=T)
train_ov <- rbind(data1[lab,], data0)
table(train_ov$Dropout)

p <- dim(train_ov)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
my_training1 <- mice(train_ov, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_ov, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_ov, method = "lasso.norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train_ov, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)

models_ov <- list(model1, model2, model3, model4)

eval(models_ov,test)

# scelgo oversampling -----------------------------------------------------

rm(list=ls())

student_dropout_dataset_v3 <- read.csv("student_dropout_dataset_v3.csv")

metrics <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  TN <- cm[1,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / sum(cm)
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(list(
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    Accuracy = accuracy,
    F1_score = f1
  ))
}

student_dropout_dataset_v3 <- read.csv("student_dropout_dataset_v3.csv")

data <- student_dropout_dataset_v3[,-1] # rimuovo ID
fattori <- c("Gender","Internet_Access","Semester","Department",
             "Parental_Education","Part_Time_Job","Scholarship","Dropout")
data[fattori] <- lapply(data[fattori], as.factor)
data <- data[, (names(data) %in% c("Family_Income", "Internet_Access",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout"))]
set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

data0 <- train[train$Dropout==0,]
data1 <- train[train$Dropout==1,]

set.seed(1)
lab <- sample(1:nrow(data1), nrow(data0), replace=T)
train <- rbind(data1[lab,], data0)

p <- dim(data)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
data_imp <- mice(data, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

densityplot(data_imp)

par(mfrow)
densityplot(data$Family_Income)
>>>>>>> 2a246aad9470e09a36b86834d223a0012a9d2175
