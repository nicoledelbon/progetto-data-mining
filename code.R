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
my_training3 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)

models <- list(model1, model2, model3)

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
my_training3 <- mice(train_und, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)

models_und <- list(model1, model2, model3)

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
my_training3 <- mice(train_ov, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)

models_ov <- list(model1, model2, model3)

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

observed <- data$Family_Income[!is.na(data$Family_Income)]

dev <- sapply(1:5, function(i){
  imp <- complete(data_imp, i)$Family_Income
  abs(mean(imp, na.rm=TRUE) - mean(observed))
})
dati <- complete(data_imp, which.min(dev))

# classificazione ---------------------------------------------------------

set.seed(1)
lab1 <- sample(1:nrow(dati), 0.2*nrow(dati))
dati_trn <- dati[-lab1,]
dati_tst <- dati[lab1,]
dati_trn_no_dropout <- dati[-lab1,-9]
dati_tst_no_dropout <- dati[lab1,-9]
x_train = dati_trn[,-9]
x_test = dati_tst[,-9]
y_train <- dati_trn[,9]
y_test <- dati_tst[, 9]
dati_trn_num <- dati_trn[, c("Family_Income","Attendance_Rate","Assignment_Delay_Days",     
                             "Travel_Time_Minutes","Stress_Index","GPA")]

library(GGally)
ggpairs(dati_trn_num, aes(color = as.factor(dati_trn$Dropout)))

# LDA ---------------------------------------------------------------------

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati_trn) 

plot(mod_lda)

lda_tst_pred = predict(mod_lda, dati_tst)$class

calc_class_err = function(actual, predicted) { 
  mean(actual != predicted)
}

calc_class_err(predicted = lda_tst_pred, actual = y_test)
table(predicted = lda_tst_pred, actual = y_test)
lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
m.lda <- metrics(lda_tab)

# QDA ---------------------------------------------------------------------

mod_qda = qda(Dropout ~ ., data = dati_trn) 

qda_tst_pred = predict(mod_qda, dati_tst)$class

calc_class_err(predicted = qda_tst_pred, actual = y_test) 
qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
m.qda <- metrics(qda_tab)

# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
glm_pred <- ifelse(glm.probs>0.5,1,0)
calc_class_err(predicted = glm_pred, actual = y_test)
glm_tab <- table(glm_pred, y_test)
m.glm <- metrics(glm_tab)

# OTTIMIZZAZIONE SOGLIA-------

soglia <- seq(0.1, 0.9, by = 0.01)
f1_scores <- numeric(length(soglia))

for (i in seq_along(soglia)) {
  pred <- ifelse(glm.probs > soglia[i], 1, 0)
  cm <- table(y_test, pred)
  f1_scores[i] <- metrics(cm)["F1_score"]
}

soglia_opt <- soglia[which.max(f1_scores)]
glm_pred_opt <- ifelse(glm.probs > soglia_opt, 1, 0)

table(glm_pred_opt, y_test)
calc_class_err(predicted = glm_pred_opt, actual = y_test)
opt_tab <- table(y_test, glm_pred_opt)
m.opt <- metrics(opt_tab)

# ALBERO ------------------------------------------------------------------
library(ISLR)
library("tree")
dati_trn$Dropout <- as.factor(dati_trn$Dropout)

tree_dati <- tree(Dropout ~ . , data=dati_trn)
summary(tree_dati)
plot(tree_dati)
text(tree_dati)
tree_dati
tree.pred <- predict(tree_dati , dati_tst , type = "class")
tree_tab = table(tree.pred , y_test)
m.tree=metrics(tree_tab)
calc_class_err(predicted = tree.pred, actual = y_test)

# RANDOM FOREST -----------------------------------------------------------

library(randomForest)
rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
rf_pred <- predict(rf_model, dati_tst)
tab_rf <- table(rf_pred, y_test)
calc_class_err(rf_pred, y_test) 
m.rf = metrics(tab_rf)

tab <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                       calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                       calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))

met <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)

rownames(tab) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
rownames(met) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
cbind(tab,met)
# QDA per test error e GLM_OPT per f1 score

roc = function(y, pred, soglia = seq(0,1,0.001)){
  y = as.numeric(y)-1
  TPR = numeric(length(soglia))
  FPR = numeric(length(soglia))
  for(i in 1:length(soglia)){
    y_pred = ifelse(pred>=soglia[i], 1,0)
    TP <- sum(y == 1 & y_pred == 1)    
    FP <- sum(y == 0 & y_pred == 1)
    FN <- sum(y == 1 & y_pred == 0)
    TN <- sum(y == 0 & y_pred == 0)
    TPR[i] <- TP / (TP + FN)
    FPR[i] <- FP / (FP + TN)}
  data.frame(TPR,FPR)
}

auc = function(roc_res){
  a = 0
  for(i in 1:(nrow(roc_res)-1)){
    a = a - ((roc_res$FPR[i+1]-roc_res$FPR[i])*(roc_res$TPR[i+1]+roc_res$TPR[i])/2)}
  round(a,4)
}

roc_obj <- roc(y_test, glm.probs)
plot(roc_obj$FPR, roc_obj$TPR, type="l", col="blue",xlim=c(0,1), ylim=c(0,1),
     xlab="False Positive Rate", ylab="True Positive Rate",
     main="Curva ROC")
abline(0,1,lty=2, col="gray")
auc(roc_obj) #0.8345

qda_probs <- predict(mod_qda, dati_tst)$posterior[,2]
roc_qda <- roc(y_test, qda_probs)
lines(roc_qda$FPR, roc_qda$TPR, col="red")
auc(roc_qda) # 0.8314

lda_probs <- predict(mod_lda, dati_tst)$posterior[,2]
roc_lda <- roc(y_test, lda_probs)
lines(roc_lda$FPR, roc_lda$TPR, col="green")
auc(roc_lda) # 0.8342

rf_probs <- predict(rf_model, dati_tst, type = "prob")[,2]
roc_rf <- roc(y_test, rf_probs)
lines(roc_rf$FPR, roc_rf$TPR, type="l", col="black",xlim=c(0,1), ylim=c(0,1))
auc(roc_rf) # 0.8192

legend("topright", c("glm", "QDA", "LDA", "RF"), col=c("blue","red","green", "black"), lty=1, lwd=3)
