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

data <- data[, (names(data) %in% c("Family_Income", "Internet_Access",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout"))]

f1_score <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

calc_class_err = function(actual, predicted) { 
  mean(actual != predicted)
}

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

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

# imputazione  ----------------------------------------------------
lab_rid <- sample(1:nrow(train), 0.8*nrow(train))
train_rid <- train[lab_rid,]
validation <- train[-lab_rid,]

p <- dim(train_rid)[2]
my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
my_training1 <- mice(train_rid, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_rid, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_rid, method = "lasso.norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train_rid, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

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
                         se.fit = TRUE, newdata = validation, type = "response")
    single_prediction <- sapply(prediction, `[[`, "fit")
    final_pred <- apply(single_prediction, 1, mean)
    final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
    confusion_matrix <- table(validation$Dropout, final_pred_class)
    accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    f1[i] <- f1_score(confusion_matrix)
  }
  
  rbind(accuracies, f1)
}

eval(models,validation)

# mean

p <- dim(train)[2]
my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 
imp_train <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
train <- complete(imp_train,1)
imp_train$imp$Family_Income[1,1] #  38419.03
imp_train$imp$Stress_Index[1,1] #  5.510391
# imputo nel test o faccio mice?
test$Family_Income[is.na(test$Family_Income)] <-  38419.03
test$Stress_Index[is.na(test$Stress_Index)] <-   5.510391

data0 <- train[train$Dropout==0,]
data1 <- train[train$Dropout==1,]


# cosi come sono

dati_trn <- train
dati_tst <- test
y_test <- dati_tst[, 9]


# LDA ---------------------------------------------------------------------

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati_trn) 
lda_tst_pred = predict(mod_lda, dati_tst)$class
calc_class_err(predicted = lda_tst_pred, actual = y_test)
# 0.183
table(predicted = lda_tst_pred, actual = y_test)
lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
m.lda <- metrics(lda_tab)


# QDA ---------------------------------------------------------------------
mod_qda = qda(Dropout ~ ., data = dati_trn) 
qda_tst_pred = predict(mod_qda, dati_tst)$class
calc_class_err(predicted = qda_tst_pred, actual = y_test) 
# 0.185
qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
m.qda <- metrics(qda_tab)


# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
glm_pred <- ifelse(glm.probs>0.5,1,0)
calc_class_err(predicted = glm_pred, actual = y_test)
# 0.1815
glm_tab <- table(glm_pred, y_test)
calc_class_err(predicted = glm_pred, actual = y_test)
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
#0.2075
opt_tab <- table(y_test, glm_pred_opt)
m.opt <- metrics(opt_tab)

# ALBERO ------------------------------------------------------------------
library(ISLR)
library("tree")
dati_trn$Dropout <- as.factor(dati_trn$Dropout)
tree_dati <- tree(Dropout ~ . , data=dati_trn)
summary(tree_dati)
tree.pred <- predict(tree_dati , dati_tst , type = "class")
tree_tab = table(tree.pred , y_test)
m.tree=metrics(tree_tab)
calc_class_err(predicted = tree.pred, actual = y_test)
# 0.195




# RANDOM FOREST -----------------------------------------------------------

library(randomForest)
rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
rf_pred <- predict(rf_model, dati_tst)
tab_rf <- table(rf_pred, y_test)
calc_class_err(rf_pred, y_test) 
# 0.1965
m.rf = metrics(tab_rf)
# risultati altissimi; mostra un modello molto significativo

# confronti under ----------------------------------------------------------------

tab <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                           calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                           calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))
met <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)
rownames(tab) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
rownames(met) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
cbind(tab,met)



# under 
set.seed(1)
lab <- sample(1:nrow(data0), nrow(data1))
train_und <- rbind(data0[lab,], data1)

table(train_und$Dropout)

dati_trn <- train_und
dati_tst <- test
y_test <- dati_tst[, 9]


# LDA ---------------------------------------------------------------------

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati_trn) 
lda_tst_pred = predict(mod_lda, dati_tst)$class
calc_class_err(predicted = lda_tst_pred, actual = y_test)
#  0.27
table(predicted = lda_tst_pred, actual = y_test)
lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
m.lda <- metrics(lda_tab)


# QDA ---------------------------------------------------------------------
mod_qda = qda(Dropout ~ ., data = dati_trn) 
qda_tst_pred = predict(mod_qda, dati_tst)$class
calc_class_err(predicted = qda_tst_pred, actual = y_test) 
# 0.2775
qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
m.qda <- metrics(qda_tab)


# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
glm_pred <- ifelse(glm.probs>0.5,1,0)
calc_class_err(predicted = glm_pred, actual = y_test)
# 0.2645
glm_tab <- table(glm_pred, y_test)
calc_class_err(predicted = glm_pred, actual = y_test)
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
# 0.204
opt_tab <- table(y_test, glm_pred_opt)
m.opt <- metrics(opt_tab)

# ALBERO ------------------------------------------------------------------
library(ISLR)
library("tree")
dati_trn$Dropout <- as.factor(dati_trn$Dropout)
tree_dati <- tree(Dropout ~ . , data=dati_trn)
summary(tree_dati)
tree.pred <- predict(tree_dati , dati_tst , type = "class")
tree_tab = table(tree.pred , y_test)
m.tree=metrics(tree_tab)
calc_class_err(predicted = tree.pred, actual = y_test)
#  0.244



# RANDOM FOREST -----------------------------------------------------------

library(randomForest)
rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
rf_pred <- predict(rf_model, dati_tst)
tab_rf <- table(rf_pred, y_test)
calc_class_err(rf_pred, y_test) 
# 0.2715
m.rf = metrics(tab_rf)
# risultati altissimi; mostra un modello molto significativo

# confronti under ----------------------------------------------------------------

tab_und <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                       calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                       calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))
met_und <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)
rownames(tab_und) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
rownames(met_und) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
cbind(tab_und,met_und)


# over


set.seed(1)
lab <- sample(1:nrow(data1), nrow(data0), replace=T)
train_ov <- rbind(data1[lab,], data0)


table(train_ov$Dropout)

dati_trn <- train_ov
dati_tst <- test
y_test <- dati_tst[, 9]


# LDA ---------------------------------------------------------------------

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati_trn) 
lda_tst_pred = predict(mod_lda, dati_tst)$class
calc_class_err(predicted = lda_tst_pred, actual = y_test)
#  0.2625
table(predicted = lda_tst_pred, actual = y_test)
lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
m.lda <- metrics(lda_tab)


# QDA ---------------------------------------------------------------------
mod_qda = qda(Dropout ~ ., data = dati_trn) 
qda_tst_pred = predict(mod_qda, dati_tst)$class
calc_class_err(predicted = qda_tst_pred, actual = y_test) 
# 0.275
qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
m.qda <- metrics(qda_tab)


# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
glm_pred <- ifelse(glm.probs>0.5,1,0)
calc_class_err(predicted = glm_pred, actual = y_test)
# 0.261
glm_tab <- table(glm_pred, y_test)
calc_class_err(predicted = glm_pred, actual = y_test)
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
# 0.2025
opt_tab <- table(y_test, glm_pred_opt)
m.opt <- metrics(opt_tab)

# ALBERO ------------------------------------------------------------------
library(ISLR)
library("tree")
dati_trn$Dropout <- as.factor(dati_trn$Dropout)
tree_dati <- tree(Dropout ~ . , data=dati_trn)
summary(tree_dati)
tree.pred <- predict(tree_dati , dati_tst , type = "class")
tree_tab = table(tree.pred , y_test)
m.tree=metrics(tree_tab)
calc_class_err(predicted = tree.pred, actual = y_test)
#  0.2955



# RANDOM FOREST -----------------------------------------------------------

library(randomForest)
rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
rf_pred <- predict(rf_model, dati_tst)
tab_rf <- table(rf_pred, y_test)
calc_class_err(rf_pred, y_test) 
# 0.2085
m.rf = metrics(tab_rf)
# risultati altissimi; mostra un modello molto significativo

# confronti under ----------------------------------------------------------------

tab_ov <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                           calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                           calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))
met_ov <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)
rownames(tab_ov) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
rownames(met_ov) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
cbind(tab_ov,met_ov)


