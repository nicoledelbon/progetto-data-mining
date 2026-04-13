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

eval = function(models, val){
  accuracies <- numeric(length(models))
  f1 <- numeric(length(models))
  
  for (i in seq_along(models)) {
    prediction <- lapply(getfit(models[[i]]), predict, 
                         se.fit = TRUE, newdata = val, type = "response")
    single_prediction <- sapply(prediction, `[[`, "fit")
    final_pred <- apply(single_prediction, 1, mean)
    final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
    confusion_matrix <- table(val$Dropout, final_pred_class)
    accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    f1[i] <- f1_score(confusion_matrix)
  }
  
  rbind(accuracies, f1)
}

eval(models,validation)

# mean

p <- dim(train_rid)[2]
my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 
imp_train <- mice(train_rid, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
train_rid <- complete(imp_train,1)

# imputo nel test o faccio mice?
validation$Family_Income[is.na(validation$Family_Income)] <- mean(train_rid$Family_Income, na.rm=TRUE)
validation$Stress_Index[is.na(validation$Stress_Index)] <- mean(train_rid$Stress_Index, na.rm=TRUE)

test$Family_Income[is.na(test$Family_Income)] <- mean(train_rid$Family_Income, na.rm=TRUE)
test$Stress_Index[is.na(test$Stress_Index)] <- mean(train_rid$Stress_Index, na.rm=TRUE)

data0 <- train_rid[train_rid$Dropout==0,]
data1 <- train_rid[train_rid$Dropout==1,]

metriche <- list()
metodi <- c("dati normali", "undersampling", "oversampling")
for(m in metodi){
  if(m=="dati normali"){
    dati_trn <- train_rid
  }
  if(m=="undersampling"){
    set.seed(1)
    lab <- sample(1:nrow(data0), nrow(data1))
    dati_trn<- rbind(data0[lab,], data1)
  }
  if(m=="oversampling"){
    set.seed(1)
    lab <- sample(1:nrow(data1), nrow(data0), replace=T)
    dati_trn <- rbind(data1[lab,], data0)
  }
  
  dati_tst <- test
  y_test <- dati_tst$Dropout
  
  library(MASS)
  mod_lda = lda(Dropout ~ ., data = dati_trn) 
  lda_tst_pred = predict(mod_lda, dati_tst)$class
  lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
  m.lda <- unlist(metrics(lda_tab))
  
  # QDA ---------------------------------------------------------------------
  mod_qda = qda(Dropout ~ ., data = dati_trn) 
  qda_tst_pred = predict(mod_qda, dati_tst)$class
  qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
  m.qda <- unlist(metrics(qda_tab))
  
  # LOGISTICA ---------------------------------------------------------------
  glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
  glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
  glm_pred <- ifelse(glm.probs>0.5,1,0)
  glm_tab <- table(glm_pred, y_test)
  m.glm <- unlist(metrics(glm_tab))
  
  # OTTIMIZZAZIONE SOGLIA-------
  glm.probs_val <- predict(glm_fit, newdata=validation, type="response")
  y_val <- validation[, 9]
  soglia <- seq(0.1, 0.9, by = 0.01)
  f1_scores <- numeric(length(soglia))
  for (i in seq_along(soglia)) {
    pred_val <- ifelse(glm.probs_val > soglia[i], 1, 0)
    cm_val <- table(y_val, pred_val)
    f1_scores[i] <- metrics(cm_val)$F1_score
  }
  soglia_opt <- soglia[which.max(f1_scores)]
  glm_pred_opt <- ifelse(glm.probs > soglia_opt, 1, 0)
  opt_tab <- table(y_test, glm_pred_opt)
  m.opt <- unlist(metrics(opt_tab))
  
  # ALBERO ------------------------------------------------------------------
  library(ISLR)
  library("tree")
  dati_trn$Dropout <- as.factor(dati_trn$Dropout)
  tree_dati <- tree(Dropout ~ . , data=dati_trn)
  summary(tree_dati)
  tree.pred <- predict(tree_dati , dati_tst , type = "class")
  tree_tab = table(tree.pred , y_test)
  m.tree=unlist(metrics(tree_tab))
  
  # RANDOM FOREST -----------------------------------------------------------
  library(randomForest)
  rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
  rf_pred <- predict(rf_model, dati_tst)
  tab_rf <- table(rf_pred, y_test)
  m.rf = unlist(metrics(tab_rf))
  
  tab <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                         calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                         calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))
  met <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)
  rownames(tab) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
  rownames(met) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
  metriche[[m]] <- cbind(tab,met)
}
