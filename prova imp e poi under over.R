rm(list=ls())
library("visdat")
library("naniar")
library(mice)
library(ggcorrplot)
library(GGally)
library(MASS)
library(ISLR)
library("tree")
library(randomForest)
library(gridExtra)
student_dropout_dataset_v3 <- read.csv("student_dropout_dataset_v3.csv")

# rimozione ID
data <- student_dropout_dataset_v3[,-1]

# trasformazione delle variabili in factor
fattori <- c("Gender","Internet_Access","Semester","Department",
             "Parental_Education","Part_Time_Job","Scholarship","Dropout")
data[fattori] <- lapply(data[fattori], as.factor)
summary(data)

# normalizzazione
min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
num_cols <- sapply(data, is.numeric)
data[num_cols] <- lapply(data[num_cols], min_max)
summary(data)

data_numeric <- data[, c("Age","Family_Income", "Study_Hours_per_Day" ,"Attendance_Rate","Assignment_Delay_Days",
                         "Travel_Time_Minutes" ,"Stress_Index" , "GPA","Semester_GPA" ,"CGPA")]

# boxplot
par(mfrow=c(2,5))
for(i in 1:ncol(data_numeric)){
  boxplot(data_numeric[,i] ~ data$Dropout, col=c("green", "red"), ylab=colnames(data_numeric)[i],
          main="", pch=16)
}
par(mfrow=c(1,1))

# grafici per indagare missing values
md.pattern(data)
p1 <- vis_miss(data)
p2 <- gg_miss_var(data)
grid.arrange(p1, p2, ncol=2)

# analisi delle correlazioni
cor <- cor(data_numeric, use="complete.obs")
ggcorrplot(cor, lab=T, hc.order=T)

# selezione delle variabili
mfull <- glm(Dropout ~ . , data=data, family="binomial")
mnull <- glm(Dropout ~ 1 , data=data, family="binomial")
step(mfull, scope = list(lower = mnull, upper = mfull), direction = "both")

data <- data[, (names(data) %in% c("Family_Income", "Internet_Access",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout"))]

data_numeric <- data_numeric[, names(data_numeric) %in% c("Family_Income", "Attendance_Rate",
                                                          "Assignment_Delay_Days", "Travel_Time_Minutes",
                                                          "Stress_Index","GPA")]

# train-test split
set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

# imputazione  ------------------------------------------------------------

# metrica di valutazione
f1_score <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

# creazione del validation set
set.seed(1)
lab_rid <- sample(1:nrow(train), 0.8*nrow(train))
train_rid <- train[lab_rid,]
validation <- train[-lab_rid,]

p <- dim(train_rid)[2]
my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 

library(mice)
my_training1 <- mice(train_rid, method = "pmm", predictorMatrix = my_predictorMatrix, 
                     seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_rid, method = "mean", predictorMatrix = my_predictorMatrix, 
                     seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_rid, method = "lasso.norm", predictorMatrix = my_predictorMatrix, 
                     seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train_rid, method = "cart", predictorMatrix = my_predictorMatrix, 
                     seed = 1234,  printFlag = FALSE)

model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)

models <- list(model1, model2, model3, model4)

# funzione per confrontare accuracy e f1 score
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

ev <- eval(models,validation)
colnames(ev) <- c("pmm", "mean", "lasso.norm", "cart")

# imputazione con metodo migliore
p <- dim(train_rid)[2]
my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,9] <- 0 
imp_train <- mice(train_rid, method = "mean", predictorMatrix = my_predictorMatrix,
                  seed = 1234,  printFlag = FALSE)
train_rid <- complete(imp_train,1)

densityplot(imp_train)

# imputazione nel test e nel validation
validation$Family_Income[is.na(validation$Family_Income)] <- mean(train_rid$Family_Income, na.rm=TRUE)
validation$Stress_Index[is.na(validation$Stress_Index)] <- mean(train_rid$Stress_Index, na.rm=TRUE)
test$Family_Income[is.na(test$Family_Income)] <- mean(train_rid$Family_Income, na.rm=TRUE)
test$Stress_Index[is.na(test$Stress_Index)] <- mean(train_rid$Stress_Index, na.rm=TRUE)

table(train_rid$Dropout)  

data0 <- train_rid[train_rid$Dropout==0,]
data1 <- train_rid[train_rid$Dropout==1,]

# classificazione ---------------------------------------------------------

# metriche di valutazione
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
  dati_trn$Dropout <- as.factor(dati_trn$Dropout)
  tree_dati <- tree(Dropout ~ . , data=dati_trn)
  summary(tree_dati)
  tree.pred <- predict(tree_dati , dati_tst , type = "class")
  tree_tab = table(tree.pred , y_test)
  m.tree=unlist(metrics(tree_tab))
  
  # RANDOM FOREST -----------------------------------------------------------
  rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=300)
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

metriche 

# curva ROC e AUC per l'oversampling
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

roc_glm <- roc(y_test, glm.probs)
plot(roc_glm$FPR, roc_glm$TPR, type="l", col="blue",xlim=c(0,1), ylim=c(0,1),
     xlab="False Positive Rate", ylab="True Positive Rate",
     main="Curva ROC - oversampling")
abline(0,1,lty=2, col="gray")
auc_glm=auc(roc_glm)

qda_probs <- predict(mod_qda, dati_tst)$posterior[,2]
roc_qda <- roc(y_test, qda_probs)
lines(roc_qda$FPR, roc_qda$TPR, col="red")
auc_qda=auc(roc_qda)

lda_probs <- predict(mod_lda, dati_tst)$posterior[,2]
roc_lda <- roc(y_test, lda_probs)
lines(roc_lda$FPR, roc_lda$TPR, col="green")
auc_lda=auc(roc_lda)

rf_probs <- predict(rf_model, dati_tst, type = "prob")[,2]
roc_rf <- roc(y_test, rf_probs)
lines(roc_rf$FPR, roc_rf$TPR, type="l", col="black",xlim=c(0,1), ylim=c(0,1))
auc_rf=auc(roc_rf)

auc_res = list(auc_glm, auc_qda,auc_lda,auc_rf)

legend("bottomright", paste(c("GLM", "QDA", "LDA", "RF"), "- AUC:", auc_res), , col=c("blue","red","green", "black"), lty=1, lwd=3)

library(caret)
cv = function(train, m, k){
  set.seed(123) 
  folds <- createFolds(train$Dropout, k = k, list = FALSE)
  res <- list(
    LDA     = matrix(NA, nrow=k, ncol=5),
    QDA     = matrix(NA, nrow=k, ncol=5),
    GLM     = matrix(NA, nrow=k, ncol=5),
    TREE    = matrix(NA, nrow=k, ncol=5),
    RF      = matrix(NA, nrow=k, ncol=5))
  
  colnames(res$LDA) <- colnames(res$QDA) <- colnames(res$GLM) <- 
    colnames(res$TREE) <- colnames(res$RF) <-
    c("Precision","Recall","Specificity","Accuracy","F1")
  
  make_cm <- function(true, pred){
    table(factor(true, levels=c(0,1)),
          factor(pred, levels=c(0,1)))
  }
  
  for (f in 1:k) {
    train_fold <- train[folds != f, ]
    val_fold   <- train[folds == f, ]
    y_val <- val_fold$Dropout
    
    data0 <- train_fold[train_fold$Dropout == 0, ]
    data1 <- train_fold[train_fold$Dropout == 1, ]
    
    if(m=="dati normali"){
      dati_trn <- train_fold
    }
    if(m=="undersampling"){
      lab <- sample(1:nrow(data0), nrow(data1))
      dati_trn<- rbind(data0[lab,], data1)
    }
    if(m=="oversampling"){
      lab <- sample(1:nrow(data1), nrow(data0), replace=T)
      dati_trn <- rbind(data1[lab,], data0)
    }
    
    
    mod_lda = lda(Dropout ~ ., data = dati_trn) 
    lda_tst_pred = predict(mod_lda, val_fold)$class
    res$LDA[f, ] <- unlist(metrics(make_cm(y_val, lda_tst_pred)))
    
    # QDA ---------------------------------------------------------------------
    mod_qda = qda(Dropout ~ ., data = dati_trn) 
    qda_tst_pred = predict(mod_qda, val_fold)$class
    res$QDA[f, ] <- unlist(metrics(make_cm(y_val, qda_tst_pred)))
    
    # LOGISTICA ---------------------------------------------------------------
    glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
    glm.probs <- predict(glm_fit, newdata= val_fold,type = "response")
    glm_pred <- ifelse(glm.probs>0.5,1,0)
    res$GLM[f, ] <- unlist(metrics(make_cm(y_val, glm_pred)))
    
    # ALBERO ------------------------------------------------------------------
    tree_dati <- tree(Dropout ~ . , data=dati_trn)
    tree.pred <- predict(tree_dati , val_fold , type = "class")
    res$TREE[f, ] <- unlist(metrics(make_cm(y_val, tree.pred)))
    
    # RANDOM FOREST -----------------------------------------------------------
    rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=300)
    rf_pred <- predict(rf_model, val_fold)
    res$RF[f, ] <- unlist(metrics(make_cm(y_val, rf_pred)))
  }
  
  final <- matrix(0, nrow=5, ncol=5)
  rownames(final) <- names(res)
  colnames(final) <- c("Precision","Recall","Specificity","Accuracy","F1")
  
  for(i in 1:5){
    final[i, ] <- colMeans(res[[i]], na.rm=TRUE)
  }
  
  print(round(final, 4))
}

cv(train_rid, "dati normali", 5)
cv(train_rid, "undersampling", 5)
cv(train_rid, "oversampling", 5)

