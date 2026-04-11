# Dubbi
# teniamo tutte le variabili? meglio solo quelle che spiegano dropout?
# usare anche meyodo cart?
# usare with e un poool invece che glm.mids

rm(list=ls())

student_dropout_dataset_v3 <- read.csv("student_dropout_dataset_v3.csv")

# Obiettivo della mia analisi
# La nostra analisi ha lo scopo di prevedere se uno studente abbandona il ciclo di studi prima di terminarlo.
# Al fine di prevedere l'abbandono, abbiamo analizzato un dataset contenente diverse informazioni,
# sui dati personali dello studente e sulla sua educazione, ma anche sulla famiglia
# e le possibilità che ha a disposizione ( es: accesso a Internet)

summary(student_dropout_dataset_v3)
# summary per capire le variabili in esame
# Ho dei valori mancanti nelle variabili Family_Income (dato sensibile)
# Study_Hours_per_Day, Stress_index
# I range di variazione delle variabili sono coerenti con le variabili
# in esame

# La variabile ID non è utile alle mie analisi, la rimuovo
data <- student_dropout_dataset_v3[,-1]

fattori <- c("Gender","Internet_Access","Semester","Department",
             "Parental_Education","Part_Time_Job","Scholarship","Dropout")

data[fattori] <- lapply(data[fattori], as.factor)



summary(data)
# Ho delle variabili numeriche e delle variabili dummy

# analisi grafica ---------------------------------------------------------
colnames(data)
data_numeric <- data[, c("Family_Income",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout")]
par(mfrow=c(3,3))
for(i in 1:(ncol(data_numeric)-1)){
  plot(data_numeric[,i], col=data$Dropout, ylab=colnames(data_numeric)[i],
       main="", pch=16)
  legend("left", levels(data$Dropout), col=c("black","red"), pch=16)
}
par(mfrow=c(1,1))
library(ggplot2)

par(mfrow=c(3,3))
for(i in 1:(ncol(data_numeric)-1)){
  boxplot(data_numeric[,i] ~ data$Dropout,
          xlab="Dropout", ylab=colnames(data_numeric)[i],
          col=c("green","red"))
}
par(mfrow=c(1,1))

# visulaizzazione dei missing
library("visdat")
library("naniar")

vis_miss(data)
# I missing sono solo in 3 variabili e sembrano essere tutti nelle stesse osservazioni
# I missing sono solo in piccola parte, se non trovo un metodo di imputazione valido
# potrei anche rimuoverli. 

gg_miss_var(data)
# 500 osservazioni contengono missing in 3 variabili 
# le altre variabili non ne contengono 
# (potrebbe essere che queste variabili non erano 
# obbligatorie nella compilazione del questionario)

#library("UpSetR")
#upset(as_shadow_upset(data))

library(mice)
md.pattern(data)


cor <- cor(data_numeric[,-10], use="complete.obs")
# selezione variabili se necessaria ---------------------------------------
library(ggcorrplot)
ggcorrplot(cor, lab=T, hc.order=T)


mfull <- glm(Dropout ~ . , data=data, family="binomial")
mnull <- glm(Dropout ~ 1 , data=data, family="binomial")
step(mfull, mnull)
# Le variabili Semester GPA, CGPA e GPA sono collineari
# infatti sono tutti indicatori della media dei voti. 
# applico una selezione della variabili

# GPA è calcolato sull'anno, CGPA è quello cumulato di tutti gli anni,
# semester GPA è quello calcolato nel semestre corrente
# quindi io terrei CGPA


# Tengo solo un valore per la media, al fine di evitare dati ridondandi
data <- data[, !(names(data) %in% c("Semester_GPA", "GPA"))]
data_numeric <- data_numeric[, !(names(data_numeric) %in% c("Semester_GPA", "GPA"))]

cor(data_numeric[,-8], use="complete.obs")

table(data$Dropout)  
# Dai dati originali, vedo che molti studenti completano i loro studi, mentre chi abbandona è
# in numero minore. Siamo quindi in presenza di dati sbilanciati. 
# Prima dell'imputazione quindi, al fine di evitare distorsioni
# nelle nostre analisi, abbiamo pensato di dividere i dati originali in train e test set, e 
# provare ad eseguire oversampling, undersampling ed eseguire le analisi sui dati originali.
# Per ogni scenario abbiamo imputato i dati in diverse modalità (pmm - cart - norm - mean)
# e valutato il risultato migliore.


f1_score <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

# provo con i dati cosi, under e oversampling -----------------------------

# divisione train test ----------------------------------------------------

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

data0 <- train[train$Dropout==0,]
data1 <- train[train$Dropout==1,]


# uso i dati come sono ----------------------------------------------------


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,16] <- 0 

library(mice)
my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)


model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)


# Lista dei modelli
models <- list(model1, model2, model3, model4)

# Lista per salvare le accuracy
accuracies <- numeric(length(models))
f1 <- numeric(length(models))

for (i in seq_along(models)) {
  prediction <- lapply(getfit(models[[i]]), predict, 
                       se.fit = TRUE, newdata = test, type = "response")
  single_prediction <- sapply(prediction, `[[`, "fit")
  final_pred <- apply(single_prediction, 1, mean)
  final_pred_class <- ifelse(final_pred > 0.4, 1, 0)
  confusion_matrix <- table(test$Dropout, final_pred_class)
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  f1[i] <- f1_score(confusion_matrix)
}

# Risultati
accuracies
#  0.8201363 0.8201363 0.8196120
f1
# 0.5134752 0.5162200 0.5113636

# Accuracy alta nei dati originali, ma F1 basso --> unbalanced data
# come imputazioni sono all'incirca simili, il mean semvra essere il migliore


# undersampling -----------------------------------------------------------

set.seed(1)
lab <- sample(1:nrow(data0), nrow(data1))
train_und <- rbind(data0[lab,], data1)

table(train_und$Dropout)


p <- dim(train_und)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,16] <- 0 

library(mice)
my_training1 <- mice(train_und, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_und, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_und, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train_und, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)


model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Dropout ~ ., family="binomial", data = my_training4)

# Lista dei modelli
models_under <- list(model1, model2, model3, model4)

# Lista per salvare le accuracy
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

# Risultati
accuracies
# 0.7378081 0.7372837 0.7372837
f1
# 0.5711835 0.5699571 0.5699571


# oversampling -----------------------------------------------------------

set.seed(1)

lab <- sample(1:nrow(data1), nrow(data0), replace=T)
train_ov <- rbind(data1[lab,], data0)
table(train_ov$Dropout)

p <- dim(train_ov)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,16] <- 0 

library(mice)
my_training1 <- mice(train_ov, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train_ov, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_ov, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train_ov, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)


model1 <- glm.mids(Dropout ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Dropout ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Dropout ~ ., family="binomial", data = my_training3)

# Lista dei modelli
models <- list(model1, model2, model3)

# Lista per salvare le accuracy
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

# Risultati
accuracies
# 0.7346618 0.7367593 0.7346618
f1
# 0.5660377 0.5679862 0.5660377


# - -----------------------------------------------------------------------

# L'undersampling è la tecnica che dà risultati migliori, considerando sia l'accuracy che  F1_score,
# metrica più adatta a valutare la precisione delle stime in presenza di dati sbilanciati.
# In particolare, Il metodo mean risulta il migliore in termini di imputazione.
# Abbiamo quindi eseguito un oversampling sulla totalità dei dati originali e imputato
# i valori mancanti tramite mean.

# - -----------------------------------------------------------------------

# --> migliore è mean

# imputazione di tutti i valori con mean e oversampling -------------------

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
data <- data[, !(names(data) %in% c("Semester_GPA", "GPA"))]

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]

data0 <- train[train$Dropout==0,]
data1 <- train[train$Dropout==1,]

set.seed(1)
lab <- sample(1:nrow(data0), nrow(data1))
train <- rbind(data0[lab,], data1)

p <- dim(data)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,16] <- 0 

library(mice)

data_imp <- mice(data, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)

densityplot(data_imp)
# --> family income?



# Classificazione ---------------------------------------------------------
dati <- complete(data_imp, 1)
# Voglio prevedere l'abbandono o meno di uno studente. Per fare ciò, eseguo una classificazione
# dei dati in base alla variabile Dropout. Se ottengo 0 significa che lo studente proseguirà i suoi
# studi, 1 invece indica l'abbandono.
# Decido di selezionare il primo set di dati ottenuto dalla prima imputazione (?).
# Divido i dati nuovamente in test e train, e valuto il miglior modello di classificazione.

set.seed(1)
lab1 <- sample(1:nrow(dati), 0.2*nrow(dati))
dati_trn <- dati[-lab1,]
dati_tst <- dati[lab1,]
dati_trn_no_dropout <- dati[-lab1,-16]
dati_tst_no_dropout <- dati[lab1,-16]
x_train = dati_trn[,-16]
x_test = dati_tst[,-16]
y_train <- dati_trn[,16]
y_test <- dati_tst[, 16]

dati_trn_num <- dati_trn[, c("Age","Family_Income"  , "Study_Hours_per_Day" ,"Attendance_Rate" ,     
                             "Travel_Time_Minutes" ,"Stress_Index","CGPA", "Dropout")]

# visualizzazione dei dati imputati di train
library(GGally)
ggpairs(dati_trn_num[,-8], aes(color = as.factor(dati_trn_num$Dropout)))
# l'assunzione di normalità non è proprio verificata, non mi aspetto che
# LDA sia il miglior metodo

# LDA ---------------------------------------------------------------------

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati_trn) 

plot(mod_lda)

lda_tst_pred = predict(mod_lda, dati_tst)$class

calc_class_err = function(actual, predicted) { 
  mean(actual != predicted)
}


calc_class_err(predicted = lda_tst_pred, actual = y_test)
# 0.263898
table(predicted = lda_tst_pred, actual = y_test)


lda_tab <- table(predicted = lda_tst_pred, actual = y_test)
m.lda <- metrics(lda_tab)


# QDA ---------------------------------------------------------------------

mod_qda = qda(Dropout ~ ., data = dati_trn) 

qda_tst_pred = predict(mod_qda, dati_tst)$class

calc_class_err(predicted = qda_tst_pred, actual = y_test) 
# 0.2609549

qda_tab <- table(predicted = qda_tst_pred, actual = y_test)
m.qda <- metrics(qda_tab)


# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")

glm_pred <- ifelse(glm.probs>0.5,1,0)

calc_class_err(predicted = glm_pred, actual = y_test)
# 0.2661871
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
# 0.2753434
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
# 0.2786135



# RANDOM FOREST -----------------------------------------------------------

library(randomForest)
rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
rf_pred <- predict(rf_model, dati_tst)
tab_rf <- table(rf_pred, y_test)
calc_class_err(rf_pred, y_test) # l'errore è solamente l'8,4%
m.rf = metrics(tab_rf)
# risultati altissimi; mostra un modello molto significativo



# confronti ----------------------------------------------------------------

tab <- cbind(Error = c(calc_class_err(lda_tst_pred, y_test), calc_class_err(qda_tst_pred, y_test), 
                       calc_class_err(glm_pred, y_test), calc_class_err(glm_pred_opt, y_test),
                       calc_class_err(tree.pred, y_test), calc_class_err(rf_pred, y_test)))
# qda migliore

met <- rbind(m.lda, m.qda, m.glm, m.opt, m.tree, m.rf)

rownames(tab) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
rownames(met) <- c("LDA", "QDA", "GLM", "GLM_OPT", "TREE", "Random Forest")
cbind(tab,met)
# Qda - minor tasso di error rate
# GLM opt - Migliore secondo F1

## ROC su GLM
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
auc(roc_obj)
# indica una buona capacità del modello nella separazione delle classi

## ROC su QDA
qda_probs <- predict(mod_qda, dati_tst)$posterior[,2]
roc_qda <- roc(y_test, qda_probs)
lines(roc_qda$FPR, roc_qda$TPR, col="red")
auc(roc_qda)
# anche AUC di QDA porta a dire che il modello ha una buona capcità discriminante

lda_probs <- predict(mod_lda, dati_tst)$posterior[,2]
roc_lda <- roc(y_test, lda_probs)
lines(roc_lda$FPR, roc_lda$TPR, col="green")
auc(roc_lda)
# l'AUC di lda è molto buono


## ROC su Random forest
rf_probs <- predict(rf_model, dati_tst, type = "prob")[,2]
roc_rf <- roc(y_test, rf_probs)
lines(roc_rf$FPR, roc_rf$TPR, type="l", col="black",xlim=c(0,1), ylim=c(0,1))
auc(roc_rf) # 0.9806, quasi perfetto!!! forte capacità preditivva, ma attenzione all'overfitting

legend("topright", c("glm", "QDA", "LDA", "RF"), col=c("blue","red","green", "black"), lty=1, lwd=3)



# nonostante auc di glm e lda siano molto simili e indicano una buona capacità
# del modello di separare bene le classi, questa metrica è indipendente dalla
# soglia. Infatti, per una migliore classificazione è meglio usare i glm che 
# risultano avere valori più alti in termini di F1-score.


### CROSS - VALIDATION .... magari questo si può fare.

## ciao
