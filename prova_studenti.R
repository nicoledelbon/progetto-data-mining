# POSSIBILI CORREZIONI DA VALUTARE
# CAMBIA LA SOGLIA NEL GLM
#thresholds <- seq(0.1, 0.9, by = 0.01)
#f1_scores <- numeric(length(thresholds))

#for (i in seq_along(thresholds)) {
 # pred <- ifelse(glm.probs > thresholds[i], 1, 0)
  #cm <- table(y_test, pred)
#  f1_scores[i] <- f1_score(cm)
#}

#best_thresh <- thresholds[which.max(f1_scores)]
#best_thresh
# glm_pred <- ifelse(glm.probs > best_thresh, 1, 0)

# PROVA RANDOM FOREST
#library(randomForest)
#rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=200)
#rf_pred <- predict(rf_model, dati_tst)
#table(rf_pred, y_test)
#calc_class_err(rf_pred, y_test)



rm(list=ls())

f1_score <- function(cm) {
  TP <- cm[2,2]
  FP <- cm[1,2]
  FN <- cm[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

student_dropout_dataset_v3 <- read.csv("~/progetto-data-mining/student_dropout_dataset_v3.csv")

# Obiettivo della mia analisi
# La nostra analisi ha lo scopo di prevedere se uno studente abbandona il suo ciclo di studi prima di terminarlo.
# Al fine di prevedere l'abbandono, abbiamo analizzato un dataset contenente diverse informazioni,
# sia sui dati personali dello studente, sulla sua educazione e studi precedenti, ma anche sulla famiglia
# e dove vive lo studente e le possibilità che ha a disposizione ( es: accesso a Internet)

summary(student_dropout_dataset_v3)
# summary per capire le variabili in esame
# Ho dei valori mancanti nelle variabili Family_Income (dato sensibile)
# Study_Hours_per_Day, Stress_index
# I range di variazione delle variabili sono coerenti con le variabili
# in esame


data <- student_dropout_dataset_v3[,-1]
data$Gender <- as.factor(data$Gender)
data$Internet_Access <- as.factor(data$Internet_Access)
data$Semester <- as.factor(data$Semester)
data$Department <- as.factor(data$Department)
data$Parental_Education <- as.factor(data$Parental_Education)
data$Part_Time_Job <- as.factor(data$Part_Time_Job)
data$Scholarship <- as.factor(data$Scholarship)
data$Dropout <- as.factor(data$Dropout )
data$Assignment_Delay_Days <- as.factor(data$Assignment_Delay_Days)

# La variabile ID non è utile alle mie analisi, la rimuovo
summary(data)
# Ho delle variabili numeriche e delle variabili dummy


# analisi grafica ---------------------------------------------------------
colnames(data)
data_numeric <- data[, c("Age","Family_Income"  , "Study_Hours_per_Day" ,"Attendance_Rate"    
                         , "Travel_Time_Minutes" ,"Stress_Index" , "GPA"                  
                        ,"Semester_GPA" ,"CGPA", "Dropout")]
par(mfrow=c(3,3))
for(i in 1:(ncol(data_numeric)-1)){
  plot(data_numeric[,i], col=data$Dropout, ylab=colnames(data_numeric)[i],
       main="")
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
# (potrebbe essere che queste variabili non erano obbligatorie nella compilazione
# del questionario)


#library("UpSetR")
#upset(as_shadow_upset(data))

library(mice)
md.pattern(data)



table(data$Dropout)  
# Dai dati originali, vedo che molti studenti completano i loro studi, mentre chi abbandona è
# in numero minore. Siamo quindi in presenza di dati sbilanciati. 
# Prima dell'imputazione quindi, al fine di evitare distorsioni
# nelle nostre analisi, abbiamo pensato di dividere i dati originali in train e test set, e 
# provare ad eseguire oversampling, undersampling ed eseguire le analisi sui dati originali.
# Per ogni scenario abbiamo imputato i dati in diverse modalità (pmm - cart - norm - mean)
# e valutato il risultato migliore.


# provo con i dati cosi, under e oversampling -----------------------------

# Accuracy alta nei dati originali, ma F1 basso --> unbalanced data
# tra under e over sampling l'over sampling funziona molto meglio

# come imputazioni sono all'incirca simili, il mean semvra essere il migliore


set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,18] <- 0 

library(mice)
my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)

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
  final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
  confusion_matrix <- table(test$Dropout, final_pred_class)
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  f1[i] <- f1_score(confusion_matrix)
}

# Risultati
accuracies
# 0.8227583 0.8227583 0.8232826 0.8232826
f1
# 0.5279330 0.5292479 0.5299861 0.5299861


# undersampling -----------------------------------------------------------

lab <- sample(1:7646, 2354)
data0 <- data[data$Dropout==0,]
data <- rbind(data0[lab,], data[data$Dropout==1,])

table(data$Dropout)

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,18] <- 0 

library(mice)
my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)


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
  final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
  confusion_matrix <- table(test$Dropout, final_pred_class)
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  f1[i] <- f1_score(confusion_matrix)
}

# Risultati
accuracies
# [1] 0.7505643 0.7505643 0.7494357 0.7494357
# accuracies minori
f1
# 0.7547170 0.7547170 0.7538803 0.7538803

# sofia:
# 0.7319005 0.7341629 0.7330317 0.7330317
# 0.7432286 0.7453954 0.7440347 0.7440347




# oversampling -----------------------------------------------------------

lab <- sample(1:2354, 7646, replace=T)
data1 <- data[data$Dropout==1,]
data <- rbind(data1[lab,], data[data$Dropout==0,])

table(data$Dropout)

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,18] <- 0 

library(mice)
my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234, printFlag = FALSE)


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
  final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
  confusion_matrix <- table(test$Dropout, final_pred_class)
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  f1[i] <- f1_score(confusion_matrix)
}

# Risultati
accuracies
# 0.8278215 0.8293963 0.8278215 0.8283465
f1
# 0.8934373 0.8943776 0.8934373 0.8937277


# - -----------------------------------------------------------------------

# L'oversampling è la tecnica che dà risultati migliori, considerando sia l'accuracy che  F1_score,
# metrica più adatta a valutare la precisione delle stime in presenza di dati sbilanciati.
# In particolare, Il metodo mean risulta il migliore in termini di imputazione.
# Abbiamo quindi eseguito un oversampling sulla totalità dei dati originali e imputato
# i valori mancanti tramite mean.

# - -----------------------------------------------------------------------



# --> migliore è mean

# imputazione di tutti i valori con mean e oversampling -------------------

rm(list=ls())
student_dropout_dataset_v3 <- read.csv("~/progetto-data-mining/student_dropout_dataset_v3.csv")
data <- student_dropout_dataset_v3[,-1] # rimuovo nome
data$Gender <- as.factor(data$Gender)
data$Internet_Access <- as.factor(data$Internet_Access)
data$Semester <- as.factor(data$Semester)
data$Department <- as.factor(data$Department)
data$Parental_Education <- as.factor(data$Parental_Education)
data$Part_Time_Job <- as.factor(data$Part_Time_Job)
data$Scholarship <- as.factor(data$Scholarship)
data$Dropout <- as.factor(data$Dropout )
data$Assignment_Delay_Days <- as.factor(data$Assignment_Delay_Days)


lab <- sample(1:2354, 7646, replace=T)
data1 <- data[data$Dropout==1,]
data <- rbind(data1[lab,], data[data$Dropout==0,])

table(data$Dropout)

p <- dim(data)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,18] <- 0 

library(mice)
data_imp <- mice(data, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234,  printFlag = FALSE)


densityplot(data_imp)

# pooling
#modelFit <- with(data_imp,lm(Dropout~ .))
#summary(pool(modelFit))




# selezione variabili se necessaria ---------------------------------------
dati <- complete(data_imp, 1)
dati_numeric <- dati[, c("Age","Family_Income"  , "Study_Hours_per_Day" ,"Attendance_Rate" ,     
                          "Travel_Time_Minutes" ,"Stress_Index" , "GPA"                  
                         ,"Semester_GPA" ,"CGPA", "Dropout")]

cor(dati_numeric[,-10])
library(ggcorrplot)
ggcorrplot(dati_numeric[,-10])
# NON SO CHE PROBLEMI ABBIA

# Le variabili Semester GPA, CGPA e GPA sono collineari
# infatti sono tutti indicatori della media dei voti. 
# applico una selezione della variabili



# raga HELP --------------------------------------------------------------------

# QUA IL MDELLO LE TIWNE TUTTE, FACCIAMO NOI UNA SELEZIONE?
# PRENDIAMO MEDIA CGPA CHE PENSO SIA TITPO TOTALE?

mnull <- glm(dati$Dropout ~ 1, data=dati, family="binomial")
mfull <- glm(dati$Dropout ~ ., data=dati, family="binomial")
step(mfull, mnull, direction="both")


# Tengo solo un valore per la media, al fine di evitare dati ridondandi
dati <- dati[, !(names(dati) %in% c("Semester_GPA", "GPA"))]


# Classificazione ---------------------------------------------------------

# Voglio prevedere l'abbandono o meno di uno studente. Per fare ciò, eseguo una classificazione
# dei dati in base alla variabile Dropout. Se ottengo 0 significa che lo studente proseguirà i suoi
# studi, 1 invece indica l'abbandono.
# Decido di selezionare il primo set di dati ottenuto dalla prima imputazione (?).
# Divido i dati nuovamente in test e train, e valuto il miglior modello di classificazione.


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

lda_trn_pred = predict(mod_lda, dati_trn)$class 
lda_tst_pred = predict(mod_lda, dati_tst)$class

calc_class_err = function(actual, predicted) { 
  mean(actual != predicted)
}


calc_class_err(predicted = lda_tst_pred, actual = y_test)
table(predicted = lda_tst_pred, actual = y_test)


# QDA ---------------------------------------------------------------------

mod_qda = qda(Dropout ~ ., data = dati_trn) 

qda_trn_pred = predict(mod_qda, dati_trn)$class 
qda_tst_pred = predict(mod_qda, dati_tst)$class

calc_class_err(predicted = qda_tst_pred, actual = y_test) 


table(predicted = qda_tst_pred, actual = y_test)


# LOGISTICA ---------------------------------------------------------------

glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
glm.probs[1:10]

glm_pred <- ifelse(glm.probs>0.5,1,0)
table(glm_pred, y_test)
calc_class_err(predicted = glm_pred, actual = y_test)


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
table(tree.pred , y_test)
calc_class_err(predicted = tree.pred, actual = y_test)

# confronti ----------------------------------------------------------------

cbind(calc_class_err(predicted = lda_tst_pred, actual = y_test),calc_class_err(predicted = qda_tst_pred, actual = y_test)
      , calc_class_err(predicted = glm_pred, actual = y_test), calc_class_err(predicted = tree.pred, actual = y_test)

      )
# logistica migliore ma per poco