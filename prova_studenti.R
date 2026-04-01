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
data <- student_dropout_dataset_v3[,-1]
data$Gender <- as.factor(data$Gender)
data$Internet_Access <- as.factor(data$Internet_Access)
data$Semester <- as.factor(data$Semester)
data$Department <- as.factor(data$Department)
data$Parental_Education <- as.factor(data$Parental_Education)
data$Part_Time_Job <- as.factor(data$Part_Time_Job)
data$Scholarship <- as.factor(data$Scholarship)

table(data$Dropout)  
# molte più persone continuano


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
# [1] 0.8232826 0.8227583 0.8232826 0.8232826
f1
#  0.5299861 0.5292479 0.5299861 0.5299861


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
# [1] 0.7505643 0.7505643 0.7494357 0.7494357
# accuracies minori
f1
# 0.7601749 0.7612643 0.7599193 0.7595960



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

dati <- complete(data_imp, 1)

lab1 <- sample(1:nrow(dati), 0.2*nrow(dati))
dati_trn <- dati[-lab1,]
dati_tst <- dati[lab1,]
dati_trn_no_dropout <- dati[-lab1,-18]
dati_tst_no_dropout <- dati[lab1,-18]
y_train <- dati_trn[,18]
y_test <- dati_tst[, 18]


## LDA
######################################################################################

library(MASS)
mod_lda = lda(Dropout ~ ., data = dati) 

plot(mod_lda)

lda_trn_pred = predict(mod_lda, dati_trn)$class 
lda_tst_pred = predict(mod_lda, dati_tst)$class

calc_class_err = function(actual, predicted) { 
  mean(actual != predicted)
}


calc_class_err(predicted = lda_tst_pred, actual = y_test)
table(predicted = lda_tst_pred, actual = y_test)


## QDA
######################################################################################

mod_qda = qda(Dropout ~ ., data = dati) 

qda_trn_pred = predict(mod_qda, dati_trn)$class 
qda_tst_pred = predict(mod_qda, dati_tst)$class

calc_class_err(predicted = qda_trn_pred, actual = y_train)
calc_class_err(predicted = qda_tst_pred, actual = y_test)

table(predicted = qda_tst_pred, actual = y_test)


## Logistica
######################################################################################
glm_fit <- glm(Dropout ~ . , data=dati, family="binomial")
glm.probs <- predict(glm_fit, type = "response")
glm.probs[1:10]

glm_pred <- ifelse(glm.probs>0.5,1,0)
table(glm_pred, dati$Dropout)
