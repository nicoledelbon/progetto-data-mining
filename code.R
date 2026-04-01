rm(list=ls())
library(readr)
student <- read_csv("~/progetto-data-mining/student_performance_updated_1000.csv")
student$Gender = as.factor(student$Gender)
student$ParentalSupport = as.factor(student$ParentalSupport)
student$`Online Classes Taken` = as.factor(student$`Online Classes Taken`)
colSums(is.na(student))
colnames(is.na(student))

# tolgo l'id e il nome perchè non servono 
student=student[,-c(1,2)]

# tengo solo le osservazioni che hanno un valore per final grade
# altrimenti non ho nessuna variabile su cui basarmi per imputare le altre
student=subset(student, !is.na(FinalGrade))

student$Ammission = ifelse(student$FinalGrade > median(student$FinalGrade), 0, 1)
table(student$Ammission)

student=student[,-7]

library(mice)
md.pattern(student)

library(VIM)
aggr_plot <- aggr(student, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(student), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# train-test split --------------------------------------------------------
set.seed(1)
labels = sample(1:nrow(student), 0.8*nrow(student))
train = student[labels,]
test = student[-labels,]

# missing imputation ------------------------------------------------------

library(mice)
p = dim(train)[2]
predmat = 1 - diag(nrow=p,ncol=p)
predmat[,10]=0

my_train1 = mice(train, method = "pmm",
                    predictorMatrix = predmat, seed=1234, printFlag = FALSE)
my_train2 = mice(train, method = "cart",
                    predictorMatrix = predmat, seed=1234, printFlag = FALSE)

fit1=glm.mids(Ammission~., family = "binomial", data=my_train1)
fit2=glm.mids(Ammission~., family = "binomial", data=my_train2)

pred1 = lapply(getfit(fit1), predict, se.fit=T, newdata=test, type="response")
pred2 = lapply(getfit(fit2), predict, se.fit=T, newdata=test, type="response")

single_pred1=sapply(pred1, `[[`, "fit")
single_pred2=sapply(pred2, `[[`, "fit")

final_pred1=apply(single_pred1, 1, mean)
final_pred2=apply(single_pred2, 1, mean)

final_pred_class1=ifelse(final_pred1 > 0.5, 1, 0)
final_pred_class2=ifelse(final_pred2 > 0.5, 1, 0)

cm1=table(test$Ammission, final_pred_class1)
cm2=table(test$Ammission, final_pred_class2)

acc1=sum(diag(cm1)) / sum(cm1)
acc2=sum(diag(cm2)) / sum(cm2)

cbind(acc1,acc2)

imp = mice(student, method = "pmm",
                   predictorMatrix = predmat, seed=1234, printFlag = FALSE)
student_imp = complete(imp)
table(student_imp$Ammission)





# --  ---------------------------------------------------------------------


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,10] <- 0 

my_training1 <- mice(train, method = "pmm", predictorMatrix = my_predictorMatrix, seed = 1234,  m = 10, maxit = 10, printFlag = FALSE)
my_training2 <- mice(train, method = "mean", predictorMatrix = my_predictorMatrix, seed = 1234, m = 10, maxit = 10, printFlag = FALSE)
my_training3 <- mice(train, method = "norm", predictorMatrix = my_predictorMatrix, seed = 1234, m = 10, maxit = 10, printFlag = FALSE)
my_training4 <- mice(train, method = "cart", predictorMatrix = my_predictorMatrix, seed = 1234, m = 10, maxit = 10, printFlag = FALSE)
my_training5 <- mice(train, method = "rf", predictorMatrix = my_predictorMatrix, seed = 1234, m = 10, maxit = 10,printFlag = FALSE)

# QUESTO SOPRA è IL MIGLIORE DALLE ACCURACY, HO PROVATO A FARE SELZIONE VARIABILI

train_complete <- complete(my_training5, 1)
m_null <- glm(Ammission ~ 1, data = train_complete, family = "binomial")
m_full <- glm(Ammission ~ ., data = train_complete, family = "binomial")
step_model <- step(m_null, scope = list(lower = m_null, upper = m_full), direction = "forward")

# ANCHE LA CORREKAZIONE NON è COSI GRAVE QUINDI NON SONO MULTICOLLINEARI
cor(train_complete[, c(2,3,4,5,7,8)])

model1 <- glm.mids(Ammission ~ ., family="binomial", data = my_training1)
model2 <- glm.mids(Ammission ~ ., family="binomial", data = my_training2)
model3 <- glm.mids(Ammission ~ ., family="binomial", data = my_training3)
model4 <- glm.mids(Ammission ~ ., family="binomial", data = my_training4)
model5 <- glm.mids(Ammission ~ ., family="binomial", data = my_training5)

# Lista dei modelli
models <- list(model1, model2, model3, model4, model5)

# Lista per salvare le accuracy
accuracies <- numeric(length(models))

for (i in seq_along(models)) {
  
  prediction <- lapply(getfit(models[[i]]), predict, 
                       se.fit = TRUE, newdata = test, type = "response")
  
  single_prediction <- sapply(prediction, `[[`, "fit")
  
  final_pred <- apply(single_prediction, 1, mean)
  
  final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
  
  confusion_matrix <- table(test$Ammission, final_pred_class)
  
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Risultati
accuracies


