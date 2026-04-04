rm(list=ls())
library(readr)
student <- read_csv("student_performance_updated_1000.csv")
student$Gender = as.factor(student$Gender)
student$ParentalSupport = as.factor(student$ParentalSupport)
student$`Online Classes Taken` = as.factor(student$`Online Classes Taken`)
colSums(is.na(student))
colnames(is.na(student))


# tolgo l'id e il nome perchè non sono utili per le analisi 
student=student[,-c(1,2)]

# tengo solo le osservazioni che hanno un valore per final grade
# altrimenti non ho nessuna variabile su cui basarmi per imputare le altre
student=subset(student, !is.na(FinalGrade))

student$Ammission = ifelse(student$FinalGrade > median(student$FinalGrade), 0, 1)
table(student$Ammission)
# le classi sono abbastanza bilanciate

student=student[,-7] # elimino Final Grafe

library(visdat)
library(naniar)
vis_dat(student) # visualizzazione nei dati
vis_miss(student) # il 3.2% dei dati è mancante

library(UpSetR)
upset(as_shadow_upset(student)) # pochi sono i casi in cui ci sono collegamento tra i valori mancanti; non sono fortemente clusterizzati; qquindi posso gestire i NA variabile per variabile

library(mice)
md.pattern(student)

library(VIM)
aggr_plot <- aggr(student, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(student), cex.axis=.7, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))

# train-test split --------------------------------------------------------

set.seed(1)

set.seed(1234)

labels = sample(1:nrow(student), 0.8*nrow(student))
train = student[labels,]
test = student[-labels,]

miss_var_table(train) # numero di valori mancanti per ciascuno dei casi

# missing imputation ------------------------------------------------------

library(mice)
p = dim(train)[2]
predmat = 1 - diag(nrow=p,ncol=p)
# predmat[,10]=0

table(train$Gender)
sum(is.na(train$Gender))
my_train1 = mice(train, method = "pmm",
                    predictorMatrix = predmat, seed=1234, printFlag = FALSE)
my_train2 = mice(train, method = "cart",
                    predictorMatrix = predmat, seed=1234, printFlag = FALSE)

# modelli logistici su dati imputati
fit1 = with(my_train1, glm(Ammission ~ ., family = "binomial"))
fit2=with(my_train2, glm(Admision ~., family = "binomial"))

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


### altra versione
predmat = make.predictorMatrix(train)
predmat[, "Ammission"]=0
# metodi corretti, per imputare correttamente le variabili factor
meth = make.method(train)
meth["Gender"] = "logreg"
meth["ParentalSupport"] = "polyreg"
meth["Online Classes Taken"] = "logreg"

# imputazione train
imp_train = mice(train,
                 method = meth,
                 predictorMatrix = predmat,
                 seed = 1234,
                 printFlag = FALSE)

train_list = complete(imp_train, "all")
# modelli logistici
models = lapply(train_list, function(d) {
  lm(Ammission ~ ., data = d, family = "binomial")
})
# Imputazione TEST (FONDAMENTALE)
str(train)
str(test)
imp_test = mice.mids(imp_train, newdata = test)
test_list = complete(imp_test, "all")
# Predizioni corrette
pred = lapply(1:length(test_list), function(i) {
  predict(models[[i]], newdata = test_list[[i]], type = "response")
})

pred_matrix = do.call(cbind, pred)
final_pred = rowMeans(pred_matrix)
# Classificazione
final_pred_class = ifelse(final_pred > 0.5, 1, 0)
# Confusion matrix + accuracy
cm = table(test$Ammission, final_pred_class)
acc = sum(diag(cm)) / sum(cm)

cm
acc
# il modello è bilanciato verso la classe 1 e sbaglia tanti 0

## probabilmente la logististica è troppo semplice

