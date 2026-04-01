student_dropout_dataset_v3 <- read.csv("~/progetto-data-mining/student_dropout_dataset_v3.csv")
data <- student_dropout_dataset_v3[,-1]
data$Gender <- as.factor(data$Gender)
data$Internet_Access <- as.factor(data$Internet_Access)
data$Semester <- as.factor(data$Semester)
data$Department <- as.factor(data$Department)
data$Parental_Education <- as.factor(data$Parental_Education)
table(data$Dropout)                                     

set.seed(1)
labels = sample(1:nrow(data), 0.8*nrow(data))
train = data[labels,]
test = data[-labels,]


p <- dim(train)[2]

my_predictorMatrix <- 1 - diag(nrow = p, ncol = p)
my_predictorMatrix[ ,18] <- 0 

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

for (i in seq_along(models)) {
  
  prediction <- lapply(getfit(models[[i]]), predict, 
                       se.fit = TRUE, newdata = test, type = "response")
  
  single_prediction <- sapply(prediction, `[[`, "fit")
  
  final_pred <- apply(single_prediction, 1, mean)
  
  final_pred_class <- ifelse(final_pred > 0.5, 1, 0)
  
  confusion_matrix <- table(test$Dropout, final_pred_class)
  
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Risultati
accuracies


