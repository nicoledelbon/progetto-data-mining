data <- data[, (names(data) %in% c("Family_Income", "Internet_Access",
                                   "Attendance_Rate","Assignment_Delay_Days",
                                   "Travel_Time_Minutes","Part_Time_Job",
                                   "Stress_Index","GPA","Dropout"))]
my_predictorMatrix[ ,9] <- 0 
lab <- sample(1:nrow(data1), nrow(data0), replace=T)
train <- rbind(data1[lab,], data0)
table(train$Dropout)
dati_trn_no_dropout <- dati[-lab1,-9]
dati_tst_no_dropout <- dati[lab1,-9]
x_train = dati_trn[,-9]
x_test = dati_tst[,-9]
y_train <- dati_trn[,9]
y_test <- dati_tst[, 9]
