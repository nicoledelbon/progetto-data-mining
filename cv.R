set.seed(123)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_rid)))

metodi <- c("dati normali", "undersampling", "oversampling")

metriche_cv <- list()

# =========================================================
# CROSS VALIDATION
# =========================================================

for (m in metodi) {
  
  # struttura folds: modello x metrica x fold
  temp <- list(
    LDA = list(),
    QDA = list(),
    GLM = list(),
    GLM_OPT = list(),
    TREE = list(),
    RF = list()
  )
  
  for (model in names(temp)) {
    temp[[model]] <- list()
  }
  
  for (f in 1:k) {
    
    train_fold <- train_rid[folds != f, ]
    val_fold   <- train_rid[folds == f, ]
    y_val <- val_fold$Dropout
    
    # -------------------------
    # RESAMPLING
    # -------------------------
    
    if (m == "dati normali") {
      dati_trn <- train_fold
    }
    
    if (m == "undersampling") {
      data0 <- train_fold[train_fold$Dropout == 0, ]
      data1 <- train_fold[train_fold$Dropout == 1, ]
      set.seed(1)
      lab <- sample(1:nrow(data0), nrow(data1))
      dati_trn <- rbind(data0[lab, ], data1)
    }
    
    if (m == "oversampling") {
      data0 <- train_fold[train_fold$Dropout == 0, ]
      data1 <- train_fold[train_fold$Dropout == 1, ]
      set.seed(1)
      lab <- sample(1:nrow(data1), nrow(data0), replace = TRUE)
      dati_trn <- rbind(data1[lab, ], data0)
    }
    
    make_cm <- function(a, p) {
      table(factor(a, levels = c(0,1)),
            factor(p, levels = c(0,1)))
    }
    
    # =====================================================
    # MODELLI
    # =====================================================
    
    # LDA
    mod <- lda(Dropout ~ ., data = dati_trn)
    pred <- predict(mod, val_fold)$class
    temp$LDA[[f]] <- metrics(make_cm(y_val, pred))
    
    # QDA
    mod <- qda(Dropout ~ ., data = dati_trn)
    pred <- predict(mod, val_fold)$class
    temp$QDA[[f]] <- metrics(make_cm(y_val, pred))
    
    # GLM
    mod <- glm(Dropout ~ ., data = dati_trn, family = "binomial")
    prob <- predict(mod, val_fold, type = "response")
    pred <- ifelse(prob > 0.5, 1, 0)
    temp$GLM[[f]] <- metrics(make_cm(y_val, pred))
    
    # GLM OPT
    soglia <- seq(0.1, 0.9, 0.01)
    f1 <- sapply(soglia, function(s) {
      p <- ifelse(prob > s, 1, 0)
      metrics(make_cm(y_val, p))$F1_score
    })
    
    best <- soglia[which.max(f1)]
    pred <- ifelse(prob > best, 1, 0)
    temp$GLM_OPT[[f]] <- metrics(make_cm(y_val, pred))
    
    # TREE
    dati_trn$Dropout <- as.factor(dati_trn$Dropout)
    mod <- tree(Dropout ~ ., data = dati_trn)
    pred <- predict(mod, val_fold, type = "class")
    temp$TREE[[f]] <- metrics(make_cm(y_val, pred))
    
    # RF
    mod <- randomForest(Dropout ~ ., data = dati_trn, ntree = 300)
    pred <- predict(mod, val_fold)
    temp$RF[[f]] <- metrics(make_cm(y_val, pred))
  }
  
  # =========================================================
  # MATRICE FINALE (modelli x metriche)
  # =========================================================
  
  models <- names(temp)
  metrics_names <- c("Precision","Recall","Specificity","Accuracy","F1")
  
  final_matrix <- matrix(0,
                         nrow = length(models),
                         ncol = length(metrics_names))
  
  rownames(final_matrix) <- models
  colnames(final_matrix) <- metrics_names
  
  for (model in models) {
    
    fold_vals <- do.call(rbind, lapply(temp[[model]], function(x) {
      unlist(x)
    }))
    
    final_matrix[model, ] <- colMeans(fold_vals)
  }
  
  metriche_cv[[m]] <- final_matrix
}

metriche_cv

