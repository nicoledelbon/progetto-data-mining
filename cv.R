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

library(caret)
cv = function(train_rid, m, k){
  folds <- createFolds(train_rid$Dropout, k = k, list = FALSE)
  res <- list(
    LDA     = matrix(NA, nrow=k, ncol=5),
    QDA     = matrix(NA, nrow=k, ncol=5),
    GLM     = matrix(NA, nrow=k, ncol=5),
    GLM_OPT = matrix(NA, nrow=k, ncol=5),
    TREE    = matrix(NA, nrow=k, ncol=5),
    RF      = matrix(NA, nrow=k, ncol=5))
  
  colnames(res$LDA) <- colnames(res$QDA) <- colnames(res$GLM) <-
    colnames(res$GLM_OPT) <- colnames(res$TREE) <- colnames(res$RF) <-
    c("Precision","Recall","Specificity","Accuracy","F1")
  
  make_cm <- function(true, pred){
    table(factor(true, levels=c(0,1)),
          factor(pred, levels=c(0,1)))
  }
  
  for (f in 1:k) {
    train_fold <- train_rid[folds != f, ]
    val_fold   <- train_rid[folds == f, ]
    y_val <- val_fold$Dropout
    
    data0 <- train_fold[train_fold$Dropout == 0, ]
    data1 <- train_fold[train_fold$Dropout == 1, ]
    
    if(m=="dati normali"){
      dati_trn <- train_fold
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
    
    dati_tst <- val_fold
    y_test <- val_fold$Dropout
    
    
    
    mod_lda = lda(Dropout ~ ., data = dati_trn) 
    lda_tst_pred = predict(mod_lda, dati_tst)$class
    res$LDA[f, ] <- unlist(metrics(make_cm(y_test, lda_tst_pred)))
    
    # QDA ---------------------------------------------------------------------
    mod_qda = qda(Dropout ~ ., data = dati_trn) 
    qda_tst_pred = predict(mod_qda, dati_tst)$class
    res$QDA[f, ] <- unlist(metrics(make_cm(y_test, qda_tst_pred)))
    
    # LOGISTICA ---------------------------------------------------------------
    glm_fit <- glm(Dropout ~ . , data=dati_trn, family="binomial")
    glm.probs <- predict(glm_fit, newdata= dati_tst,type = "response")
    glm_pred <- ifelse(glm.probs>0.5,1,0)
    res$GLM[f, ] <- unlist(metrics(make_cm(y_test, glm_pred)))
    
    # OTTIMIZZAZIONE SOGLIA-------
    soglia <- seq(0.1, 0.9, by = 0.01)
    f1_scores <- numeric(length(soglia))
    for (i in seq_along(soglia)) {
      pred_val <- ifelse(glm.probs > soglia[i], 1, 0)
      cm_val <- make_cm(y_test, pred_val)
      f1_scores[i] <- metrics(cm_val)$F1_score
    }
    soglia_opt <- soglia[which.max(f1_scores)]
    glm_pred_opt <- ifelse(glm.probs > soglia_opt, 1, 0)
    res$GLM_OPT[f, ] <- unlist(metrics(make_cm(y_test, glm_pred_opt)))
    
    # ALBERO ------------------------------------------------------------------
    tree_dati <- tree(Dropout ~ . , data=dati_trn)
    tree.pred <- predict(tree_dati , dati_tst , type = "class")
    res$TREE[f, ] <- unlist(metrics(make_cm(y_test, tree.pred)))
    
    # RANDOM FOREST -----------------------------------------------------------
    rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=300)
    rf_pred <- predict(rf_model, dati_tst)
    res$RF[f, ] <- unlist(metrics(make_cm(y_test, rf_pred)))
  }
  
  final <- matrix(0, nrow=6, ncol=5)
  rownames(final) <- names(res)
  colnames(final) <- c("Precision","Recall","Specificity","Accuracy","F1")
  
  for(i in 1:6){
    final[i, ] <- colMeans(res[[i]], na.rm=TRUE)
  }
  
  print(round(final, 4))
}

cv(train_rid, "dati normali", 5)
cv(train_rid, "undersampling", 5)
cv(train_rid, "oversampling", 5)
