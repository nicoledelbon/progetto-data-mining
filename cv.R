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
    
    set.seed(1)
    id = sample(1:nrow(train_fold), 0.2*nrow(train_fold))
    validation = train_fold[id,]
    train_fold = train_fold[-id,] 
    
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
    
    # OTTIMIZZAZIONE SOGLIA-------
    soglia <- seq(0.1, 0.9, by = 0.01)
    glm.probs_val <- predict(glm_fit, newdata=validation, type="response")
    y_vals <- validation[, 9]
    f1_scores <- numeric(length(soglia))
    for (i in seq_along(soglia)) {
      pred_val <- ifelse(glm.probs_val > soglia[i], 1, 0)
      cm_val <- make_cm(y_vals, pred_val)
      f1_scores[i] <- metrics(cm_val)$F1_score
    }
    soglia_opt <- soglia[which.max(f1_scores)]
    glm_pred_opt <- ifelse(glm.probs > soglia_opt, 1, 0)
    res$GLM_OPT[f, ] <- unlist(metrics(make_cm(y_val, glm_pred_opt)))
    
    # ALBERO ------------------------------------------------------------------
    tree_dati <- tree(Dropout ~ . , data=dati_trn)
    tree.pred <- predict(tree_dati , val_fold , type = "class")
    res$TREE[f, ] <- unlist(metrics(make_cm(y_val, tree.pred)))
    
    # RANDOM FOREST -----------------------------------------------------------
    rf_model <- randomForest(Dropout ~ ., data=dati_trn, ntree=300)
    rf_pred <- predict(rf_model, val_fold)
    res$RF[f, ] <- unlist(metrics(make_cm(y_val, rf_pred)))
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
