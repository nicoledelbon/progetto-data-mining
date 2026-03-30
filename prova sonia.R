# POROVA

rm(list=ls())
data <- read.csv("IMDbMovies-Clean.csv", header = TRUE)
data[data==""] <- NA
sum(is.na(data))

sum(is.na(data$Main.Genres)) # 7 missing nei generi --> sapremo trovare il genere?
genres <- sort(unique(data$Main.Genres))
genres_filtrati <- genres[nchar(genres) <= 11]
# filtro i generi con meno di 11 caratteri (che corrispondono ad adventure che è il genere con più caratteri)

# tutti i generi sarebbero
# genre = c("Action","Adventure","Animation","Biography","Comedy","Crime",
#           "Documentary","Drama","Fantasy","Film-Noir","History","Horror","Mystery",
#           "Music","Musical","Romance","Sci-Fi","Sport","Thriller","War","Western")

# cosi seleziono solo i generi singoli che appaiono realmente
unique_genre = c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama",
                 "Horror","Music","Musical","Romance","Sci-Fi","Sport","Thriller","War","Western")
imdb = subset(data, data$Main.Genres %in% unique_genre)
sum(is.na(imdb))
table(imdb$Main.Genres)


# romance cine western pochi anche dppi, provo a toglierli

# music, musical, sport, adventure, animation, war
# hanno meno di 3 film appartenenti a quel genere 
# quindi non sarebbero sufficienti per allenare il mio modello



# unisco alcuni generi ----------------------------------------------------


data[which(data$Main.Genres=="Adventure" | data$Main.Genres=="Action,Adventure" | 
             data$Main.Genres=="Action,Adventure,Biography" | 
             data$Main.Genres== "Action,Adventure,Fantasy"), "Main.Genres"] <- "Action"
# al posto di solo action che ha poche oss farei action - adventure - fantasy

data[which(data$Main.Genres %in% c("Documentary,Biography", "Documentary,History", "Documentary,History,War", "Documentary,War",
                                   "Documentary,Biography,Crime", "Documentary,Biography,Drama", "Documentary,Biography,History",
                                   "Documentary,Biography,Music", "Documentary,Biography,Sport", "Documentary,Animation,Biography",
                                   "Documentary,Sport")), "Main.Genres"] <- "Documentary"
# al posto di documentari
imdb = subset(imdb, !(imdb$Main.Genres %in% c("Music", "Musical", "Sport", 
                                              "War", "Adventure", "Animation", "Sci-Fi", "Crime", "Biography")))

table(imdb$Main.Genres)

imdb = subset(data, data$Main.Genres %in% unique_genre)
table(imdb$Main.Genres)
imdb_bal = subset(imdb, !(imdb$Main.Genres %in% c("Music", "Musical", "Sport", "Romance", "Western", 
                                              "War", "Adventure", "Animation", "Sci-Fi", "Crime", "Biography")))
table(imdb_bal$Main.Genres)

#  stop -------------------------------------------------------------------


# controllo finale
table(imdb_bal$Main.Genres)


library(tm)
desc <- imdb_bal$Summary
sum(is.na(desc))
docs <- VCorpus(VectorSource(desc)) # così non da errore
summary(docs)

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

writeLines(as.character(docs[[4]]))

library(SnowballC)
docs <- tm_map(docs,stemDocument)

dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

freq[head(ord, 40)]
# vedo che tra le parole più frequenti ci sono find, one, two, take
# old, new, goe, must, turn, tri, fall

my_step <- c("find", "one", "two", "take", "old", "new", 
             "goe", "must", "turn", "tri", "fall",
             "man", "woman", "young", "group")
docs <- tm_map(docs, removeWords, my_step)

dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

freq[tail(ord)]

freq[head(ord, 300)]

dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
        bounds = list(global = c(20,750))))

freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr,decreasing=TRUE)
findFreqTerms(dtmr,lowfreq=50)


# class -------------------------------------------------------------------

# =========================
# PREPARAZIONE DATI
# =========================

# target
X <- as.matrix(dtmr)
y <- as.factor(imdb_bal$Main.Genres)

# train-test split
set.seed(123)
train_idx <- sample(1:nrow(X), 0.8 * nrow(X))

X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]

y_train <- y[train_idx]
y_test  <- y[-train_idx]


# =========================
# NAIVE BAYES
# =========================
library(e1071)
model_nb <- naiveBayes(X_train, y_train)
pred_nb <- predict(model_nb, X_test)

# =========================
# LOGISTIC REGRESSION (multinomiale)
# =========================
library(nnet)
model_lr <- multinom(y_train ~ ., data = as.data.frame(X_train))
pred_lr <- predict(model_lr, newdata = as.data.frame(X_test))

# =========================
# RANDOM FOREST
# =========================
library(randomForest)
model_rf <- randomForest(x = X_train, y = y_train)
pred_rf <- predict(model_rf, X_test)

# =========================
# SVM
# =========================
model_svm <- svm(X_train, y_train)
pred_svm <- predict(model_svm, X_test)

# =========================
# VALUTAZIONE
# =========================
library(caret)

cm_nb  <- confusionMatrix(pred_nb, y_test)
cm_lr  <- confusionMatrix(pred_lr, y_test)
cm_rf  <- confusionMatrix(pred_rf, y_test)
cm_svm <- confusionMatrix(pred_svm, y_test)
# fatica a riconoscere horror e thriller maggiormente

# accuracy
acc_nb  <- cm_nb$overall["Accuracy"]
acc_lr  <- cm_lr$overall["Accuracy"]
acc_rf  <- cm_rf$overall["Accuracy"]
acc_svm <- cm_svm$overall["Accuracy"]

# confronto finale
results <- data.frame(
  Modello = c("Naive Bayes", "Logistic Regression", "Random Forest", "SVM"),
  Accuracy = c(acc_nb, acc_lr, acc_rf, acc_svm)
)

print(results)


# class
prev <- data[is.na(data$Main.Genres),2]
# vedo che 4 film non hanno descrizione, non posso prevedere
prev <- prev[c(1,3,4)]

prev <- VCorpus(VectorSource(prev)) # così non da errore

prev <- tm_map(prev, toSpace, "-")
prev <- tm_map(prev, toSpace, ":")
prev <- tm_map(prev, toSpace, "'")
prev <- tm_map(prev, toSpace, "'")
prev <- tm_map(prev, toSpace, " -")
prev <- tm_map(prev, removePunctuation)
prev <- tm_map(prev,content_transformer(tolower))
prev <- tm_map(prev, removeNumbers)
prev <- tm_map(prev, removeWords, stopwords("english"))
prev <- tm_map(prev, stripWhitespace)
prev <- tm_map(prev, removeWords, my_step)

writeLines(as.character(prev[[3]]))

library(SnowballC)
prev <- tm_map(prev,stemDocument)

dtm_prev <- DocumentTermMatrix(prev, control = list(dictionary = Terms(dtmr)))

mew <- as.matrix(dtm_prev)

predict(model_lr, as.data.frame(mew))



# guardo comedy che è sempre previsto -------------------------------------

library(nnet)
coefs <- coef(model_lr)  # matrice: righe = classi, colonne = termini

# Coefficienti per Comedy
coefs_comedy <- coefs["Comedy",]

# Ordina i termini che spingono di più verso Comedy
top_comedy <- sort(coefs_comedy, decreasing = TRUE)
head(top_comedy, 20)  # prime 20 parole più influenti




























# over under ----------------------- MIGLIORE PER ORA---------------------------------------

set.seed(2)
target_n <- 100

imdb_bal <- do.call(rbind,
                    lapply(split(imdb, imdb$Main.Genres), function(df) {
                      
                      n <- nrow(df)
                      
                      if (n > target_n) {
                        # undersampling
                        df[sample(1:n, target_n), ]
                        
                      } else if (n < target_n) {
                        # oversampling
                        df[sample(1:n, target_n, replace = TRUE), ]
                        
                      } else {
                        df
                      }
                      
                    })
)

# stop --------------------------------------------------------------------



#  solo undersampling di drama --------------------------------------------


set.seed(42)  # per riproducibilità
target_n <- 120  # numero massimo di esempi per classe

# undersampling di Drama
imdb_bal <- do.call(rbind,
                    lapply(split(imdb, imdb$Main.Genres), function(df) {
                      
                      n <- nrow(df)
                      
                      if (df$Main.Genres[1] == "Drama" & n > target_n) {
                        # undersampling di Drama
                        df[sample(1:n, target_n), ]
                      } else {
                        df
                      }
                      
                    })
)

# stop --------------------------------------------------------------------


# under di comedy e drama -------------------------------------------------
set.seed(42)  # per riproducibilità
target_n <- 150  # numero massimo di esempi per classe

# undersampling di Drama
imdb_bal <- do.call(rbind,
                    lapply(split(imdb_bal, imdb_bal$Main.Genres), function(df) {
                      
                      n <- nrow(df)
                      
                      if (df$Main.Genres[1] %in% c("Comedy", "Drama")  & n > target_n) {
                        # undersampling di Drama e Comedy
                        df[sample(1:n, target_n), ]
                      } else {
                        df
                      }
                      
                    })
)

# stop --------------------------------------------------------------------


X <- as.matrix(dtmr)
horror_words <- c(
  "ghost", "kill", "blood", "dead", "death", "evil",
  "murder", "dark", "fear", "haunt", "zombi",
  "monster", "horror", "night", "scream",
  "terror", "supernatur", "demon", "curse", "shadow"
)
thriller_words <- c(
  "polic", "crime", "investig", "detect", "murder",
  "case", "suspect", "agent", "secret", "conspiraci",
  "chase", "escape", "killer", "plan", "target",
  "mission", "evid", "interrog", "pursuit", "spy"
)
horror_words <- intersect(horror_words, colnames(X))
thriller_words <- intersect(thriller_words, colnames(X))

X_extra <- data.frame(
  horror_score = if(length(horror_words) > 0) rowSums(X[, horror_words, drop=FALSE]) else rep(0, nrow(X)),
  thriller_score = if(length(thriller_words) > 0) rowSums(X[, thriller_words, drop=FALSE]) else rep(0, nrow(X))
)

X <- cbind(X, X_extra)

# ciaooo

