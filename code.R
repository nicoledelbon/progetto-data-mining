rm(list=ls())
IMDb <- read_csv("~/progetto-data-mining/IMDbMovies-Clean.csv")

data <- read.csv("C:/Users/HP/Downloads/archive (6)/IMDbMovies-Clean.csv", header=T)
sum(is.na(data))

unique(data$Main.Genres)

sum(data$Main.Genres=="")

idbm <- data[
  data$Main.Genres == "Action" |
    data$Main.Genres == "Comedy" |
    data$Main.Genres == "Documentary" |
    data$Main.Genres == "Drama" |
    data$Main.Genres == "Fantasy" |
    data$Main.Genres == "Film-Noir" |
    data$Main.Genres == "History" |
    data$Main.Genres == "Horror" |
    data$Main.Genres == "Mystery" |
    data$Main.Genres == "Romance" |
    data$Main.Genres == "Thriller" |
    data$Main.Genres == "Western", 
]
idbm[idbm==""] <- NA
sum(is.na(idbm))

table(idbm$Main.Genres)

library(tm)
desc <- idbm$Summary
docs <- Corpus(VectorSource(desc))
summary(docs)

# tolgo music sport war musical family 
# crime biography adventure animarion sci fi crime

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
# Rimozione dei numeri.
docs <- tm_map(docs, removeNumbers)

writeLines(as.character(docs[[4]]))

docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

library(SnowballC)
# Fase di Stemming
docs <- tm_map(docs,stemDocument)

dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:2,1000:1005])

freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
# Verifichiamo la frequenza di apparizione dei primi termini.
freq[head(ord)]

findAssocs(dtm,"famili", corlimit = 0.2)

library(wordcloud)
wordcloud(names(freq),freq, min.freq=60)

library(dendextend)
library(wordcloud)
library(colorspace)
# --- Clustering ---
m <- as.matrix(dtm)
d <- dist(m)  # se vuoi puoi usare cosine con proxy::dist(m, method="cosine")
groups <- hclust(d, method = "ward.D2")
cluster_cut <- cutree(groups, k = 7)

# Dendrogramma colorato
dend <- as.dendrogram(groups)
dend <- color_branches(dend, k = 7)
plot(dend, main = "Dendrogramma colorato per cluster")

# --- Analisi dei cluster ---
library(wordcloud)

# Imposta layout 4 righe x 2 colonne (lasciando una cella vuota)
par(mfrow = c(4, 2), mar = c(1,1,2,1))  # margini leggermente ridotti

for (i in 1:7) {
  # Documenti nel cluster
  cluster_docs <- dtm[cluster_cut == i, ]
  num_docs <- nrow(cluster_docs)
  
  # Frequenze dei termini nel cluster
  cluster_freq <- colSums(as.matrix(cluster_docs))
  
  # Titolo del cluster
  title_text <- paste("Cluster", i, "- N word:", num_docs)
  
  # Wordcloud
  wordcloud(names(cluster_freq), cluster_freq, min.freq = 2,
            max.words = 100, colors = rainbow(7)[i])
  title(main = title_text, line = -1, cex.main = 1)  # aggiunge titolo sopra la wordcloud
}

# Ripristina layout standard dopo
par(mfrow = c(1,1))
