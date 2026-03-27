rm(list=ls())
data <- read.csv("~/progetto-data-mining/IMDbMovies-Clean.csv", header=T)
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

# music, musical, sport, adventure, animation, war
# hanno meno di 5 film appartenenti a quel genere 
# quindi non sarebbero sufficienti per allenare il mio modello

imdb = subset(imdb, !(imdb$Main.Genres %in% c("Music", "Musical", "Sport", "Biography",
                                              "War", "Adventure", "Animation", "Crime","Sci-Fi")))
imdb$Main.Genres=as.factor(imdb$Main.Genres)


library(tm)
desc <- imdb$Summary
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

dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
                                             bounds = list(global = c(30,990))))
freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr,decreasing=TRUE)
findFreqTerms(dtmr,lowfreq=30)

wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
ggplot(subset(wf, freqr>30), aes(term, occurrences)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

m<-as.matrix(dtmr)
d <- dist(m)
groups <- hclust(d,method="ward.D2")
plot(groups, hang=-1)


# con tidyverse -----------------------------------------------------------
library(tidyverse)
library(stringr)
library(tidytext)
df <- read.csv("IMDbMovies-Clean.csv")

# tiene il primo genere per ogni elenco
df$Main.Genres <- sapply(strsplit(df$Main.Genres, ","), `[`, 1)

sum(is.na(df$Main.Genres))
table(df$Main.Genres)
df = df %>%
  select(Summary, Main.Genres) %>%
  filter(!is.na(Main.Genres) & !Main.Genres %in% c("Film-Noir", "History","Music","Musical","Sport","War")) %>%
  mutate(Summary = tolower(Summary))

df$Main.Genres=as.factor(df$Main.Genres)

bow <- df %>%
  unnest_tokens(word, Summary) %>%
  anti_join(stop_words)

bow %>%
  count(word, sort = TRUE)

bow %>% 
  group_by(Main.Genres) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n=10, with_ties = FALSE)

df %>%
  unnest_tokens(word, Summary) %>%
  anti_join(stop_words) %>%
  group_by(Main.Genres) %>%
  count(word, sort = TRUE) %>%
  slice_max(n ,n=10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Main.Genres = factor(Main.Genres),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = Main.Genres)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Main.Genres, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")


# -------------------------------------------------------------------------

library(wordcloud)
wordcloud(names(freq),freq, min.freq=30)

library(dendextend)
library(wordcloud)
library(colorspace)
# --- Clustering ---
m <- as.matrix(dtm)
d <- dist(m)  # se vuoi puoi usare cosine con proxy::dist(m, method="cosine")
groups <- hclust(d, method = "ward.D2")
cluster_cut <- cutree(groups, k = 11)

# Dendrogramma colorato
dend <- as.dendrogram(groups)
dend <- color_branches(dend, k = 11)
plot(dend, main = "Dendrogramma colorato per cluster")

# --- Analisi dei cluster ---
library(wordcloud)

# Imposta layout 4 righe x 2 colonne (lasciando una cella vuota)
par(mfrow = c(4, 2), mar = c(1,1,2,1))  # margini leggermente ridotti

for (i in 1:11) {
  # Documenti nel cluster
  cluster_docs <- dtm[cluster_cut == i, ]
  num_docs <- nrow(cluster_docs)
  
  # Frequenze dei termini nel cluster
  cluster_freq <- colSums(as.matrix(cluster_docs))
  
  # Titolo del cluster
  title_text <- paste("Cluster", i, "- N word:", num_docs)
  
  # Wordcloud
  wordcloud(names(cluster_freq), cluster_freq, min.freq = 2,
            max.words = 100, colors = rainbow(11)[i])
  title(main = title_text, line = -1, cex.main = 1)  # aggiunge titolo sopra la wordcloud
}

# Ripristina layout standard dopo
par(mfrow = c(1,1))
