setwd("C:/Users/jaro9/OneDrive/Desktop/apu/zad7")

#load libraries
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("syuzhet")
#install.packages("ggplot2")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

#read text
text <- readLines("christopher_columbus.txt", warn=FALSE)

#convert text to object
TextDoc <- Corpus(VectorSource(text))

#### clean text ####
#remove special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) # Funkcja zamiany znaku
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, toSpace, "/")
#remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
#remove stop characters
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
#remove proprietary characters
TextDoc <- tm_map(TextDoc, removeWords, c("[", "]"))
#remove punctuation
TextDoc <- tm_map(TextDoc, removePunctuation)
#remove whitespaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
#change to basic form
TextDoc <- tm_map(TextDoc, stemDocument)
#to lower
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

#### build text matrix ####
#build matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
#sort descending based on how often word appears
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
#show 5 most often appearing
head(dtm_d, 5)

#### plot of most frequent words ####
barplot(
  dtm_d[1:20, ]$freq,
  las = 2,
  names.arg = dtm_d[1:20, ]$word,
  col = "lightgreen",
  main = "Top 20 most frequent words",
  ylab = "Word frequency"
)

##### generate word cloud #####
set.seed(1234)
wordcloud(
  words = dtm_d$word,
  freq = dtm_d$freq,
  scale = c(5, 0.5),
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.40,
  colors = brewer.pal(8, "Dark2")
)

##### Kojarzenia slow #####
#find association for these words
findAssocs(
  TextDoc_dtm, 
  terms = c("learn", "machine", "algorithm", "train"),
  corlimit = 0.5
)
#find asoociation for words that appear at least 20 times
findAssocs(
  TextDoc_dtm, 
  terms = findFreqTerms(TextDoc_dtm, lowfreq = 20),
  corlimit = 0.5
)

##### sentiment analysis #####
syuzhet_vector <- get_sentiment(text, method = "syuzhet")
bing_vector <- get_sentiment(text, method = "bing")
nrc_vector <- get_sentiment(text, method = "nrc")
#compare analysis
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(nrc_vector))
)

##### emotion classification #####
d <- get_nrc_sentiment(as.vector(dtm_d$word))
head(d,10)
#transpose
td <- data.frame(t(d))
#sum frequency of emotions for first 56 words
td_new <- data.frame(rowSums(td[1:56]))
#clear result
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2 <- td_new[1:8,]
#plot - words tied to emotions
quickplot(
  sentiment,
  data = td_new2,
  weight = count,
  geom = "bar",
  fill = sentiment,
  ylab = "count"
) + ggtitle("Survey sentiments")
#plot - percent of each emotion
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text",
  xlab = "Percentage"
)
