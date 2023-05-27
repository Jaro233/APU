setwd("C:/Users/jaro9/OneDrive/Desktop/apu/zad7")

#install.packages("tidytext")
#install.packages("igraph")
#install.packages("ggraph")
library("tidytext")
library("igraph")
library("ggraph")
library(dplyr)
library(tidyr)

##### read file #####
text <- readChar("christopher_columbus.txt", file.info("christopher_columbus.txt")$size)
text_df <- data.frame(
  line = 1, 
  text = text)
text_df

##### set up text #####
#clear text
tidy_text <- text_df %>%
  unnest_tokens(word, text)

#set up and clear old words, stop words
data(stop_words)
de <- data.frame("thy", "OLD_WORDS")
names(de) <- c("word", "lexicon")
stop_words <- rbind(stop_words, de)
de <- data.frame("1", "OLD_WORDS")
names(de) <- c("word", "lexicon")
de <- data.frame("hath", "OLD_WORDS")
names(de) <- c("word", "lexicon")
de <- data.frame("mar'd", "OLD_WORDS")
names(de) <- c("word", "lexicon")
stop_words <- rbind(stop_words, de)

tidy_text <- tidy_text %>%
  anti_join(stop_words)

##### list of word frequency #####
tidy_text %>%
  count(word, sort = TRUE)

##### Bigramy #####
#ngram 2 elementowy
text_bigrams <- text_df %>%
  unnest_tokens(
    bigram, 
    text, 
    token = "ngrams", 
    n = 2)

#display
text_bigrams

#sort based on frequency
text_bigrams %>%
  count(bigram, sort = TRUE)

#Podziel bigram na 2 kolumnowa strukture
bigrams_separated <- text_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")

#Usun wiersze wyst?puj?ce w stop_words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#liczebnosc nowego bigramu
bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigrams_counts

#polacz kolumny ponownie
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

##### Konstruowanie grafow #####
# Znajdz tylko relatywnie czeste kombinacje
bigram_graph <- bigrams_counts %>%
  filter(word1 == "algorithm" | word2 == "algorithm") %>%
  graph_from_data_frame()

bigram_graph4 <- bigrams_counts %>%
  filter(word1 == "machine" | word2 == "machine") %>%
  graph_from_data_frame()

bigram_graph5 <- bigrams_counts %>%
  filter(word1 == "software" | word2 == "software") %>%
  graph_from_data_frame()


##### Wyswietlanie grafow #####
dev.new()
ggraph(bigram_graph4, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = NULL, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), position = "identity") + 
  theme_void()

dev.new(width = 550, height = 330, unit = "px")
ggraph(bigram_graph5, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = NULL, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), position = "identity") + 
  theme_void()

# Pierwszy poziom
bigram_graph1 <- bigrams_counts %>%
  filter(word1 %in% c("text", "characters") | word2 %in% c("text", "characters"))

# Drugi poziom
bigram_graph2 <- bigrams_counts %>%
  filter(word1 %in% bigram_graph1$word1 | word1 %in% bigram_graph1$word2 | 
           word2 %in% bigram_graph1$word1 | word2 %in% bigram_graph1$word2)

# Trzeci poziom
bigram_graph3 <- bigrams_counts %>%
  filter(word1 %in% bigram_graph2$word1 | word1 %in% bigram_graph2$word2 | 
           word2 %in% bigram_graph2$word1 | word2 %in% bigram_graph2$word2)

bigram_graph

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph1 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(bigram_graph2 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

dev.new()
ggraph(bigram_graph3 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n + 5), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

