# Web Scraper #

#install.packages('rvest')
library(rvest)
#install.packages('xml2')
library(xml2)
#install.packages('ngram')
library(ngram)
library(tidyverse)
#install.packages('corpus')
library(corpus)
#install.packages('tidytext')
library(tidytext)

library(rlan)
# Read the Book as text
url <- 'https://www.gutenberg.org/files/2600/2600-h/2600-h.htm'
webpage <- read_html(url)

book_html <- html_elements(webpage,'p')

# Grab Chapter 1
book <- html_text(book_html)

#Let's have a look at the rankings
book_new <- gsub('\r\n      ', ' ', book)
book_newer <- gsub('\r\n    ', ' ', book_new)
book_newest <- gsub("[\r\n]", "", book_newer)

b2 <- toString(book_newest)

#check it
b3 <- gsub('[[:punct:] ]+',' ', b2)
b4 <- as.data.frame(book)

b5 <- data.frame(txt = b4$book,
           stringsAsFactors = FALSE)

unigrams <- b5 %>% 
  unnest_tokens(word, txt) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE) %>%
  filter(word != "NA")

bigrams <- b5 %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>%
  filter(word != "NA NA")


trigrams <- b5 %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = TRUE) %>%
  filter(word != "NA NA NA")

fourgrams <- b5 %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 4) %>% 
  separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
#  filter(!word1 %in% stop_words$word) %>%
#  filter(!word2 %in% stop_words$word) %>% 
#  filter(!word3 %in% stop_words$word) %>% 
#  filter(!word4 %in% stop_words$word) %>% 
  unite(word,word1, word2, word3, word4, sep = " ") %>% 
  count(word, sort = TRUE) %>%
  filter(word != "NA NA NA NA")

fivegrams <- b5 %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 5) %>% 
  separate(word, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>% 
#  filter(!word1 %in% stop_words$word) %>%
#  filter(!word2 %in% stop_words$word) %>% 
#  filter(!word3 %in% stop_words$word) %>% 
#  filter(!word4 %in% stop_words$word) %>% 
#  filter(!word5 %in% stop_words$word) %>% 
  unite(word,word1, word2, word3, word4, word5, sep = " ") %>% 
  count(word, sort = TRUE) %>%
  filter(word != "NA NA NA NA NA")

#install.packages('gt')
library(gt)


unigrams %>%
  subset(n > 1000) %>%
  gt() %>%
  tab_header(
    title = "War and Peace Text Analysis",
    subtitle = "1-grams"
  )

bigrams %>%
  subset(n > 1000) %>%
  gt() %>%
  tab_header(
    title = "War and Peace Text Analysis",
    subtitle = "2-grams"
  )

trigrams %>%
  subset(n > 1000) %>%
  gt() %>%
  tab_header(
    title = "War and Peace Text Analysis",
    subtitle = "3-grams"
  )

fourgrams %>%
  subset(n > 10) %>%
  gt() %>%
  tab_header(
    title = "War and Peace Text Analysis",
    subtitle = "4-grams"
  )

fivegrams %>%
  subset(n > 10) %>%
  gt() %>%
  tab_header(
    title = "War and Peace Text Analysis",
    subtitle = "5-grams"
  )

install.packages('wordcloud')
library(wordcloud)

wordcloud(words = unigrams$word, freq = unigrams$n, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud(words = bigrams$word, freq = bigrams$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = trigrams$word, freq = trigrams$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = fourgrams$word, freq = fourgrams$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = fivegrams$word, freq = fivegrams$n, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))