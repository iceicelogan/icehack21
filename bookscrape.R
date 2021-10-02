# Web Scraper #

install.packages('rvest')
library(rvest)
install.packages('xml2')
library(xml2)
install.packages('ngram')
library(ngram)

library(tidyverse)

install.packages('corpus')
library(corpus)

# Read the Book as text
url <- 'https://www.gutenberg.org/files/2600/2600-h/2600-h.htm'
webpage <- read_html(url)

book_html <- html_nodes(webpage,'p')

# Grab Chapter 1
book <- html_text(book_html)

#Let's have a look at the rankings
book_new <- gsub('\r\n      ', ' ', book)
book_newer <- gsub('\r\n    ', ' ', book_new)
book_newest <- gsub("[\r\n]", "", book_newer)

#check it
head(book_newest, 5)

# Create Corpus
book_corpus <- as_corpus_text(book_newest)

# Test ngram
install.packages('tm')
install.packages('tau')
library(tm)
library(tau)

tokenize_ngrams <- function(x, n=3) return(
  rownames(
    as.data.frame(
      unclass(
        textcnt(
          x,method="string",n=n)))))

#texts <- c("This is the first document.", "This is the second file.", "This is the third text.")
corpus <- Corpus(VectorSource(book_newest))
matrix <- DocumentTermMatrix(corpus,control=list(tokenize=tokenize_ngrams))


#TextDoc_dtm <- TermDocumentMatrix(book_newest)
#dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
#dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
#dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
#head(dtm_d, 5)


# Load the data as a corpus
TextDoc <- Corpus(VectorSource(book_newest))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)