# Web Scraper #

install.packages('rvest')
library(rvest)
install.packages('xml2')
library(xml2)
install.packages('ngram')
library(ngram)

library(tidyverse)

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