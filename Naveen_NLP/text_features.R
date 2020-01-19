library(tm)

get_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

get_tfidf <- function(text, thr = 20, norm = F) {
  corpus <- get_corpus(text)
  summary_tfidf <- TermDocumentMatrix(corpus, control = list(
    weighting = function(x) weightTfIdf(x, normalize = norm),
    stopwords = T, bounds = list(global = c(thr, Inf))))
  print(summary_tfidf)
  summary_tfidf <- as.matrix(summary_tfidf)
  return(t(summary_tfidf))
}

get_dtm <- function(text, thr = 20, norm = F) {
  corpus <- get_corpus(text)
  summary_tfidf <- TermDocumentMatrix(corpus, control = list(
    weighting = function(x) weightTf(x),
    stopwords = T, bounds = list(global = c(thr, Inf))))
  print(summary_tfidf)
  summary_tfidf <- as.matrix(summary_tfidf)
  return(t(summary_tfidf))
}
