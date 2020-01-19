library(data.table)
library(dplyr)
source("text_features.R")
source("text_preprocessing.R")

first_run_flag <- T
setwd("../Shane_s_lab/Data_Graduate/")
files <- list.files(pattern = ".csv$")

process_text <- function(file, processed = "processed") {
  if(!(paste0(processed, "_", file, ".Rds") %in% files)) {
    reviews <- data.frame(fread(file), stringsAsFactors = F)
    text_reviews <- as.character(reviews$text)
    
    text_reviews <- tolower(text_reviews)
    
    text_reviews <- clean_text(text_reviews)
    if(processed != "processed") {
      review_polarity <- sapply(1:length(text_reviews), function(i) {
        review <- text_reviews[i]
        review <- clean_text(review)
        corpus <- Corpus(VectorSource(review))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        tryCatch({
          corpus <- tm_map(corpus, removeWords, readLines("stopwords.txt"))
        }, error = function(e) {
          corpus <- tm_map(corpus, removeWords,
                           readLines("../../Naveen_NLP/stopwords.txt"))
        })
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, stemDocument, language="english")
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        tryCatch({
          corpus <- tm_map(corpus, removeWords, readLines("stopwords.txt"))
        }, error = function(e) {
          corpus <- tm_map(corpus, removeWords,
                           readLines("../../Naveen_NLP/stopwords.txt"))
        })
        review <- corpus[[1]]$content
        print(i)
        annotated <- cnlp_annotate(input = review)
        sentences <- c()
        for(sid1 in unique(annotated$token$sid)) {
          annotated1 <- annotated$token[annotated$token$sid == sid1, ]
          annotated1$word[annotated1$lemma != "-PRON-"] <-
            annotated1$lemma[annotated1$lemma != "-PRON-"]
          annotated1 <- annotated1[!is.na(annotated1$cid), ]
          sentences <- c(sentences, paste0(annotated1$word, collapse = " "))
        }
        pols <- sapply(sentences, function(sentence) polarity(sentence))
        min_max_polarity <- range(sapply(pols, function(polar) polar$polarity))
        pol <- polarity(sentences)
        df <- data.frame(avg_polarity = pol$group$ave.polarity,
                         max_polarity = min_max_polarity[2],
                         min_polarity = min_max_polarity[1],
                         sd_polarity = pol$group$sd.polarity,
                         useful_text = paste0(sentences, collapse = ". "),
                         stringsAsFactors = F)
        return(df)
      })
      review_polarity <- data.frame(
        avg_polarity = unlist(review_polarity["avg_polarity", ]),
        max_polarity = unlist(review_polarity["max_polarity", ]),
        min_polarity = unlist(review_polarity["min_polarity", ]),
        sd_polarity = unlist(review_polarity["sd_polarity", ]),
        useful_text = unlist(review_polarity["useful_text", ]),
        stringsAsFactors = F)
      saveRDS(review_polarity, paste0(processed, "_", file, ".Rds"))
    } else {
      review_polarity <- get_polarity_df(
        text_reviews, save_file = paste0(processed, "_", file, ".Rds"))
    }
  }
  else {
    review_polarity <- readRDS(paste0(processed, "_", file, ".Rds"))
  }
  rm(list = setdiff(ls(), lsf.str()))
  gc()
}
