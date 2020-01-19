library(ROCR)
library(randomForest)
library(xgboost)
library(caret)
library(Rtsne)
library(dplyr)
library(readxl)
library(rpart)
library(rpart.plot)
library(tseries)
library(forecast)
library(TTR)
library(quantmod)

source("util.R")

# Processing the data and performing EDA
# Set first_run = T to run the whole code

first_run <- F
if(first_run) {
  source("data_preprocessing.R")
  for(file in files) {
    for(processed in c("processed", "unprocessed")) {
      process_text(file = file, processed = processed)
    }
  }
  # Reading preprocessed data
  # setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("processed_China tweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png processed_China_wordcloud.png")
  
  setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("unprocessed_China tweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png unprocessed_China_wordcloud.png")
  
  setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("unprocessed_FarmerTweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png unprocessed_Farmer_wordcloud.png")
  
  setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("processed_FarmerTweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png processed_Farmer_wordcloud.png")
  
  setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("unprocessed_soybeans tweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png unprocessed_soybeans_wordcloud.png")
  
  setwd("../Shane_s_lab/Data_Graduate/")
  df1 <- readRDS("processed_soybeans tweets @realDonaldTrump.csv.Rds")
  setwd("../../Naveen_NLP/")
  source("plot_wordcloud.R")
  system("mv wordcloud.png processed_soybeans_wordcloud.png")
  
}


# Feature extraction code here
source("text_features.R")
df <- data.frame(fread("../Shane_s_lab/Data_Graduate/China tweets @realDonaldTrump.csv"))
df1 <- data.frame(fread("../Shane_s_lab/Data_Graduate/FarmerTweets @realDonaldTrump.csv"))
df <- rbind(df, df1)
df1 <-  data.frame(fread("../Shane_s_lab/Data_Graduate/soybeans tweets @realDonaldTrump.csv"))
df <- rbind(df, df1)
dtm <- data.frame(as.matrix(get_dtm(text = df$text, thr = 50)))

df <- cbind(data.frame(created_at = df$created_at), dtm)
df$created_at <- as.Date(df$created_at, format = "%m-%d-%Y")
df <- df[!is.na(df$created_at), ]
df <- df %>% group_by(created_at) %>% summarize_all(sum)

july_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsforJuly2020.CSV.xlsx",
  subset = T)
may_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsForMay2020.CSV.xlsx",
  subset = T)
march_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsForMarch2020.CSV.xlsx",
  subset = T)

saveRDS(df, "text_features.Rds")

df_july <- get_features(df, july_prices)
df_march <- get_features(df, march_prices)
df_may <- get_features(df, may_prices)
df_july$realdonaldtrump <- df_may$realdonaldtrump <-
  df_march$realdonaldtrump <- df_july$amp <-
  df_may$amp <- df_march$amp <- NULL

# Modeling code here
july_lm_model <- lm(Close ~ . - Open - High - Low - created_at, data = df_july)
july_lm_model <- step(july_lm_model, direction = "backward", trace = 0)
summary(july_lm_model)
july_rpart_model <- rpart(Close ~ . - Open - High - Low - created_at,
                          data = df_july)
png("july_tree_model.png", width = 568, height = 391)
rpart.plot(july_rpart_model)
dev.off()

march_lm_model <- lm(Close ~ . - Open - High - Low - created_at, data = df_march)
march_lm_model <- step(march_lm_model, direction = "backward", trace = 0)
summary(march_lm_model)
march_rpart_model <- rpart(Close ~ . - Open - High - Low - created_at,
                          data = df_march)
png("march_tree_model.png", width = 568, height = 391)
rpart.plot(march_rpart_model)
dev.off()

may_lm_model <- lm(Close ~ . - Open - High - Low - created_at, data = df_may)
may_lm_model <- step(may_lm_model, direction = "backward", trace = 0)
summary(may_lm_model)
may_rpart_model <- rpart(Close ~ . - Open - High - Low - created_at,
                         data = df_may)
png("may_tree_model.png", width = 568, height = 391)
rpart.plot(may_rpart_model)
dev.off()

july_prices1 <- get_residues(df_july, july_prices, july_lm_model)
march_prices1 <- get_residues(df_march, march_prices, march_lm_model)
may_prices1 <- get_residues(df_may, may_prices, may_lm_model)

ts1 <- ts(july_prices$Close, frequency = 220)
model <- decompose(ts1)
mdl <- auto.arima(model$random)

sma <-SMA(Cl(july_prices), n = 20)
m <- momentum(Cl(july_prices), n = 2)
ROC <- ROC(Cl(july_prices), n = 2)
