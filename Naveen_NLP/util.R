read_price <- function(in_file, delta_price = F, subset = F) {
  prices <- data.frame(read_excel(in_file, skip = 3))
  prices$Date <- as.Date(prices$Date, format = "%Y-%m-%d")
  prices <- prices[order(prices$Date), ]
  if(delta_price) {
    prices$Open <- c(NA, diff(prices$Open))
    prices$Close <- c(NA, diff(prices$Close))
    prices$High <- c(NA, diff(prices$High))
    prices$Low <- c(NA, diff(prices$Low))
  }
  if(subset) {
    prices <- prices[prices$Date >= as.Date("2017-01-01", format = "%Y-%m-%d"), ]
  }
  return(prices)
}

get_features <- function(df, prices) {
  df <- merge(df, prices, by.x = "created_at", by.y = "Date")
  df <- df[, sapply(df, sd) != 0]
  return(df)
}

backfill_na <- function(series) {
  na_idx <- which(is.na(series))
  while(length(na_idx) > 0) {
    series[na_idx] <- series[na_idx + 1]
    na_idx <- which(is.na(series))
  }
  return(series)
}

get_residues <- function(df, prices, model) {
  pred_df <- data.frame(Date = df$created_at,
                        pred_Close = predict(model, df),
                        stringsAsFactors = F)
  prices$Date_cut <- as.character(cut(x = prices$Date,
                                      breaks = pred_df$Date))
  prices$Date_cut[nrow(prices)] <-
    as.character(pred_df$Date[nrow(pred_df)])
  prices$Date_cut[is.na(prices$Date_cut)] <-
    prices$Date_cut[!is.na(prices$Date_cut)][1]
  prices$Date1 <- as.Date(prices$Date_cut, format = "%Y-%m-%d")
  prices <- merge(prices, pred_df, on.x = "Date1",
                  on.y = "Date", all.x = T)
  prices$Date1 <- prices$Date_cut <- NULL
  
  prices$pred_Close <- backfill_na(prices$pred_Close)
  
  prices$Close <- prices$Close - prices$pred_Close
  prices$pred_Close <- NULL
  return(prices)
}