library(readxl)
source("text_features.R")
source("util.R")

df <- data.frame(readRDS("text_features.Rds"))
colnames(df)[1] <- "Date"

july_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsforJuly2020.CSV.xlsx",
  subset = T)
colnames(july_prices)[2:ncol(july_prices)] <-
  paste0("july_", colnames(july_prices)[2:ncol(july_prices)])
july_prices$delta_july_Close <- c(NA, diff(july_prices$july_Close))
july_prices$delta_july_Close_sign <- sign(july_prices$delta_july_Close)

march_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsForMarch2020.CSV.xlsx",
  subset = T)
colnames(march_prices)[2:ncol(march_prices)] <-
  paste0("march_", colnames(march_prices)[2:ncol(march_prices)])
march_prices$delta_march_Close <- c(NA, diff(march_prices$march_Close))
march_prices$delta_march_Close_sign <- sign(march_prices$delta_march_Close)

may_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsForMay2020.CSV.xlsx",
  subset = T)
colnames(may_prices)[2:ncol(may_prices)] <-
  paste0("may_", colnames(may_prices)[2:ncol(may_prices)])
may_prices$delta_may_Close <- c(NA, diff(may_prices$may_Close))
may_prices$delta_may_Close_sign <- sign(may_prices$delta_may_Close)

df2 <- merge(df, july_prices)
df2 <- merge(df2, march_prices)
df2 <- merge(df2, may_prices)
df2 <- df2[, sapply(df2, sd) != 0]
df2$china_tariffs <- as.integer((df2$china > 0) & (df2$tariffs > 0))
df2$china_currency <- as.integer((df2$china > 0) & (df2$currency > 0))
df2$china_jobs <- as.integer((df2$china > 0) & (df2$jobs > 0))
# sapply(df1[, 2:54], function(col) cor(col, df1$delta_Close_sign))
# colnames(df1)


create_corr_plot = function(independent_var_names, dependent_var_names, df) {
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  independent_var = independent_var_names
  independent_var <- setdiff(
    independent_var, c(
      "amp", "get", "want", "many", "will", "just", "want", "can", "realdonaldtrump",
      "made", "going", "must", "make"))
  dependent_var = dependent_var_names
    corr_mat = cor(x = df[, independent_var],
                   y = df[, dependent_var],
                   method = "pearson")
    corr_mat <- corr_mat[c("china", "trade", "money", "deal", "tariffs", "economy",
                           "currency", "china_jobs"),
                         c("delta_july_Close", "delta_may_Close", "delta_march_Close",
                           "delta_july_Close_sign", "delta_may_Close_sign", "delta_march_Close_sign")]
    
  return (corrplot(corr_mat,
                   tl.cex = .6, 
                   method = "color", hclust.method = "ward"))
  
}

independent_var_names <- setdiff(
  colnames(df2), c(colnames(df2)[grep(colnames(df2), pattern = "_Close$")],
                   colnames(df2)[grep(colnames(df2), pattern = "_Open$")],
                   colnames(df2)[grep(colnames(df2), pattern = "_High$")],
                   colnames(df2)[grep(colnames(df2), pattern = "_Low$")],
                   colnames(df2)[grep(colnames(df2), pattern = "_sign$")],
                   "Date"))
dependent_var_names <- c(colnames(df2)[grep(colnames(df2), pattern = "_Close$")],
                         colnames(df2)[grep(colnames(df2), pattern = "_Open$")],
                         colnames(df2)[grep(colnames(df2), pattern = "_High$")],
                         colnames(df2)[grep(colnames(df2), pattern = "_Low$")],
                         colnames(df2)[grep(colnames(df2), pattern = "_sign$")])

create_corr_plot(independent_var_names, dependent_var_names, df2)
