library(quantmod)
library(TTR)
march_prices <- read_price(
  "../do_s_playground/Data_Undergrad/ActiveSoybeanContractsForMarch2020.CSV.xlsx",
  subset = T)
df1 <- march_prices[march_prices$Date >= as.Date("2018-07-12"), ]
df2 <- march_prices[march_prices$Date >= as.Date('2019-05-10'), ]
df1 <- df1[1:40, ]
df2 <- df2[1:40, ]

plot(df1$Close, ylim = c(min(c(df1$Close, df2$Close)),
                         max(c(df1$Close, df2$Close))), type = 'l',
     xlab = "Days from political event", ylab = "Close Price")
lines(df2$Close, col = "red")
legend("bottomright", legend = c("Event 3", "Event 5"), fill = c("black", "red"))

cor(diff(df1$Close), diff(df2$Close))
cor(df1$Close, df2$Close)
# march_prices <- march_prices[march_prices$Date >= as.Date("2018-01-01"), ]
# colnames(march_prices)[-1] <- paste0("march.", colnames(march_prices)[-1])
# march_prices$delta_Close <- c(NA, diff(march_prices$Close))
march_prices <- xts(march_prices[, -1], order.by = march_prices$Date, frequency = 5)

chartSeries(march_prices, theme = chartTheme('white'),
            TA = c(addRSI(n = 14, maType = "SMA"))
            )
abline(v = as.Date("2018-02-01"), col = "red", lwd = 4)
