library(ggplot2)
library(scales)
# Crop Progress percentage data
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set the working directory
crop_progress_17 <- read.csv(file = 'SOYBEANS-CropProgress-2017.csv', header = TRUE, sep = ',')
crop_progress_18 <- read.csv(file = 'SOYBEANS-CropProgress-2018.csv', header = TRUE, sep = ',')
crop_progress_19 <- read.csv(file = 'SOYBEANS-CropProgress-2019.csv', header = TRUE, sep = ',')
crop_progress_17$WEEK.ENDING <- as.Date(crop_progress_17$WEEK.ENDING, "%Y-%m-%d")
crop_progress_18$WEEK.ENDING <- as.Date(crop_progress_18$WEEK.ENDING, "%Y-%m-%d")
crop_progress_19$WEEK.ENDING <- as.Date(crop_progress_19$WEEK.ENDING, "%Y-%m-%d")
crop_progress <- rbind(crop_progress_17[-c(2:5,7)], crop_progress_18[-c(2:5,7)], crop_progress_19[-c(2:5,7)])
crop_progress$WEEK.ENDING <- as.Date(crop_progress$WEEK.ENDING, "%Y-%m-%d")
# Contracts price
library(readxl)
ContractsforJuly2020 <- read_excel("ActiveSoybeanContractsforJuly2020.CSV", skip = 3)
ContractsforMarch2020 <- read_excel("ActiveSoybeanContractsForMarch2020.CSV", skip = 3)
ContractsforMay2020 <- read_excel("ActiveSoybeanContractsForMay2020.CSV", skip = 3)
ContractsforJuly2020$Date <- as.Date(ContractsforJuly2020$Date, "%Y-%m-%d")
ContractsforMarch2020$Date <- as.Date(ContractsforMarch2020$Date, "%Y-%m-%d")
ContractsforMay2020$Date <- as.Date(ContractsforMay2020$Date, "%Y-%m-%d")


# diff across rows
group_diff <- function(df){
   tmp <- apply(df[-c(1:8)], MARGIN = 2, FUN = function(x) diff(x))
   tmp
 }
crop_progress_17_diff <- group_diff(crop_progress_17)
crop_progress_18_diff <- group_diff(crop_progress_18)
crop_progress_19_diff <- group_diff(crop_progress_19)


# plot by graphics in base
plt <- function(df1_progress, df2_contracts) {
  date_1 <- df1_progress$WEEK.ENDING - 2
  date_2 <- df1_progress$WEEK.ENDING + 2
  price_1 <- df2_contracts[df2_contracts$Date %in% date_1, ]
  price_2 <- df2_contracts[df2_contracts$Date %in% date_2, ]
  table = df1_progress
  par(mar = c(5,5,2,5))
  plot(table$WEEK.ENDING,table$PROGRESS.in.PCT.PLANTED,type="l",col="red", lwd = 2, xlab = 'Timeline', ylab = 'Progress(%)')
  text = c('Planted', 'Emerged', 'Blooming', 'SettingPods', 'DroppingLeaves', 'Harvested')
  color = c('red','green','darkgreen','purple','brown','gold')
  legend(x = as.Date('2017-04-01'), y = 100,  legend = text, text.width = strwidth(text)[5]/100,
         col = color,lty = 1, bty= "n",
         cex = 0.76, seg.len=0.4, text.col = color)
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.PLANTED,lty = 2,col="red", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.PLANTED,lty = 3,col="red", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS.in.PCT.EMERGED,col="green", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.EMERGED,lty = 2,col="green", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.EMERGED,lty = 3,col="green", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS.in.PCT.BLOOMING,col="darkgreen", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.BLOOMING,lty = 2,col="darkgreen", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.BLOOMING,lty = 3, col="darkgreen", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS.in.PCT.SETTING.PODS,col="purple")
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.SETTING.PODS,lty = 2,col="purple", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.SETTING.PODS,lty = 3, col="purple", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS.in.PCT.DROPPING.LEAVES,col="brown")
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.DROPPING.LEAVES,lty = 2,col="brown", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.DROPPING.LEAVES,lty = 3, col="brown", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS.in.PCT.HARVESTED,col="gold")
  lines(table$WEEK.ENDING,table$PROGRESS..5.YEAR.AVG.in.PCT.HARVESTED, lty = 2,col="gold", lwd = 2)
  lines(table$WEEK.ENDING,table$PROGRESS..PREVIOUS.YEAR.in.PCT.HARVESTED, lty = 3, col="gold", lwd = 2)
  par(new = T)
  plot(price_1$Date, price_1$Close, pch=16, axes = F , type = 'b', xlim = range(table$WEEK.ENDING), ylim = range(price_1$Open), xlab=NA, ylab=NA, col = 'grey')
  lines(price_2$Date, price_2$Close, pch=16, axes = F, type = 'b', xlim = range(table$WEEK.ENDING), ylim = range(price_2$Open), xlab=NA, ylab=NA)
  mtext(side = 4, line = 3, 'Contract price')
  axis(side = 4) #add a right axis
  text = c('before', 'after')
  legend('bottomright', legend = text, lwd = 1, col = c('grey','black'), text.width = strwidth(text)[1]/10, 
         pch = 19, cex = 0.9, seg.len=0.5)
}
{plt(df1_progress = crop_progress_17, df2_contracts = ContractsforMarch2020)
title("Crop_progress_17 & March Contracts")
plt(df1_progress = crop_progress_17, df2_contracts = ContractsforMay2020)
title("Crop_progress_17 & May Contracts")
plt(df1_progress = crop_progress_17, df2_contracts = ContractsforJuly2020)
title("Crop_progress_17 & July Contracts")
plt(df1_progress = crop_progress_18, df2_contracts = ContractsforMarch2020)
title("Crop_progress_18 & March Contracts")
plt(df1_progress = crop_progress_18, df2_contracts = ContractsforMay2020)
title("Crop_progress_18 & May Contracts")
plt(df1_progress = crop_progress_18, df2_contracts = ContractsforJuly2020)
title("Crop_progress_18 & July Contracts")
plt(df1_progress = crop_progress_19, df2_contracts = ContractsforMarch2020)
title("Crop_progress_19 & March Contracts")
plt(df1_progress = crop_progress_19, df2_contracts = ContractsforMay2020)
title("Crop_progress_19 & May Contracts")
plt(df1_progress = crop_progress_19, df2_contracts = ContractsforJuly2020)
title("Crop_progress_19 & July Contracts") }


# plot by ggplot2
plt_pretty <- function(df1_progress, df2_contracts){
  # Close prices of last business day and the following business day
  date_1 <- df1_progress$WEEK.ENDING - 2
  date_2 <- df1_progress$WEEK.ENDING + 2
  price_1 <- df2_contracts[df2_contracts$Date %in% date_1, 'Close']
  price_2 <- df2_contracts[df2_contracts$Date %in% date_2, 'Close']
  tmp1 <- cbind(Date = date_1, Close = price_1)
  tmp2 <- cbind(Date = df2_contracts[df2_contracts$Date %in% date_2, "Date"], Close = price_2) # pair the date in records
  gap1 <- max(tmp1$Close) - min(tmp1$Close)
  gap2 <- max(tmp2$Close) - min(tmp2$Close)
  
  ggplot(data = df1_progress) + 
    geom_point(data = tmp1,aes(x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) + 
    geom_line(data = tmp1,aes(x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) + 
    geom_point(data = tmp2,aes(x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) + 
    geom_line(data = tmp2,aes(x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.PLANTED, colour = "Planted", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.PLANTED, colour = "Planted", linetype = 'Current Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.PLANTED, colour = "Planted", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.PLANTED, colour = "Planted", linetype = 'Previous Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.EMERGED, colour = "Emerged", linetype = 'Current Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.EMERGED, colour = "Emerged", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.EMERGED, colour = "Emerged", linetype = 'Previous Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.BLOOMING, colour = "Blooming", linetype = 'Current Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.BLOOMING, colour = "Blooming", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.BLOOMING, colour = "Blooming", linetype = 'Previous Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = 'Current Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = 'Previous Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = 'Current Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = 'Previous Year')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.HARVESTED, colour = "Harvested", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.HARVESTED, colour = "Harvested", linetype = '5 Years Average')) + 
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.HARVESTED, colour = "Harvested", linetype = 'Previous Year')) +
    scale_x_date(name = "Time", date_breaks = "2 weeks", date_labels = "%m-%d") +
    scale_y_continuous("Crop Progress in percentage", sec.axis = sec_axis(~ (. * gap1 /100) + min(tmp1$Close) ,breaks = function(x) pretty(x, n=10),  name = "Price")) +
    scale_linetype_manual("Variabler",values=c("Current Year"=1, "5 Years Average" = 2, "Previous Year" = 3)) +
    scale_color_manual("Stage",values = c("brown","darkgreen","green","gold","red","purple")) + 
    scale_alpha_manual("Price before/after the report", values = c(1, 1/5)) + 
    theme_bw() + coord_fixed()
}
plt_pretty(df1_progress = crop_progress_17, df2_contracts = ContractsforMarch2020) + labs(title = "Crop_progress_17 vs March Contract price")
plt_pretty(df1_progress = crop_progress_17, df2_contracts = ContractsforMay2020) + labs(title = "Crop_progress_17 vs May Contract price")
plt_pretty(df1_progress = crop_progress_17, df2_contracts = ContractsforJuly2020) + labs(title = "Crop_progress_17 vs July Contract price")
plt_pretty(df1_progress = crop_progress_18, df2_contracts = ContractsforMarch2020) + labs(title = "Crop_progress_18 vs March Contract price")
plt_pretty(df1_progress = crop_progress_18, df2_contracts = ContractsforMay2020) + labs(title = "Crop_progress_18 vs May Contract price")
plt_pretty(df1_progress = crop_progress_18, df2_contracts = ContractsforJuly2020) + labs(title = "Crop_progress_18 vs July Contract price")
plt_pretty(df1_progress = crop_progress_19, df2_contracts = ContractsforMarch2020) + labs(title = "Crop_progress_19 vs March Contract price")
plt_pretty(df1_progress = crop_progress_19, df2_contracts = ContractsforMay2020) + labs(title = "Crop_progress_19 vs May Contract price")
plt_pretty(df1_progress = crop_progress_19, df2_contracts = ContractsforJuly2020) + labs(title = "Crop_progress_19 vs July Contract price")

