library("readxl")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("cowplot")

#loop-read data
file_list <- list.files(path="C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/WASDE")

data=data.frame()
com_data=readxl::read_xls(file_list[1], "Page 15", range='A13:A58',col_names = 'SOYBEANS')%>%slice(-c(3, 5, 17:21, 33:37))
com_data$SOYBEANS[15:25]=paste0('Oil-',com_data$SOYBEANS[15:25])
com_data$SOYBEANS[26:34]=paste0('Meal-',com_data$SOYBEANS[26:34]) 
for (i in 1:length(file_list)){
  newdata=readxl::read_xls(file_list[i], "Page 15", range ="E13:E58",col_names =substr(file_list[i],start=1,stop=8))%>%slice(-c(3, 5, 17:21, 33:37))
  com_data=cbind(com_data,newdata)
}

#remove hifen
#if they have hiphen, split into two string and make them numeric then calculate the average
test=grepl("-",as.matrix(com_data[,-1]))
test=matrix(test,dim(com_data))
temp  <- strsplit(as.character(com_data[,-1][test]), "-")
non_hyphen=sapply(temp, function(x) mean(as.numeric(x)))
com_data[,-1][test]=non_hyphen

#remove asterisk
com_data[,-1]=apply(com_data[,-1],2,function(x){gsub("\\*",'',x)})

#as.numeric
com_data[,-1]=apply(com_data[,-1],2,as.numeric)

#transpose the data
trans_data=as.data.frame(t(com_data[,-1]))  
colnames(trans_data)=com_data[,1]
trans_data=cbind(rownames(trans_data),trans_data)
colnames(trans_data)[1]='Date'
trans_data$Date=as.Date(trans_data$Date, format = "%m-%d-%y")

#prepare the price data
price_prep=function(file,p){
  price=readxl::read_xlsx(file,skip = 3,col_types = c('date','numeric','numeric','numeric','numeric'))%>% select(Date,Open,Close)
  price$Date=as_date(price$Date,tz = NULL)
  colnames(price)[-1]=paste0(p,colnames(price[-1]))
  return(price)
}

march=price_prep("C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/ActiveSoybeanContractsForMarch2020.CSV.xlsx",'Mar-')
may=price_prep("C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/ActiveSoybeanContractsForMay2020.CSV.xlsx",'May-')
july=price_prep("C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/ActiveSoybeanContractsForJuly2020.CSV.xlsx",'July-')
price_data=merge(merge(march,may,by=c('Date','Date')),july,by=c('Date','Date'))


#merge three price dataset into one dataset
m=dplyr::left_join(price_data,trans_data,by='Date')

#calculate the average price of from the day after the report day to day before next report day
#create index and group variables
m$index=ifelse(apply(!is.na(m[,-c(1:7)]),1, any),1,0)
m$group=NA
m$group[1]=0
for (i in 2:dim(m)[1]) {
  if (m$index[i]==1){
    m$group[i]=m$group[i-1]+1
  }
  else {m$group[i]=m$group[i-1]}
}

#summarise
avg_m=m%>%group_by(group)%>%summarise_at(vars('Mar-Open':'July-Close'),mean)
wasde=cbind(avg_m[-1,-1],merge(price_data,trans_data,by='Date')) 
wasde=wasde[,-c(8:14)]
wasde$Date=as_date(wasde$Date,tz = NULL)
wasde=wasde%>%arrange(Date)%>% select(Date,everything())

#price change
change=m[which(m$index==1),2:7]-m[which(m$index==1)-1,2:7]
colnames(change)=paste0("Change-",colnames(change))
wasde=cbind(wasde,change)

readr::write_csv(wasde,'wasde.csv',col_names = T)

#Price and Wasde report date
color=c("blue","orange","forestgreen","red","deeppink",'brown')
matplot(wasde[,1],wasde[,2:7], type = c("b"),pch=1,col = color,main='Price and Wasde report date',xlab = 'Date',ylab='Price',xaxt = "n") #plot
axis(1, wasde$Date, format(wasde$Date, "%m-%d-%y") ,cex.axis = .56,las=2)
legend("bottomleft",legend = colnames(wasde)[2:7], col=color,pch =1,cex = 0.6,bty="n",inset = c(0.01, 0.01),pt.cex = 0.6) #legend

#Price change and Wasde report date
matplot(wasde[,1],wasde[,42:47], type = c("b"),pch=1,col = color,main='Price change and Wasde report date',xlab = 'Date',ylab='Price change',xaxt = "n") #plot
axis(1, wasde$Date, format(wasde$Date, "%m-%d-%y") ,cex.axis = .5,las=2,pch=16)
legend("bottomleft",legend = colnames(wasde)[42:47], col=color,pch =1,cex = 0.6,bty="n",inset = c(0.01, 0.01),pt.cex = 0.6) #legend

#plot2
library(ggplot2)
library(reshape2)

meltdf <- melt(wasde[,1:7],id="Date")
ggplot(meltdf, aes(
  x = Date,
  y = value,
  group = variable,
  colour = variable
)) + scale_x_date(date_labels = "%Y-%m-%d") + geom_line() + geom_point(aes(shape = variable))

#heatmap 
cormat <- round(cor(wasde[,42:47],wasde[,8:41]),2)
melted_cormat <- melt(cormat)
colnames(melted_cormat)[3]='Correlation'
ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = Correlation)) + geom_tile() +
  scale_fill_gradient(low = 'purple', high = 'green') + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  


