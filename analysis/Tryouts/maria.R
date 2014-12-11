library(ggplot2)

#Import Data
sales_cat3 <- read.csv("table1.csv")
sales_16 <- read.csv("table2.csv")

#Vector of days
range_days<-seq(as.integer(as.Date("1996-07-05", format= "%Y-%m-%d")),as.integer(as.Date("1998-05-06", format= "%Y-%m-%d")),1)


#Create resum table
resum <-data.frame(range_days)
names(resum)<- c("days")

##put information in resun:

resum$quantity_16<- rep(0,length(range_days))
resum$days_16<-rep(0,length(range_days))
k<-1
i<-1
for(i in 1:nrow(resum)){
  
  if(resum$days[i]==sales_16$days[k]){
    resum$quantity_16[i]<-sales_16$Quantity[k]
    resum$days_16[i]<-sales_16$days[k]
    k<-k+1
  }else{
    cat("i")
  }
}



########---------------------------
##X1=Sales of last week:

resum$X1_SalesWeek <- rep(0,nrow(resum))

for(i in (nrow(resum):7)){
  k<- i-6
  resum$X1_SalesWeek[i]<-sum(resum$quantity_16[k:i])
}

resum$X1_SalesWeekSTA <- sapply(resum$X1_SalesWeek,function(x) (x-week_mean_16)/week_sd_16)

week_mean_16<-mean(resum$X1_SalesWeek)
week_sd_16<-sd(resum$X1_SalesWeek)


########---------------------------
##X2=Sales of last month:

resum$X1_SalesWeek <- rep(0,nrow(resum))

for(i in (nrow(resum):7)){
  k<- i-6
  resum$X1_SalesWeek[i]<-sum(resum$quantity_16[k:i])
}

resum$X1_SalesWeekSTA <- sapply(resum$X1_SalesWeek,function(x) (x-week_mean_16)/week_sd_16)

week_mean_16<-mean(resum$X1_SalesWeek)
week_sd_16<-sd(resum$X1_SalesWeek)






#X2=


ggplot(data=sales_cat3, aes(x=OrderDate, y=Quantity, group=ProductID, colour=ProductID)) +
  geom_point()+geom_line()
# This looks ugly, I think our results are going to be....

sales_cat3$days <- as.integer(as.Date(sales_cat3[,2], format= "%Y-%m-%d"))
sales_16$days <- as.integer(as.Date(sales_16[,2], format= "%Y-%m-%d"))





