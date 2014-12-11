library(ggplot2)


################################################################
################      FUNCTIONS    #############################
################################################################

#Creating sum of standarized:
# r=6 week, r=29 month, r=89 trimestre
sumquantities<-function(X,r){
  data<-rep(0,length(X))
  for(i in (length(X):r)){ 
    k<- i-r
    data[i]<-sum(X[k:i])
  }
  mean<-mean(data)
  sd<-sd(data)
  da <- sapply(data,function(x) (x-mean)/sd)
  return(da)
}

##Putting info in Resum table
##X:days in resum, YD:days in product, YD:quantity of product
resumtable<- function(X,YD,YQ){
  data <- rep(0,length(range_days))
  k<-1
  for(i in 1:nrow(resum)){
    if(X[i]==YD[k]){
      data[i]<-YQ[k]
      k<-k+1
    }
  }
  return(data)
}

################################################################
#####################     DATA    ##############################
################################################################

##Import
sales_cat3 <- read.csv("table1.csv")
sales_16 <- read.csv("table2.csv")

sales_cat3$days <- as.integer(as.Date(sales_cat3[,2], format= "%Y-%m-%d"))
sales_16$days <- as.integer(as.Date(sales_16[,2], format= "%Y-%m-%d"))


#Vector of days
range_days<-seq(as.integer(as.Date("1996-07-05", format= "%Y-%m-%d")),as.integer(as.Date("1998-05-06", format= "%Y-%m-%d")),1)


#Create resum table
resum <-data.frame(range_days)
names(resum)<- c("days")

##put information in resum:

resum$quantity_16<-resumtable(resum$days,sales_16$days,sales_16$Quantity)


########---------------------------
##X1=Sales of last week:

resum$X1_SalesWeek <-sumquantities(resum$quantity_16,6)

########---------------------------
##X2=Sales of last month:

resum$X2_SalesMonth <- sumquantities(resum$quantity_16,29)

########---------------------------
##X3=Sales of last trimester:

resum$X3_SalesTrimes <- sumquantities(resum$quantity_16,90)

########---------------------------
##X4=Sales of last Semester:

resum$X4_SalesSemes <- sumquantities(resum$quantity_16,181)


