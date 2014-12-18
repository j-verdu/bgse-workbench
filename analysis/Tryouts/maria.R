library(ggplot2)

Sys.setenv(PKG_CPPFLAGS = "-I/usr/local/mysql/include/")
Sys.setenv(PKG_LIBS = "-L/usr/local/mysql/lib -lmysqlclient")
install.packages("RMySQL", type = "source")



################################################################
################      FUNCTIONS    #############################
################################################################

#Creating sum of standarized:
# r=7 week, r=30 month, r=90 trimestre
sumquantities<-function(X,r){
  data<-rep(0,length(X))
  for(i in (length(X):r)){ 
    k<- i-r
    s<-i-1
    data[i]<-sum(X[k:s])
  }
  mean<-mean(data)
  sd<-sd(data)
  da <- sapply(data,function(x) (x-mean)/sd)
  return(da)
}

##Putting info in Resum table
##X:days in resum, YD:days in product, YD:quantity of product
resumtable<- function(X,YD,YQ){
  data <- rep(0,length(X))
  k<-1
  for(i in 1:length(X)){
    if(X[i]==YD[k]){
      data[i]<-YQ[k]
      k<-k+1
    }
  }
  return(data)
}

##Other quantities

quantitiescate<-function(X,YD,YQ){
  data<-resumtable(X,YD,YQ)
  mean<-mean(data)
  sd<-sd(data)
  da <- sapply(data,function(x) (x-mean)/sd)
  return(da)
}


##### Variable Y, prevision next week:

Yfunction <- function(X){
  r<- 6
  t <-(length(X)-6)
  data<-rep(0,length(X))
  for(i in (1:t)){ 
    k <- i+r
    data[i]<-sum(X[i:k])
  }
  return(data)
}



################################################################
#####################     DATA    ##############################
################################################################

##Import
sales_cat3 <- read.csv("table1.csv")
sales_16 <- read.csv("table2.csv")
sales_cats <- read.csv("table3.csv")

sales_cat3$days <- as.integer(as.Date(sales_cat3[,2], format= "%Y-%m-%d"))
sales_16$days <- as.integer(as.Date(sales_16[,2], format= "%Y-%m-%d"))
sales_cats$days <- as.integer(as.Date(sales_cats[,2], format= "%Y-%m-%d"))

#Create resum table
resum <-data.frame(seq(as.integer(as.Date("1996-07-04", format= "%Y-%m-%d")),as.integer(as.Date("1998-05-06", format= "%Y-%m-%d")),1))
names(resum)<- c("days")

##put information in resum:

resum$quantity_16<-resumtable(resum$days,sales_16$days,sales_16$Quantity)

########---------------------------
##Y - Creation on the dependent variable:


resum$Y_salesnextweek<-Yfunction(resum$quantity_16)



########---------------------------
##X1=Sales of last week:

resum$X1_SalesWeek <-sumquantities(resum$quantity_16,7)


########---------------------------
##X2=Sales of last month:

resum$X2_SalesMonth <- sumquantities(resum$quantity_16,30)

########---------------------------
##X3=Sales of last trimester:

resum$X3_SalesTrimes <- sumquantities(resum$quantity_16,90)

########---------------------------
##X4=Sales of last Semester:

resum$X4_SalesSemes <- sumquantities(resum$quantity_16,182)

########---------------------------
##X5=Sales of the day of other products of categ.3:

resum$X5quantity_cat3<-quantitiescate(resum$days,sales_cat3$days,sales_cat3$Quantity)

########---------------------------
##X5=Sales of the day of other products of categ.3:

resum$X6quantity_cat<-quantitiescate(resum$days,sales_cats$days,sales_cats$Quantity)

###########################################################################
#  Discard first 6 months, since we can't generate all X properly #########
##########################################################################


DATA<- resum[183:nrow(resum)-7,3:ncol(resum)]
<<<<<<< Updated upstream

# Split data into training (75%) and testing (25%)
train_idx <- sample(1:nrow(DATA),round(nrow(DATA)*0.75),replace=FALSE)
test <- DATA[-train_idx,] # test data
DATA <- DATA[train_idx,] # train data


=======
>>>>>>> Stashed changes
y <- DATA[,1]
X<-as.matrix(DATA[,2:7])

logit <- glm( y ~ X , poisson(link='log') )

summary( logit )

logit

<<<<<<< Updated upstream
# Validate with test data

b<-paste(names(DATA)[-1],collapse=" + ")
formula <- paste0(paste0(names(DATA)[1]," ~ "),b) 

logit <- glm( formula , poisson(link='log'),data=DATA )


predicted<-predict(logit,test,type="response")
# order by date
date<-as.integer(rownames(test))
results<-data.frame(date,observed=test[,1],predict=predicted,error=predicted-test[,1])
results<-results[order(results$date),]
plot(results$date,results$error)

weeks0<-results[results$observed==0,]
plot(weeks0$date,weeks0$error)

weeksGt0<-results[results$observed!=0,]
plot(weeksGt0$date,weeksGt0$error)


RMSE<-sqrt(mean(results$error^2))
=======
>>>>>>> Stashed changes

