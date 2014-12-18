# Estimates by monthly sales


library(ggplot2)


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


##### Variable Y, prevision next r days:

Yfunction <- function(X,r){
  r<- r-1
  t <-(length(X)-r)
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


resum$Y_salesnextweek<-Yfunction(resum$quantity_16,30) # sales in posterior 30 days 



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


DATA<- resum[183:nrow(resum)-30,3:ncol(resum)]

## Predict the next month sale

b<-paste(names(DATA)[-1],collapse=" + ")
formula <- paste0(paste0(names(DATA)[1]," ~ "),b) 

logit <- glm( formula , poisson(link='log'),data=DATA )

summary(logit)
tests <-resum[644,3:9]

preds<-predict(logit,tests,type="response",interval="predict",se.fit=TRUE)


##Critical Values
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit


###########################################################################
##################  Creatting data for the Graphs ########################
##########################################################################

predictionsYs<-predict(logit,DATA,type="response",interval="predict",se.fit=TRUE)

Graph<- as.data.frame(resum[183:nrow(resum)-30,1])
Graph$PredictionX<-predictionsYs$fit


critval <- 1.96 ## approx 95% CI
for (i in 1:nrow(Graph)){
  Graph$upr[i]<-predictionsYs$fit[i] + (critval * predictionsYs$se.fit[i])
  Graph$lwr[i]<-predictionsYs$fit[i] - (critval * predictionsYs$se.fit[i])
  
}

write.table(Graph, "graphlines.txt", sep="\t")
