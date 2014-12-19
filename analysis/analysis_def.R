### Computing Lab $ Data Warehouse Project ######################
### Silvia Ariza, Stephen Carmody, Maria Fernandez, Joan Verdu ##
#################################################################

library(RMySQL)
library(ggplot2)
library(glmnet)

#########################
##### Functions #########
#########################

##Creating sum of standarized data, standarized sales (X) in last 'r' days:
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

##Standarize quantities
quantitiescate<-function(X,YD,YQ){
    data<-resumtable(X,YD,YQ)
    mean<-mean(data)
    sd<-sd(data)
    da <- sapply(data,function(x) (x-mean)/sd)
    return(da)
}


## Variable Y, sales next 'r' days:
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

## Generate results data frame from predicted test data
gen_results<- function(prediction,test){
    
    # generate results ordered by date
    date<-as.integer(rownames(test))
    results<-data.frame(date,observed=test[,1],predict=prediction,error=predicted-test[,1])
    results<-results[order(results$date),]
    
    return(results)
}

## Read data and generate features for regression
## for top 'i' product, database 'db' connected trhough dbConnect, 
## days interval to forecast (sales in x days to come)
read_gen_data<-function(i,db,days_forecast){
    
    # sales of product of the same category
    query<-paste0("SELECT * from table1_top",as.character(i))
    result = dbSendQuery(db, query)
    sales_scat = fetch(result, n=-1)
    
    # sales of product i
    query<-paste0("SELECT * from table2_top",as.character(i))
    result = dbSendQuery(db, query)
    sales_i = fetch(result, n=-1)    
    
    # sales of other categories, adding all products of each category
    query<-paste0("SELECT * from table3_top",as.character(i))
    result = dbSendQuery(db, query)
    sales_cats = fetch(result, n=-1)  
    
    sales_scat$days <- as.integer(as.Date(sales_scat[,2], format= "%Y-%m-%d"))
    sales_i$days <- as.integer(as.Date(sales_i[,2], format= "%Y-%m-%d"))
    sales_cats$days <- as.integer(as.Date(sales_cats[,2], format= "%Y-%m-%d"))
    
    #Create resum table
    resum <-data.frame(seq(as.integer(as.Date("1996-07-04", format= "%Y-%m-%d")),as.integer(as.Date("1998-05-06", format= "%Y-%m-%d")),1))
    names(resum)<- c("days")
    
    ##put information in resum:
    resum$quantity_i<-resumtable(resum$days,sales_i$days,sales_i$Quantity)
    
    ##Y - Creation on the dependent variable:
    resum$Y_salesNxtMonth<-Yfunction(resum$quantity_i,days_forecast) # sales in posterior xx days 
    
    ##X1=Sales of last week:
    resum$X1_SalesWeek <-sumquantities(resum$quantity_i,7)
    
    ##X2=Sales of last month:
    resum$X2_SalesMonth <- sumquantities(resum$quantity_i,30)
    
    ##X3=Sales of last trimester:
    resum$X3_SalesTrimes <- sumquantities(resum$quantity_i,90)
    
    ##X4=Sales of last Semester:
    resum$X4_SalesSemes <- sumquantities(resum$quantity_i,182)
    
    ##X5=Sales of the day of other products of same category:
    resum$X5quantity_scat<-quantitiescate(resum$days,sales_scat$days,sales_scat$Quantity)
    
    ##X6=Sales of the day of other products of other categories:
    resum$X6quantity_cat<-quantitiescate(resum$days,sales_cats$days,sales_cats$Quantity)
    
    return(resum)
}



#########################
### MAIN ALGORITHM  #####
#########################

db = dbConnect(MySQL(), user='root', password='root', dbname='ecommerce', host='localhost')

# Analysis for every top-10 best-sellers product
max_backwards<-180 #number of backward days needed to calculate features for a particular day
days_forecast<-30 # the output will be forecast sales for a product along next
                # 'days_forescast' days

i<-1 # to eliminate once set the loop
#for (i in 1:10){
    
    read_gen_data(i,db,days_forecast)


    y <- data[,1]
    X <- as.matrix(data[,2:11])



logit <- glm( y ~ X , binomial(link='logit') )

summary( logit )

table <- summary(logit)$coefficients



for( i in 1:10 ){
  query <- sprintf('INSERT INTO analysis_estimates VALUES (\'coef%s\',%f,%f,%f,%f);',i,table[i,1],table[i,2],table[i,3],table[i,4])
  query = dbSendQuery(db, query )
}

#
inv.link <- function(eta){ exp(eta)/(1+exp(eta))}
beta     <- coef(logit)
c=3
x <- seq(min(X[,c]),max(X[,c]),0.01)
p <- rep(0,length(x))
for( i in 1:length(x) ){
    cat('.')
    eta  <- beta[1] + x[i]*beta[1+c] + colMeans(X[,setdiff(1:10,c)]) %*% beta[ 1+setdiff(1:10,c) ]
    p[i] <- inv.link(eta)
 
    query <- sprintf('INSERT INTO analysis_prob VALUES (\'coef%s\',%f,%f);',c,x[i],p[i])
    query = dbSendQuery(db, query )
}

