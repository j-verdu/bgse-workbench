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
    results<-data.frame(date,observed=test[,1],predict=prediction,error=prediction-test[,1])
    results<-results[order(results$date),]
    
    return(results)
}

## Read data and generate features for regression
## for top 'i' product, database 'db' connected trhough dbConnect, 
## days interval to forecast (sales in x days to come)
read_gen_data<-function(i,db,days_forecast){
    
    # sales of product of the same category
    query<-paste0("SELECT * from table1_top",as.character(i))
    sales_scat = dbGetQuery(db, query)
    
    # sales of product i
    query<-paste0("SELECT * from table2_top",as.character(i))
    sales_i = dbGetQuery(db, query)    
    
    # sales of other categories, adding all products of each category
    query<-paste0("SELECT * from table3_top",as.character(i))
    sales_cats = dbGetQuery(db, query)  
    
    sales_scat$days <- as.integer(as.Date(sales_scat[,1], format= "%Y-%m-%d"))
    sales_i$days <- as.integer(as.Date(sales_i[,1], format= "%Y-%m-%d"))
    sales_cats$days <- as.integer(as.Date(sales_cats[,1], format= "%Y-%m-%d"))
    
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

## Model selection from data
model_selection<-function (DATA) {
    
    # Split data into training (75%) and testing (25%)
    train_idx <- sample(1:nrow(DATA),round(nrow(DATA)*0.75),replace=FALSE)
    test <- DATA[-train_idx,] # test data
    train <- DATA[train_idx,] # train data
    
    #Building formula
    b<-paste(names(train)[-1],collapse=" + ")
    formula <- paste0(paste0(names(train)[1]," ~ "),b) 
    
    model_percentRMSE<-rep(0,4)
    names(model_percentRMSE)<-c("linear","GLM","GLM_Lasso","GLM_Ridge")
    
    ## Model simply linear
    lin <- lm( formula ,data=train )
    predicted<-predict(lin,test,type="response")
    results<-gen_results(predicted,test)
    RMSE<-sqrt(mean(results$error^2)) # Root Mean Square Error
    model_percentRMSE[1]<-RMSE/mean(test[,1])
    
    ### Model GLM Poisson
    logit <- glm( formula , poisson(link='log'),data=train )
    predicted<-predict(logit,test,type="response")
    results<-gen_results(predicted,test)
    RMSE<-sqrt(mean(results$error^2)) # Root Mean Square Error
    model_percentRMSE[2]<-RMSE/mean(test[,1])
    
    ### Model GLM Poisson Lasso
    lasso <- cv.glmnet(as.matrix(train[,-1]),train[,1], family="poisson",alpha=1 )
    predicted<-as.vector(predict(lasso,as.matrix(test[,-1]),s="lambda.min"))
    results<-gen_results(predicted,test)
    RMSE<-sqrt(mean(results$error^2)) # Root Mean Square Error
    model_percentRMSE[3]<-RMSE/mean(test[,1])
    
    ### Model GLM Poisson Ridge
    ridge <- cv.glmnet(as.matrix(train[,-1]),train[,1], family="poisson",alpha=0 )
    predicted<-as.vector(predict(ridge,as.matrix(test[,-1]),s="lambda.min"))
    results<-gen_results(predicted,test)
    RMSE<-sqrt(mean(results$error^2)) # Root Mean Square Error
    model_percentRMSE[4]<-RMSE/mean(test[,1])
    
    models<-list(lin,logit,lasso,ridge)
    
    ### Model selection
    
    selected<-which(model_percentRMSE==min(model_percentRMSE))
    model<-models[[selected]]
    
    return(list(model,model_percentRMSE[selected],selected))
    #returns the model, its percentRMSE in validation, and a number 
    #which identifies type of model (1,2,3,4)=(linear,GLM,GLMLasso,GLMRidge)
}

# Predict a single value with its 95% confidence interval if possible
do_predict<-function(model,tests,selected){
    
    preds<-predict(model,tests,type="response",interval="predict",se.fit=TRUE)

    ##Critical Values
    if (selected==2){ # GLM 
        critval <- 1.96 ## approx 95% CI
        upr <- preds$fit + (critval * preds$se.fit)
        lwr <- preds$fit - (critval * preds$se.fit)
        fit <- preds$fit
    } else if (selected==1){
      fit<-preds$fit[1]
      lwr<-preds$fit[2]
      upr<-preds$fit[3]
    } else {# GLM or Lasso
        fit<-preds
        upr<-NA
        lwr<-NA
    }
    return(c(fit,upr,lwr))
}

# Generates data for times series graph to check model behaviour
gen_graph_data<-function(DATA,model,selected){
    
    Graph<- data.frame(date=as.Date(DATA[,1],origin = "1970-01-01"),Observed=DATA[,3])
    if (selected %in% 3:4){
        DATA<-as.matrix(DATA)
        prediction<-predict(model,as.matrix(DATA[,-1]),s="lambda.min")
        Graph$Predicted<-prediction
        # In that case we don't include upr and lwr values
    } else if (selected<3){
        predictionsYs<-predict(model,DATA,type="response",interval="predict",se.fit=TRUE)
        Graph$Predicted<-predictionsYs$fit
        critval <- 1.96 ## approx 95% CI
        for (i in 1:nrow(Graph)){
            Graph$upr[i]<-predictionsYs$fit[i] + (critval * predictionsYs$se.fit[i])
            Graph$lwr[i]<-predictionsYs$fit[i] - (critval * predictionsYs$se.fit[i])
        }
    }
    
    return(Graph)
}



#########################
### MAIN ALGORITHM  #####
#########################

db = dbConnect(MySQL(), user='root', password='root', dbname='ecommerce', host='localhost')

# Analysis for every top-10 best-sellers product
top<-5
max_backwards<-180 #number of backward days needed to calculate features for a particular day
days_forecast<-30 # the output will be forecast sales for a product along next
                # 'days_forescast' days

top_prods = dbGetQuery(db, "SELECT * from TopResum")


# Create variable to include predicted sales for all top products
predict_sales<-matrix(0,top,5)
predict_sales<-as.data.frame(predict_sales)
names(predict_sales)<-c("Ranking","ProdId","Predicted","Max_Predict","Min_Predict")



for (i in top:1){
    
    data<-read_gen_data(i,db,days_forecast)

    DATA<- data[max_backwards:nrow(data)-days_forecast,3:ncol(data)]
    # subset feasible data to generate models (first days discarded, 
    # unable to generate proper X because need more backwards days)
    # last days discarded because can't calculate forecast sales 
    # in 'days_forecast' horizon
    
    # Try models and pick the one with lowest percentRMSE in validation
    res<-model_selection(DATA)
    model<-res[[1]]
    RMSE<-res[[2]]
    selected<-res[[3]] #1:linear, 2:GLM, 3:GLMLasso, 4:GLM Ridge

    # Predict next 'days_forecast' sales for last day
    tests <-data[dim(data)[1],3:9] #data for last day, to predict actual future sales
    if (selected %in% 3:4){tests<-as.matrix(tests[-1])} # format for Lasso and Ridge
    
    predict_sales[i,]<-c(i,top_prods[i,2],do_predict(model,tests,selected))
    
# table <- summary(model)$coefficients if we wanted coefficients

}

# export table, to be done in SQL
write.table(predict_sales, "predict_sales.txt", sep="\t")

dbWriteTable(conn = con, name = 'predict_sales', value = as.data.frame(predict_sales))


# Top 1 model is in memory once finished the loop
# Create past time series with predictions and observations for this top product

Graph_data<-gen_graph_data(DATA,model,selected)
dbWriteTable(conn = con, name = 'graph_data', value = as.data.frame(Graph_data))


write.table(Graph, "graphlines.txt", sep="\t")

## FROM HERE TO BE DONE STILL
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

