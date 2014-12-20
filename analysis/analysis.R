### Computing Lab $ Data Warehouse Project ######################
### Silvia Ariza, Stephen Carmody, Maria Fernandez, Joan Verdu ##
#################################################################

library(RMySQL)
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
        if (k==length(YD)+1){break}
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
    resum$quantity_scat<-resumtable(resum$days,sales_scat$days,sales_scat$Quantity)
    resum$quantity_cats<-resumtable(resum$days,sales_cats$days,sales_cats$Quantity)
    
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
    
    ##X6=Sales of last week of other products of same category:
    resum$X6_SalesWeek_SameCat <-sumquantities(resum$quantity_scat,7)
    
    ##X7=Sales of last month of other products of same category:
    resum$X7_SalesMonth_SameCat <- sumquantities(resum$quantity_scat,30)
    
    ##X8=Sales of last trimester of other products of same category:
    resum$X8_SalesTrimes_SameCat <- sumquantities(resum$quantity_scat,90)
    
    ##X9=Sales of last semester of other products of same category:
    resum$X9_SalesSemes_SameCat <- sumquantities(resum$quantity_scat,182)
    
    ##X10=Sales of the day of other products of other categories:
    resum$X10quantity_cat<-quantitiescate(resum$days,sales_cats$days,sales_cats$Quantity)
    
    ##X11=Sales of last week of other products of other categories:
    resum$X11_SalesWeek_OthCat <-sumquantities(resum$quantity_cats,7)
    
    ##X12=Sales of last month of other products of other categories:
    resum$X12_SalesMonth_OthCat <- sumquantities(resum$quantity_cats,30)
    
    ##X13=Sales of last trimester of other products of other categories:
    resum$X13_SalesTrimes_OthCat <- sumquantities(resum$quantity_cats,90)
    
    ##X14=Sales of last semester of other products of same category:
    resum$X14_SalesSemes_SameCat <- sumquantities(resum$quantity_cats,182)
    
    resum<-resum[,-3] #drop quantity_scat
    resum<-resum[,-3] #drop quantity_cats
    
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
    
    ##Predict with confidence interval
    if (selected==2){ #GLM
        preds<-predict(model,tests,type = "link", se.fit=TRUE)
        critval <- qnorm(0.975) ##  95% Confidence Interval
        upr <- preds$fit + (critval * preds$se.fit)
        lwr <- preds$fit - (critval * preds$se.fit)
        fit <- preds$fit
        fit <- model$family$linkinv(fit)
        upr <- model$family$linkinv(upr)
        lwr <- model$family$linkinv(lwr)
        
    } else if (selected==1) { # simple linear regression
        preds<-predict(model, tests, interval="confidence")
        fit <- preds[1]
        lwr<-preds[2]
        upr<-preds[3]
    } else { # GLM Lasso or GLM Ridge
        preds<-predict(model,tests,type="response",se.fit=TRUE)
        fit<-preds
        upr<-NA # confidence interval do not apply to Lasso or Ridge, already optimized lambda
        lwr<-NA
    } 
    
    return(c(max(0,fit),max(0,upr),max(0,lwr)))
}

# Generates data for times series graph to check model behaviour
gen_graph_data<-function(dates,DATA,model,selected){
    
    Graph<- data.frame(date=as.Date(dates,origin = "1970-01-01"),Observed=DATA[,1])
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
top<-10
max_backwards<-180 #number of backward days needed to calculate features for a particular day
days_forecast<-30 # the output will be forecast sales for a product along next
                # 'days_forescast' days

top_prods = dbGetQuery(db, "SELECT * from TopResum")
top_prods[,3]<-as.character(top_prods[,3])

# Create variable to include predicted sales for all top products
predict_sales<-matrix(0,top,5)
predict_sales<-as.data.frame(predict_sales)
names(predict_sales)<-c("Ranking","ProdId","Predicted","Max_Predict","Min_Predict")

# Create variable to summarize model
model_summ<-matrix(0,top,5)
model_summ<-as.data.frame(model_summ)
names(model_summ)<-c("Ranking","ProdId","Prod.Name","Model","Validation RMSE (%)")


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
    tests <-data[dim(data)[1],3:ncol(data)] #data for last day, to predict actual future sales
    if (selected %in% 3:4){tests<-as.matrix(tests[-1])} # format for Lasso and Ridge
    
    predict_sales[i,]<-c(i,top_prods[i,3],do_predict(model,tests,selected))
    
    # Save info about model
    type<-c("Linear Regression","Poisson GLM","Poisson GLM Lasso","Poisson GLM Ridge")
    model_summ[i,]<-c(i,top_prods[i,3],top_prods[i,4],type[selected],round(RMSE*100,1))
    
}

# Send to db predictions for next month in top 10
predict_sales[,1]<-as.numeric(predict_sales[,1])
predict_sales[,2]<-as.numeric(predict_sales[,2])
predict_sales[,3]<-as.numeric(predict_sales[,3])
predict_sales[,4]<-as.numeric(predict_sales[,4])
predict_sales[,5]<-as.numeric(predict_sales[,5])
prediction<-predict_sales
for( i in 1:length(prediction) ){
  cat('.')  
  query <- sprintf('INSERT INTO PredictionStock VALUES (%f,%f,%f,%f,%f);',prediction[i,1],prediction[i,2],prediction[i,3],prediction[i,4],prediction[i,5])
  query = dbSendQuery(db, query )
}
write.table(prediction, "predictsales.txt", sep="\t") # check

# Save summary of selected models in a table
model_summ[,1]<-as.numeric(model_summ[,1])
model_summ[,2]<-as.numeric(model_summ[,2])
model_summ[,5]<-as.numeric(model_summ[,5])
for( i in 1:length(model_summ) ){
    cat('.')  
    query <- sprintf('INSERT INTO ModelSumm VALUES (\'%s\',\'%s\',\'%s\',\'%s\');',model_summ[i,1],model_summ[i,2],model_summ[i,3],model_summ[i,4])
    query = dbSendQuery(db, query )
}


# Top 1 model is in memory once finished the loop
# Create past time series with predictions and observations for this top product
dates<-data[max_backwards:nrow(data)-days_forecast,1]
Graph_data<-gen_graph_data(dates,DATA,model,selected)
for( i in 1:length(Graph_data) ){
  cat('.')  
  query <- sprintf('INSERT INTO GraphPredictions VALUES (%f,%f,%f,%f,%f);',Graph_data[i,1],Graph_data[i,2],Graph_data[i,3],Graph_data[i,4],Graph_data[i,5])
  query = dbSendQuery(db, query )
}

dbDisconnect(db)
