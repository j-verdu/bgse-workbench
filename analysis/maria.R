
library(RMySQL)

db = dbConnect(MySQL(), user='root', password='root', dbname='ecommerce', host='localhost')

result = dbSendQuery(db, "SELECT * from customers")

data = fetch(result, n=-1)

y <- data[,1]
X <- as.matrix(data[,2:11])