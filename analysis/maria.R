
library(RMySQL)

db = dbConnect(MySQL(), user='root', password='root', dbname='ecommerce', host='localhost')

result = dbSendQuery(db, "select sum(D.UnitPrice*D.Quantity), P.CategoryID, C.CategoryName
from order_details D, products P, categories C
where D.ProductID=P.ProductID and P.CategoryID=C.CategoryID
                     group by P.CategoryID;")

data = fetch(result, n=-1)

y <- data[,1]
X <- as.matrix(data[,2:11])
