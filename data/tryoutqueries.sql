use ecommerce;

##Finding the category with more sales (money)
select sum(D.UnitPrice*D.Quantity), P.CategoryID, C.CategoryName
from order_details D, products P, categories C
where D.ProductID=P.ProductID and P.CategoryID=C.CategoryID
group by P.CategoryID;

