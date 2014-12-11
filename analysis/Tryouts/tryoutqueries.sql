use ecommerce;

##Data1: Finding the category with more sales (money)
select sum(D.UnitPrice*D.Quantity), P.CategoryID, C.CategoryName, O.OrderDate
from order_details D, products P, categories C, orders O
where D.ProductID=P.ProductID and P.CategoryID=C.CategoryID and D.OrderID=O.OrderID
group by P.CategoryID, O.OrderDate
order by O.OrderDate desc;

select OrderDate
from orders
order by OrderDate asc;


##

select *
from order_details;

select * 
from orders;

select count(*)
from products
where CategoryID=3;
