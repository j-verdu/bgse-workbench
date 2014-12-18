use ecommerce;

##Finding the category with more sales (money) in the last 3 months:
select sum(D.UnitPrice*D.Quantity) As Money, P.CategoryID, C.CategoryName
from order_details D, products P, categories C, orders O
where D.ProductID=P.ProductID and P.CategoryID=C.CategoryID and D.OrderID=O.OrderID and date_format(O.OrderDate, '%Y-%m') > date_format(OrderDate, '1998-02')
group by P.CategoryID
order by Money desc;

##As category 3 is the one with most of the sales we are going to work with the products of this one:

##Products saled in this category the last 3 months:
select D.ProductID
from order_details D, products P, orders O
where D.ProductID=P.ProductID and P.CategoryID=3 
and D.OrderID=O.OrderID and date_format(O.OrderDate, '%Y-%m') > date_format(OrderDate, '1998-02')
group by D.ProductID;


## Table 1:  Sales of category 3 NOT product 16:
select sum(D.Quantity) As Quantity, O.OrderDate
from order_details D, products P, orders O
where D.ProductID=P.ProductID and P.CategoryID=3 and P.ProductID <> 16
and D.OrderID=O.OrderID
group by O.OrderDate;


##CHOOSE PRODUCT 16:

##Number of sales of product 16 per day

select sum(D.Quantity) As Quantity, O.OrderDate
from order_details D, products P, orders O
where D.ProductID=P.ProductID and D.ProductID=16
and D.OrderID=O.OrderID
group by O.OrderDate;


## Table 3:  Sales of all the categories BUT NOT cat 3 NOT product 16:
select sum(D.Quantity) As Quantity, O.OrderDate
from order_details D, products P, orders O
where D.ProductID=P.ProductID and P.CategoryID<>3 and P.ProductID <> 16
and D.OrderID=O.OrderID
group by O.OrderDate;



##Number of products with most of the sales
select count(D.Quantity) As Quantity, P.ProductID
from order_details D, products P, orders O
where D.ProductID=P.ProductID
and D.OrderID=O.OrderID
group by P.ProductID;

select OrderDate
from orders
order by OrderDate asc;