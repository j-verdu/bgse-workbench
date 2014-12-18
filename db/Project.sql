# Project: Joan Verdú, María Fernández, Stephan Carmody, Sílvia Ariza 

drop database if exists ecommerce;
create database if not exists ecommerce; 
use ecommerce;

create table Customers (
CustomerID nchar(5),
CompanyName nvarchar(40),
ContactName nvarchar(30),
ContactTitle nvarchar(30),
Address nvarchar(60),
City nvarchar(15),
Region nvarchar(15),
PostalCode nvarchar(10),
Country nvarchar(15),
Phone nvarchar(24),
Fax nvarchar(24),
primary key (CustomerID)
);

create table Shippers (
ShipperID int(11),
CompanyName nvarchar(40),
Phone nvarchar(24),
primary key (ShipperID)
);

create table Employees (
EmployeeID int(11),
LastName nvarchar(20),
FirstName nvarchar(20),
Title nvarchar(30),
TitleOfCourtesy nvarchar(25),
BirthDate datetime,
HireDate datetime,
Address nvarchar(60),
City nvarchar(15),
Region nvarchar(15),
PostalCode nvarchar(10),
Country nvarchar(15),
HomePhone nvarchar(24),
Extension nvarchar(4),
Photo nvarchar(40),
Notes text,
ReportsTo int,
primary key (EmployeeID)
);


create table Orders (
OrderID int(11),
CustomerID nchar(5),
EmployeeID int(11),
OrderDate datetime,
RequiredDate datetime,
ShippedDate datetime,
ShipVia int(11),
Freight decimal,
ShipName nvarchar(40),
ShipAddress nvarchar(60),
ShipCity nvarchar(15),
ShipRegion nvarchar(15),
ShipPostalCode nvarchar(10),
ShipCountry nvarchar(15),
primary key (OrderID),
foreign key (CustomerID) references Customers(CustomerID),
foreign key (ShipVia) references Shippers(ShipperID),
foreign key (EmployeeID) references Employees(EmployeeID)
);

create table Categories (
CategoryID int(11),
CategoryName nvarchar(15),
Description text,
Picture nvarchar(40),
primary key (CategoryID)
); 

create table Suppliers (
SupplierID int(11),
CompanyName nvarchar(40),
ContactName nvarchar(30),
ContactTitle nvarchar(30),
Address nvarchar(60),
City nvarchar(15),
Region nvarchar(15),
PostalCode nvarchar(10),
Country nvarchar(15),
Phone nvarchar(24),
Fax nvarchar(24),
HomePage text,
primary key (SupplierID)
);

create table Products (
ProductID int(11),
ProductName nvarchar(40),
SupplierID int(11),
CategoryID int(11),
QuantityPerUnit nvarchar(20),
UnitPrice decimal,
UnitsInStock smallint,
UnitsOnOrder smallint,
ReorderLevel smallint,
Discontinued tinyint, 
primary key (ProductID),
foreign key (SupplierID) references Suppliers(SupplierID),
foreign key (CategoryID) references Categories(CategoryID)
); 

create table Order_Details (
odID int(10),
OrderID int(11),
ProductID int(11),
UnitPrice decimal,
Quantity smallint,
Discount real,
primary key (odID),
foreign key (ProductID) references Products(ProductID), 
foreign key (OrderID) references Orders (OrderID)
);

use ecommerce;


# Which is the category with more sales? 
select sum(D.UnitPrice*D.Quantity), P.CategoryID, C.CategoryName, O.OrderDate
from Order_details D, Products P, Categories C, Orders O
where D.ProductID=P.ProductID and P.CategoryID=C.CategoryID and D.OrderID=O.OrderID
group by P.CategoryID, O.OrderDate
order by O.OrderDate desc;
select OrderDate
from Orders
order by OrderDate asc;

select *
from Order_details;
select *
from Orders;
select count(*)
from Products
where CategoryID=3;

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
## Table 1: Sales of category 3 NOT product 16:
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
## Table 3: Sales of all the categories BUT NOT cat 3 NOT product 16:
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


select sum(od.UnitPrice*od.Quantity) as MoneyValue, od.ProductId as productId
from order_details od inner join orders o on od.OrderId = o.OrderId 
where o.OrderDate >= DATE_SUB(STR_TO_DATE('01-03-1996', '%d-%m-%Y'), INTERVAL 3 MONTH)
group by od.ProductId
order by MoneyValue desc
limit 1