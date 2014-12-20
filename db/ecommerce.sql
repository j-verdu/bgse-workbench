
DROP DATABASE IF EXISTS ecommerce;
CREATE database ecommerce;


USE ecommerce;


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;



--
-- Table structure for table `categories`
--

DROP TABLE IF EXISTS `categories`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `categories` (
  `CategoryID` int(11) NOT NULL AUTO_INCREMENT,
  `CategoryName` varchar(15) DEFAULT NULL,
  `Description` text,
  `Picture` varchar(40) DEFAULT NULL,
  PRIMARY KEY (`CategoryID`)
) ENGINE=MyISAM AUTO_INCREMENT=9 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;


--
-- Table structure for table `customers`
--

DROP TABLE IF EXISTS `customers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `customers` (
  `CustomerID` varchar(5) NOT NULL DEFAULT '',
  `CompanyName` varchar(40) DEFAULT NULL,
  `ContactName` varchar(30) DEFAULT NULL,
  `ContactTitle` varchar(30) DEFAULT NULL,
  `Address` varchar(60) DEFAULT NULL,
  `City` varchar(15) DEFAULT NULL,
  `Region` varchar(15) DEFAULT NULL,
  `PostalCode` varchar(10) DEFAULT NULL,
  `Country` varchar(15) DEFAULT NULL,
  `Phone` varchar(24) DEFAULT NULL,
  `Fax` varchar(24) DEFAULT NULL,
  PRIMARY KEY (`CustomerID`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;


--
-- Table structure for table `employees`
--

DROP TABLE IF EXISTS `employees`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `employees` (
  `EmployeeID` int(11) NOT NULL AUTO_INCREMENT,
  `LastName` varchar(20) DEFAULT NULL,
  `FirstName` varchar(10) DEFAULT NULL,
  `Title` varchar(30) DEFAULT NULL,
  `TitleOfCourtesy` varchar(25) DEFAULT NULL,
  `BirthDate` date DEFAULT NULL,
  `HireDate` date DEFAULT NULL,
  `Address` varchar(60) DEFAULT NULL,
  `City` varchar(15) DEFAULT NULL,
  `Region` varchar(15) DEFAULT NULL,
  `PostalCode` varchar(10) DEFAULT NULL,
  `Country` varchar(15) DEFAULT NULL,
  `HomePhone` varchar(24) DEFAULT NULL,
  `Extension` varchar(4) DEFAULT NULL,
  `Photo` varchar(40) DEFAULT NULL,
  `Notes` text,
  `ReportsTo` int(11) DEFAULT NULL,
  PRIMARY KEY (`EmployeeID`)
) ENGINE=MyISAM AUTO_INCREMENT=10 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `mailing_list`
--

DROP TABLE IF EXISTS `mailing_list`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mailing_list` (
  `email` varchar(100) NOT NULL,
  `name` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `order_details`
--

DROP TABLE IF EXISTS `order_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `order_details` (
  `odID` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `OrderID` int(11) DEFAULT '0',
  `ProductID` int(11) DEFAULT '0',
  `UnitPrice` float(8,2) DEFAULT '0.00',
  `Quantity` smallint(6) DEFAULT '1',
  `Discount` float(8,2) DEFAULT '0.00',
  PRIMARY KEY (`odID`),
  KEY `product_id` (`ProductID`)
) ENGINE=MyISAM AUTO_INCREMENT=2156 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `orders`
--

DROP TABLE IF EXISTS `orders`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `orders` (
  `OrderID` int(11) NOT NULL AUTO_INCREMENT,
  `CustomerID` varchar(5) DEFAULT NULL,
  `EmployeeID` int(11) DEFAULT NULL,
  `OrderDate` date DEFAULT NULL,
  `RequiredDate` date DEFAULT NULL,
  `ShippedDate` date DEFAULT NULL,
  `ShipVia` int(11) DEFAULT NULL,
  `Freight` float(1,0) DEFAULT '0',
  `ShipName` varchar(40) DEFAULT NULL,
  `ShipAddress` varchar(60) DEFAULT NULL,
  `ShipCity` varchar(15) DEFAULT NULL,
  `ShipRegion` varchar(15) DEFAULT NULL,
  `ShipPostalCode` varchar(10) DEFAULT NULL,
  `ShipCountry` varchar(15) DEFAULT NULL,
  PRIMARY KEY (`OrderID`)
) ENGINE=MyISAM AUTO_INCREMENT=11078 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `products`
--

DROP TABLE IF EXISTS `products`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `products` (
  `ProductID` int(11) NOT NULL AUTO_INCREMENT,
  `ProductName` varchar(40) DEFAULT NULL,
  `SupplierID` int(11) DEFAULT NULL,
  `CategoryID` int(11) DEFAULT NULL,
  `QuantityPerUnit` varchar(20) DEFAULT NULL,
  `UnitPrice` float(8,2) DEFAULT '0.00',
  `UnitsInStock` smallint(6) DEFAULT '0',
  `UnitsOnOrder` smallint(6) DEFAULT '0',
  `ReorderLevel` smallint(6) DEFAULT '0',
  `Discontinued` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`ProductID`)
) ENGINE=MyISAM AUTO_INCREMENT=78 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `shippers`
--

DROP TABLE IF EXISTS `shippers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shippers` (
  `ShipperID` int(11) NOT NULL AUTO_INCREMENT,
  `CompanyName` varchar(40) DEFAULT NULL,
  `Phone` varchar(24) DEFAULT NULL,
  PRIMARY KEY (`ShipperID`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `suppliers`
--

DROP TABLE IF EXISTS `suppliers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `suppliers` (
  `SupplierID` int(11) NOT NULL AUTO_INCREMENT,
  `CompanyName` varchar(40) DEFAULT NULL,
  `ContactName` varchar(30) DEFAULT NULL,
  `ContactTitle` varchar(30) DEFAULT NULL,
  `Address` varchar(60) DEFAULT NULL,
  `City` varchar(15) DEFAULT NULL,
  `Region` varchar(15) DEFAULT NULL,
  `PostalCode` varchar(10) DEFAULT NULL,
  `Country` varchar(15) DEFAULT NULL,
  `Phone` varchar(24) DEFAULT NULL,
  `Fax` varchar(24) DEFAULT NULL,
  `HomePage` text,
  PRIMARY KEY (`SupplierID`)
) ENGINE=MyISAM AUTO_INCREMENT=30 DEFAULT CHARSET=latin1;

/*!40101 SET character_set_client = @saved_cs_client */;

/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;


DROP TABLE IF EXISTS `analysis_data_table`;

CREATE TABLE `analysis_data_table` (
  `dates`  date,
  `y`   float(5) NOT NULL,
  `x01` float(5) DEFAULT NULL,
  `x02` float(5) DEFAULT NULL,
  `x03` float(5) DEFAULT NULL,
  `x04` float(5) DEFAULT NULL,
  `x05` float(5) DEFAULT NULL,
  `x06` float(5) DEFAULT NULL,
  `x07` float(5) DEFAULT NULL,
  `x08` float(5) DEFAULT NULL,
  `x09` float(5) DEFAULT NULL,
  `x10` float(5) DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE `analysis_estimates` (
  `coef`    varchar(40) DEFAULT NULL,
  `estimate`    float(5) DEFAULT NULL,
  `stderr`      float(5) DEFAULT NULL,
  `zval`        float(5) DEFAULT NULL,
  `pval`        float(5) DEFAULT NULL,
  PRIMARY KEY (`coef`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE `analysis_prob` (
  `coef`    varchar(40) DEFAULT NULL,
  `x`       float(5) DEFAULT NULL,
  `p`       float(5) DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

/*CREATE TABLE `analsys_analytics_estimates` ()*/


## TOP Products###
##################



drop table if exists table2_top1;

create table table2_top1 (
	OrderDate  date,
    Quantity int(225)
);



##2nd product
drop table if exists table2_top2;

create table table2_top2 (
	OrderDate  date,
    Quantity int(225)
);



##3rd product
drop table if exists table2_top3;

create table table2_top3 (
	OrderDate  date,
    Quantity int(225)
);


##4rd product
drop table if exists table2_top4;

create table table2_top4 (
	OrderDate  date,
    Quantity int(225)
);

##5rd product
drop table if exists table2_top5;

create table table2_top5 (
	OrderDate  date,
    Quantity int(225)
);


##6st
drop table if exists table2_top6;

create table table2_top6 (
	OrderDate  date,
    Quantity int(225)
);

##7st
drop table if exists table2_top7;

create table table2_top7 (
	OrderDate  date,
    Quantity int(225)
);

##8st
drop table if exists table2_top8;

create table table2_top8 (
	OrderDate  date,
    Quantity int(225)
);


##9st
drop table if exists table2_top9;

create table table2_top9 (
	OrderDate  date,
    Quantity int(225)
);

##10st
drop table if exists table2_top10;

create table table2_top10 (
	OrderDate  date,
    Quantity int(225)
);


######################
####### TABLE 1 ######

##1st
drop table if exists table1_top1;

create table table1_top1 (
	OrderDate  date,
    Quantity int(225)
);


##2st
drop table if exists table1_top2;

create table table1_top2 (
	OrderDate  date,
    Quantity int(225)
);

##3st
drop table if exists table1_top3;

create table table1_top3 (
	OrderDate  date,
    Quantity int(225)
);

##4st
drop table if exists table1_top4;

create table table1_top4 (
	OrderDate  date,
    Quantity int(225)
);


##5st
drop table if exists table1_top5;

create table table1_top5 (
	OrderDate  date,
    Quantity int(225)
);

##6st
drop table if exists table1_top6;

create table table1_top6 (
	OrderDate  date,
    Quantity int(225)
);

##7st
drop table if exists table1_top7;

create table table1_top7 (
	OrderDate  date,
    Quantity int(225)
);

##8st
drop table if exists table1_top8;

create table table1_top8 (
	OrderDate  date,
    Quantity int(225)
);


##9st
drop table if exists table1_top9;

create table table1_top9 (
	OrderDate  date,
    Quantity int(225)
);

##10st
drop table if exists table1_top10;

create table table1_top10 (
	OrderDate  date,
    Quantity int(225)
);


######################
####### TABLE 3 ######

##1st
drop table if exists table3_top1;

create table table3_top1 (
	OrderDate  date,
    Quantity int(225)
);

##2st
drop table if exists table3_top2;

create table table3_top2 (
	OrderDate  date,
    Quantity int(225)
);

##3st
drop table if exists table3_top3;

create table table3_top3 (
	OrderDate  date,
    Quantity int(225)
);

##4st
drop table if exists table3_top4;

create table table3_top4 (
	OrderDate  date,
    Quantity int(225)
);


##5st
drop table if exists table3_top5;

create table table3_top5 (
	OrderDate  date,
    Quantity int(225)
);

##6st
drop table if exists table3_top6;

create table table3_top6 (
	OrderDate  date,
    Quantity int(225)
);

##7st
drop table if exists table3_top7;

create table table3_top7 (
	OrderDate  date,
    Quantity int(225)
);

##8st
drop table if exists table3_top8;

create table table3_top8 (
	OrderDate  date,
    Quantity int(225)
);



##9st
drop table if exists table3_top9;

create table table3_top9 (
	OrderDate  date,
    Quantity int(225)
);

##10st
drop table if exists table3_top10;

create table table3_top10 (
	OrderDate  date,
    Quantity int(225)
);


drop table if exists TopResum;

create table TopResum(
	`top` int(11),
  `CategoryName` varchar(15),
  `ProductID` int (11)
);



drop table if exists PredictionStock;

create table PredictionStock(
  `Ranking` int(11),
  `ProductID` int (11),
  `Predicted` float(5),
  `Max_Predict` float(5),
  `Min_Predict` float(5)
);



drop table if exists GraphPredictions;

create table GraphPredictions(
  `Date` DATE,
  `Observed` int (11),
  `PredictionX` float(5),
  `upr` float(5),
  `lwr` float(5)
);

drop table if exists TopProducts;

create table TopProducts(
	`Quantity` int(11),
  `ProductID` int (11),
  `CategoryID` float(5)
);