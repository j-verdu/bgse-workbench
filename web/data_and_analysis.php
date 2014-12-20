<?php

	include 'functions.php';
	$GLOBALS['graphid'] = 0;

	// Load libraries
	document_header();

	// Create connection
	$link = connect_to_db();
?>
		<div id="data" style="display: none">
		<h2>Data</h2>
<?php
	// Page body. Write here your queries
	//GRAPH1
	$query = "select ProductName , Quantity from ecommerce.TopProducts order by Quantity desc limit 10";
	$title = "Top products";
	query_and_print_graph($query,$title,"Quantity");
	echo "The graph shows the ranking of the 10 products with more sales in monatary value";

	//VALUES Graph 1
	$query = "select TR.top AS Ranking, TP.ProductID AS ProductID, TP.ProductName As ProductName, TP.CategoryID AS CategoryID
from ecommerce.TopProducts TP, ecommerce.TopResum TR
where TP.ProductID=TR.ProductID
limit 10";
	query_and_print_table($query);
	echo "Returns the 10 products that have made most sales in the ";

	$query = "SELECT * FROM ecommerce.products LIMIT 10";
	query_and_print_table($query);
	echo "Comment 2";

	// query_and_print_graph: Needs two columns: the first one with labels, the second one with values of the graph


	$query = "SELECT ProductName, UnitPrice FROM ecommerce.products ORDER BY UnitPrice ASC LIMIT 10";
	$title = "Top cheap products";
	query_and_print_graph($query,$title,"Euros");
	echo "Comment 4";
?>
		</div>
		<div id="analysis" style="display: none">
			<h2>Analysis</h2>
<?php
	$query = "SELECT x,p FROM ecommerce.analysis_prob WHERE coef='coef3'";
	$title = "Probability of Success";
	query_and_print_series($query,$title,"probability of success of coef 3");
	echo "Comment 1";

	$query = "SELECT * FROM ecommerce.analysis_estimates";
	$title = "Parameter Estimates";
	query_and_print_table($query,$title);
	echo "Comment 2";
?>
		</div>
<?php
	// Close connection
	mysql_close($link);
?>
