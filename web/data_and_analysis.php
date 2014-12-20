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
	echo "The graph shows the ranking of the 10 products with more sales in monetary value. From this historical records we will try to predict the next month sales in order to have enough stock for the predictions.";

	//VALUES Graph 1
	$query = "select TR.top AS Ranking, TP.ProductID AS ProductID, TP.ProductName As ProductName, TP.CategoryID AS CategoryID
from ecommerce.TopProducts TP, ecommerce.TopResum TR
where TP.ProductID=TR.ProductID
limit 10";
	query_and_print_table($query);
	echo "This table shows us the relation of the top-ten products with their ID and the categories they belong.";

	//GRAPH1
	$query = "select * from ecommerce.PredictionStock";
	$title = "Works?";
	query_and_print_graph($query,$title,"Quantity");
	echo "Can you see something?";

	
?>
		</div>
		<div id="analysis" style="display: none">
			<h2>Analysis</h2>
<?php
	$query = "SELECT x,p FROM ecommerce.analysis_prob WHERE coef='coef3'";
	$title = "Probability of Success";
	query_and_print_series($query,$title,"probability of success of coef 3");
	echo "Comment 1";

	$query = "SELECT * FROM ecommerce.ModelSumm";
	$title = "Summary of models per top10 product";
	query_and_print_table($query,$title);
	echo "This table shows the type of model used to predict one-month sales of every product, together with validation error in percercent.";
?>
		</div>
<?php
	// Close connection
	mysql_close($link);
?>
