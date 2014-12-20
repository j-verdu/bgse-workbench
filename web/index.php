<?php ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html>
<head>
	<title>One-month forecast stock optimization</title>    
	<link rel="stylesheet" type="text/css" href="style.css">
</head>
<script>
/**
 * Given an element, or an element ID, blank its style's display
 * property (return it to default)
 */
function show(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = '';
    }
}

/**
 * Given an element, or an element ID, set its style's display property
 * to 'none'
 */
function hide(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = 'none';
    }
}

function show_content(optionsId) {
	var ids = new Array('home','data','analysis');
	show(optionsId);
	document.getElementById(optionsId + '_link').className = 'active';

	for (var i = 0; i < ids.length; i++)
	{
	    if (ids[i] == optionsId) continue;
	    hide(ids[i]);
	    document.getElementById(ids[i] + '_link').className = '';
	}
}
</script>
<body>
	<div id="header"><h1>MyApp</h1></div>

	<div id="menu">
		<a id="home_link" href="#" class="active" onclick="show_content('home'); return false;">Home</a> &middot;
		<a id="data_link" href="#" onclick="show_content('data'); update_data_charts(); return false;">Data</a> &middot;
		<a id="analysis_link" href="#" onclick="show_content('analysis'); return false;">Analysis</a> 
	</div>

	<div id="main">

		<div id="home">
			<h2>Home</h2>

			<p>
			This project aims to predict actual stock needs for top-ten sales products, by predicting next month sales.
            For each product, it shows the prediction, the actual stock, and then if there is overstock, the supplier contact. On the contrary, if there is overstock, the best client for this product according to past sales.
            For each product, the predictive model is the one with less validation error, chosen from: simple linear regression, GLM, GLM Poisson Lasso, GLM Poisson Ridge. Model features are different time horizon sales for the particular product, together with same features but applied to the overall of other products in its category, and to the overall of other products of other categories.
			</p>
		</div>	

                <?php include 'data_and_analysis.php' ?>
	
	</div>

	<div id="footer">Stock optimization </div>

</body>
</html>
<?php ?>
