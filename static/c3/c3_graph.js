$(function () {
	$("#graph2").css("background-color", "white");
});

function c3_graph(series, options, o) {
	// keep only elements that contain useful information for the graph
	series = series.filter(function (x) {return !('lines' in x)});

	var chart_data = {
        	xs: {},
        	columns: []
	};
	series.forEach(function (item, index) {
		chart_data.xs['data' + index] = 'x' + index;
		var x_values = item.data.map(function(pair) {
			return pair[0];
		});
		var y_values = item.data.map(function(pair) {
			return pair[1];
		});
		chart_data.columns.push(['x' + index].concat(x_values));
		chart_data.columns.push(['data' + index].concat(y_values));
	});
	console.log("Chart Data:", chart_data);
	var chart = c3.generate({
		bindto: '#graph2',
		data: chart_data
	});
}
