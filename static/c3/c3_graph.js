$(function () {
	$("#graph2").css("background-color", "white");
});

function c3_graph(series, options, o) {
	// keep only the elements that hold the average points
	mean_data = series.filter(function (x) {return ('lines' in x)});
	// keep only elements that contain graph points (remove average points)
	series = series.filter(function (x) {return !('lines' in x)});

	var chart_data = {
        	xs: {},
        	columns: [],
		type: 'scatter',
		types: {}
	};
	//add the necessary data to chart_data to plot graph points
	series.forEach(function (item) {
		chart_data.xs[item.label] = 'x_' + item.label;
		var x_values = item.data.map(function(pair) {
			return pair[0];
		});
		var y_values = item.data.map(function(pair) {
			return pair[1];
		});
		chart_data.columns.push(['x_' + item.label].concat(x_values));
		chart_data.columns.push([item.label].concat(y_values));
	});
	//add the necessary data to chart_data to plot average graph points
	mean_data.forEach(function (item) {
		chart_data.xs[item.tooltiplabel] = 'x_mean';
		var x_values = item.data.map(function(pair) {
			return pair[0];
		});
		var y_values = item.data.map(function(pair) {
			return pair[1];
		});
		chart_data.columns.push(['x_mean'].concat(x_values));
		chart_data.columns.push([item.tooltiplabel].concat(y_values));
		//set type for plots of average points
		chart_data.types[item.tooltiplabel] = 'line';
	});

	console.log('Options:', o);
	console.log("Chart Data:", chart_data);

	var chart = c3.generate({
		bindto: '#graph2',
		data: chart_data,
		zoom: {
			enabled: true
		},
		legend: {
			//hide legend elements for average plots
			hide: mean_data.map(function (item) { return item.tooltiplabel; })
		}
	});
}
