$(function () {
	$("#graph2").css("background-color", "white");
});

function c3_graph(series, options, o) {
	// keep only the elements that hold the average points
	mean_data = series.filter(function (x) {return ('lines' in x)});
	// keep only elements that contain graph points (remove average points)
	series = series.filter(function (x) {return !('lines' in x)});
	let colors_obj = {}; //for keeping track of colors in non-average plots
	var chart_data = {
        	xs: {},
        	columns: [],
		type: 'scatter',
		types: {},
		names: {},
		//set color of average plots to be the same as the non-average plots
		color: function (color, d) {
			d = d.id || d;
			if (/mean/.test(d)) {
				return colors_obj[d.replace(/mean/, 'data')];
			} else {
				colors_obj[d] = color;
				return color;
			}
		}
	};
	//add the necessary data to chart_data to plot graph points
	series.forEach(function (item, index) {
		chart_data.xs['data' + index] = 'x_data' + index;
		var x_values = item.data.map(function(pair) {
			return pair[0];
		});
		var y_values = item.data.map(function(pair) {
			return pair[1];
		});
		chart_data.columns.push(['x_data' + index].concat(x_values));
		chart_data.columns.push(['data' + index].concat(y_values));
		//set displayed name
		chart_data.names['data' + index] = item.label;
	});
	//add the necessary data to chart_data to plot average graph points
	mean_data.forEach(function (item, index) {
		chart_data.xs['mean' + index] = 'x_mean' + index;
		var x_values = item.data.map(function(pair) {
			return pair[0];
		});
		var y_values = item.data.map(function(pair) {
			return pair[1];
		});
		chart_data.columns.push(['x_mean' + index].concat(x_values));
		chart_data.columns.push(['mean' + index].concat(y_values));
		//set type for plots of average points
		chart_data.types['mean' + index] = 'line';
		//set displayed name
		chart_data.names['mean' + index] = item.tooltiplabel;
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
			hide: 	(function hide (arr) {
					//if only one plot, return true (hide legend completely)
					if (series.length === 1) return true;
					//recursion:
					if (arr.length === mean_data.length) return arr;
					return hide(arr.concat(['mean' + arr.length]));
				})([]) //returns ['mean0', 'mean1', 'mean2', ...]
		},
		tooltip: {
			grouped: false
		}
	});
}
