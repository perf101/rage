function c3_graph(series, o, cb) {
	// keep the elements that hold the average points and distribution points
	var mean_data = series.filter(function (x) {return ('tooltiplabel' in x)});
	var distribution_data =	series.filter(function (x) {return ('lines' in x) && ('fill' in x.lines)}); //shadowSize distribution line excluded - seems to have no purpose
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
			if (/mean/.test(d)) { //if show_points=false, will use own color
				let mean_color = colors_obj[d.replace('mean', 'data')] || color;
				//store color for distribution when show_points=false
				if (distribution_data.length !== 0 && series.length === 0) colors_obj[d] = mean_color; 
				return mean_color;
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
		chart_data.types['mean' + index] = $('#line_type').val();
		//set displayed name (will have label property if show_points=false)
		chart_data.names['mean' + index] = item.label || item.tooltiplabel;
	});
	//add the necessary data to chart_data to plot average graph points
	/*distribution_data.forEach(function (item, index) {
		//format of names: dist(which plot)_(which pair)_(which value in pair)
		var dist_name_part = 'dist' + Math.floor(index/3) + '_' + index % 3;
		chart_data.xs[dist_name_part + '_0'] = 'x_dist' + index;
		chart_data.xs[dist_name_part + '_1'] = 'x_dist' + index;
		var x_values = item.data.map(function (triple) {
			return triple[0];
		});
		var y1_values = item.data.map(function (triple) {
			return triple[1];
		});
		var y2_values = item.data.map(function (triple) {
			return triple[2];
		});
		chart_data.columns.push(['x_dist' + index].concat(x_values));
		chart_data.columns.push([dist_name_part + '_0'].concat(y1_values));
		chart_data.columns.push([dist_name_part + '_1'].concat(y2_values));
	});*/
	//for measuring time taken
	var start = new Date();
	var finished_rendering = function () {
		if (typeof(cb) === "function") cb(new Date() - start);
	}

	var chart_properties = {
		bindto: '#graph2',
		data: chart_data,
		zoom: {
			enabled: true,
			rescale: $('#rescale_y').is(':checked'),
			onzoom: fillArea
		},
		legend: {
			//hide legend elements for average plots
			hide: 	(function () {
					//if only one plot, return true (hide legend completely)
					if (series.length === 1) return true;
					//if show_points=false, show mean curve legend elements (all elements)
					if (series.length === 0 && mean_data.length !== 1) return false;
					//otherwise return ['mean0', 'mean1', 'mean2', ...]
					return (function hide (arr) {
						if (arr.length === mean_data.length) return arr;
						return hide(arr.concat(['mean' + arr.length]));
					})([]); 
				})(),
			item: {
				onmouseover: 	function (id) {
							if (/mean/.test(id)) { //will only be a mean plot if show_points=false (no data plots)
								chart.focus(id);
							} else {
								chart.focus([id, id.replace('data', 'mean')]);
							}
						},
				onclick:	function (id) {
							if (/mean/.test(id)) { //will only be a mean plot if show_points=false (no data plots)
								chart.toggle(id);
							} else {
								chart.toggle([id, id.replace('data', 'mean')]);
							}
						}
			}
		},
		axis: {
			x: { 
				label: {
					text: o.xaxis,
				},
				tick: {
					rotate: (o.x_labels ? -60 : 0),
					multiline: false,
					culling: (o.x_labels ? false : true),
					format:	function (x) {
							return (o.x_labels ? o.x_labels[x] : x);
						}
				}
			},
			y: { 
				label: {
					text: (o.yaxis === 'result' ? $('span.som_name').text() : o.yaxis),
					position: 'outer-middle'
				},
				tick: {
					//make all labels visible on the y-axis 
					values:	(function () {
							//if there are no y-labels, return undefined (let the y-axis manage itself)
							if (!o.y_labels) return;
							//otherwise return [1, 2, 3, 4, ... o.y_labels.length] 
							return (function tick_values (arr) {
								if (arr.length === Object.keys(o.y_labels).length) return arr;
								return tick_values(arr.concat([arr.length + 1]));
							})([]);
						})(),
					format: function (y) {
							return (o.y_labels ? o.y_labels[y] : y);
						}
				}
			}
		},
		tooltip: {
			grouped: false,
			contents: function (d, defaultTitleFormat, defaultValueFormat, color) {
				d = d[0];
				//console.log("d:", d, "defaultTitleFormat:", defaultTitleFormat, "defaultValueFormat:", defaultValueFormat, "color:", color);
				let is_mean_series = /mean/.test(d.id);
				//set header to series label
				let title_value = (is_mean_series ? mean_data : series)[d.id.replace(/mean|data/,'')][is_mean_series ? 'tooltiplabel' : 'label'];
				let x_value = o.x_labels ? o.x_labels[d.x] : d.x;
				//round to 2 decimal places
				let decimal_format = d3.format('.2f');
				let y_value = decimal_format(d.value);
				let point_data = is_mean_series ? {} : series[d.id.replace('data','')].data[d.index][2];
				other_data = '';
				Object.keys(point_data).forEach(function (key) {
					other_data += '<tr class="' + key + '"><td class="name">' + key.replace(/_/g,' ') + ':</td><td class="value">' + point_data[key] + '</td></tr>';
				});
				return 	'<table class="c3-tooltip"><tbody>' + 
					'<tr><th colspan="2">' + title_value + '</th></tr>' +
					'<tr class="x"><td class="name">x:</td><td class="value">' + x_value + '</td></tr>' +
					'<tr class="y"><td class="name">y:</td><td class="value">' + y_value + '</td></tr>' +
					other_data + '</tbody></table>';
			}
		},
		onrendered: finished_rendering
	};

	//handle y_fromto_zero
	if ($('input[name="y_fromto_zero"]').is(':checked')) {
		chart_properties.axis.y[o.positive ? "min" : "max"] = 0;
		chart_properties.axis.y.padding = {};
		chart_properties.axis.y.padding[o.positive ? "bottom" : "top"] = 0;
	}

	console.log("Chart Properties:", chart_properties);
	var chart = c3.generate(chart_properties);
	fillArea();

	function fillArea () {
		distribution_data.forEach(function (item, index) {
			var indices = d3.range(item.data.length); 
			var xscale = chart.internal.x;
			var yscale = chart.internal.y;
			var color = colors_obj['data' + Math.floor(index/3)] || colors_obj['mean' + Math.floor(index/3)];
			var area = d3.area()
				.x(function (d) { return xscale(item.data[d][0]); })
				.y0(function (d) { return yscale(item.data[d][1]); })
				.y1(function (d) { return yscale(item.data[d][2]); })
				.curve($('#line_type').val() === "spline" ? d3.curveMonotoneX : d3.curveLinear);
			d3.select('g.c3-chart #dist' + index).remove();
			d3.select('g.c3-chart').append('path')
				.datum(indices)
				.attr('class', 'area')
				.attr('id', 'dist' + index)
				.attr('d', area)
				.style('fill', color)
				.style('opacity', 0.2);
		});
	}
}
