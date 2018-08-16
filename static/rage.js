/*
Invariants (also reflected on server side):
- Default value for field "xaxis" is "branch".
- Default value for field "yaxis" is "result".
- show_points, show_avgs, y_fromto_zero is selected by default.
- All other checkboxes are not selected by default.
- "SHOW FOR" is the first (default) option for filters ("f_").
- "ALL" is the first (default) option for filter values ("v_").
*/

// === GLOBAL VARIABLES --- start ===
var autofetch = false; // if false, the following triggers have no effect
var checkboxes_on_by_default = ["show_points", "show_avgs", "y_fromto_zero"];
//defaults for all drop-down selection options above filter boxes
var graph_selection_defaults = {
	xaxis: ["branch"], //multiselect defaults of length > 1 will always show up in the url
	yaxis: "result",
	default_graph: "flot" //default value for selection "default_graph"
};
var selected_graph_options = {}; // which specific graph options are selected
//specific options available for each graph
var specific_graph_options = {
	flot: {
		legend_position: {
			label: "Legend Position:",
			type: "select",
			options: [ ["ne", "North-East"],
				   ["nw", "North-West"],
				   ["se", "South-East"],
				   ["sw", "South-West"],
				   ["__", "(nowhere)"] ]
		},
		symbol: {
			label: "Symbol to use:",
			type: "select",
			options: [ ["Diamond", "Diamond"],
				   ["Circle", "Circle"],
				   ["Cross", "Cross"],
				   ["Square", "Square"],
				   ["Triangle", "Triangle"] ]
		}
	},
	d3: {},
	c3: {
		line_type: {
			label: "Line Type:",
			type: "select",
			options: [ ["line", "Line"],
				   ["spline", "Spline"] ]
		},
		rescale_y: {
			label: "Autorescale Y:",
			type: "checkbox"
		},
		zoom_enabled: {
			label: "Zoom enabled:",
			type: "checkbox"
		},
		legend_show: {
			label: "Show legend:",
			type: "checkbox"
		}
	}
};
var specific_graph_options_defaults = {
	flot: { legend_position: "ne", symbol: "Circle" },
	d3: {},
	c3: { line_type: "line", rescale_y: false, zoom_enabled: true, legend_show: true }
};
var graph_only_fields = [
  "#xaxis", "#yaxis", "#show_points", "#show_avgs", "#show_dist",
  "#x_from_zero", "#y_fromto_zero", "#x_as_seq", "#y_as_seq", "#x_as_num", "#show_all_meta",
  "#symbol", "#xaxis_log", "#yaxis_log", "#legend_position", "#get_img", "#auto_redraw"
];
var url_params = get_url_params();
var debug = ("debug" in url_params);
var filters_visible = true;

// ==== GLOBAL VARIABLES --- end ====

// ======== MAIN --- begin ===========
decode_all();
if ("som" in url_params)
  som_page_init();
else if (url_params.p == "soms_by_tc")
  soms_by_tc_page_init();
// ========= MAIN --- end ============

function endsWith(str, suffix) {
  return str.indexOf(suffix, str.length - suffix.length) !== -1;
}

function decode(encoded) {
  return decodeURIComponent(encoded).replace(/\+/g, " ");
}

function decode_all() {
  $(".encoded").each(function() {
    var elem = $(this)[0];
    elem.innerHTML = decode(elem.innerHTML);
  });
}

function som_page_init() {
  // automatically change available configuration given view
  $("#view").change(view_change);
  // reset configuration button
  $("#reset_config").click(function() {window.location.href = get_som_url();});
  // "stop" button is disabled by default
  $("#stop_plotting").prop("disabled", true);
  $("#stop_plotting").click(on_stop_plotting);
  $("#toggle_filters").click(toggle_filter_visibility);
  $("#redraw").click(redraw_graph);
  // automatic refresh on change
  $("select[name='xaxis']").change(redraw_trigger);
  $("select[name='yaxis']").change(redraw_trigger);
  $("input[name='show_points']").change(redraw_trigger);
  $("input[name='show_avgs']").change(redraw_trigger);
  $("input[name='show_dist']").change(redraw_trigger);
  $("input[name='x_from_zero']").change(redraw_trigger);
  $("input[name='y_fromto_zero']").change(redraw_trigger);
  $("input[name='x_as_seq']").change(redraw_trigger);
  $("input[name='y_as_seq']").change(redraw_trigger);
  $("input[name='x_as_num']").change(redraw_trigger);
  $("input[name='show_all_meta']").change(redraw_trigger);
  $("input[name='xaxis_log']").change(redraw_trigger);
  $("input[name='yaxis_log']").change(redraw_trigger);
  $(".filterselect").change(redraw_trigger);
  $(".multiselect").change(function() {
    redraw_trigger(); 
    if ($(this).prop('value') == "ALL")
      $("th[name='title_" + $(this).prop('name') + "']").css({'color':'black'});
    else
      $("th[name='title_" + $(this).prop('name') + "']").css({'color':'#e21b09'});
  });
  // fetch and process data immediately
  preselect_fields_based_on_params();
  load_default_graph();
  $("#graph_title").css('display','none'); //until a graph is drawn, the graph title should be hidden
  $("#redraw").css('color', '#e21b09'); //until a graph is drawn, the Draw/Redraw button should be red
  set_auto_redraw(); //will call fetch_data_and_process() if auto_redraw is enabled
  // extract image button
  load_get_image_if_not_ie();
  // tiny url
  $("#get_tinyurl").click(get_tinyurl);
  $("input[name='auto_redraw']").change(set_auto_redraw);
}

function soms_by_tc_page_init() {
  var request = get_base_url() + "/?p=soms";
  console.log("Request:", request);
  $.ajax({
    url: request,
    type: 'GET',
    dataType: 'json',
    success: on_soms_by_tc_received,
    error: on_async_fail
  });
}

function on_soms_by_tc_received(o) {
  console.log("Response:", o);
  var tc_to_soms = {};
  $.each(o.soms, function(som_id, som) {
    if (!(som.tc in tc_to_soms)) tc_to_soms[som.tc] = [];
    tc_to_soms[som.tc].push(som_id);
  });
  var base_url = get_base_url();
  var s = "";
  $.each(o.tcs, function(tc_fqn, tc) {
    s += "<h2 class=\"heading\">" + tc_fqn + " (" + tc.desc + ")</h2>";
    if (tc_fqn in tc_to_soms) {
      s += "<ul class=\"link_list som\">";
      $.each(tc_to_soms[tc_fqn], function(i, som_id) {
        var som_url = base_url + "/?som=" + som_id;
        var som_caption = som_id + " (" + o.soms[som_id].name + ")";
        s += "<li><a href='" + som_url + "'>" + som_caption + "</a></li>";
      });
      s += "</ul>";
    } else {
      s += "<ul class=\"link_list no_item\"><li><a href=\"#\" onclick=\"return false;\">none</a></li></ul>";
    }
  });
  $("body").append(s);
}

function toggle_filter_visibility() {
  if (filters_visible) {
    filters_visible = false;
    $("#toggle_filters").prop('value', 'Show Configuration');
    $("form[name='optionsForm']").css('display','none');
  }
  else {
    filters_visible = true;
    $("#toggle_filters").prop('value', 'Hide Configuration');
    $("form[name='optionsForm']").css('display','block');
  }
}
function view_change() {
  autofetch = false;
  var view = $('#view').val();
  var is_graph = (view == "Graph");
  for (var i in graph_only_fields)
    $(graph_only_fields[i]).toggle(is_graph);
  $("input[name='show_points']").prop("checked", is_graph);
  $("input[name='show_avgs']").prop("checked", is_graph);
  $("input[name='show_all_meta']").prop("checked", !is_graph);
  fetch_data_and_process();
  if (is_graph)
    autofetch = is_checked("auto_redraw");
  else
    autofetch = true;
}

function on_by_default(name) {
  return $.inArray(name, checkboxes_on_by_default) >= 0;
}

function preselect_fields_based_on_params() {
  var params = get_url_params();
  delete params.som;
  //get graph-specific options data from parameters
  if(params.graph_specific_options) {
    console.log("Received Graph Options:", JSON.parse(decode(params.graph_specific_options[0])));
    selected_graph_options = JSON.parse(decode(params.graph_specific_options[0]));
    delete params.graph_specific_options;
  }
  specific_graph_options_init();
  //apply parameter data
  for (var param in params) {
    var elt = $("[name='" + param + "']");
    if (elt.is("select")) {
      // If any of the options we're expecting to select don't exist, append them
      var known_vals = $.map(elt.find('option'), function(elt, i) { return $(elt).val(); });
      var vals = params[param].map(decode);
      for (var i in vals) {
        var option = vals[i];
        if (known_vals.indexOf(option) < 0) {
	  console.log('Note: ' + param + ' option "' + option + '" is not known; added it. Known vals:' + known_vals);
          elt.append("<option value='" + option + "'>" + option + "</option>");
        }
      }
    }
    $("[name='" + param + "']").val(params[param].map(decode));
    $("th[name='title_" + param + "']").css({'color':'#e21b09'});
  }
  if ("filters_visible" in params) {
    toggle_filter_visibility();   
  }
  //apply default values
  checkboxes_on_by_default.forEach(function (cb_name) {
    if (cb_name in params) return;
    $("input[name='" + cb_name + "']").prop("checked", true);
  });
  Object.keys(graph_selection_defaults).forEach(function (sel_name) {
    if (sel_name in params) return;
    $("select[name='" + sel_name + "']").val(graph_selection_defaults[sel_name]);
  });
}


//apply necessary defaults to graph (what is missing is set to default)
function specific_graph_options_init () {
  for (graph_type in specific_graph_options_defaults) {
	var graph_type_defaults = specific_graph_options_defaults[graph_type];
	if (!selected_graph_options[graph_type]) {
		selected_graph_options[graph_type] = jQuery.extend(true, {}, graph_type_defaults); //deep copy (changes to selected shouldn't affect defaults)
		continue;
	}
	for (option in graph_type_defaults) {
		if (!(option in selected_graph_options[graph_type])) {
			selected_graph_options[graph_type][option] = graph_type_defaults[option]; //no deep copy (not object)
		}
	}
  }
}

function get_url_params() {
  var href = window.location.href;
  var s = href.slice(href.indexOf('?') + 1);
  return extract_params(s);
}

function load_get_image_if_not_ie() {
  if ($.browser == "msie") return;
  $.getScript("canvas2image.js", function() {
    $('#get_img').click(get_image);
    $('#get_img').toggle(true);
  });
}

function extract_params(s) {
  var result = {};
  var pairs = s.split('&');
  for(var i in pairs) {
    var pair = pairs[i].split('=');
    var key = pair[0];
    var value = pair[1] ? pair.slice(1, pair.length).join("=") : ""; //goes beyond pair[1] in case the value originally contained an equals sign
    if (!result[key]) result[key] = [];
    result[key].push(value.replace(/\+/g, " "));
  }
  return result;
}

function get_image() {
  var canvas = $('canvas.flot-base')[0];
  var context = canvas.getContext("2d");
  var w = canvas.width;
  var h = canvas.height;
  var origData = context.getImageData(0, 0, w, h);
  var compositeOperation = context.globalCompositeOperation;
  context.globalCompositeOperation = "destination-over";
  context.fillStyle = "white";
  context.fillRect(0, 0, w, h);
  Canvas2Image.saveAsPNG(canvas);
  context.clearRect(0, 0, w, h);
  context.putImageData(origData, 0, 0);
  context.globalCompositeOperation = compositeOperation;
}

function get_tinyurl() {
  $.ajax({
    url: get_base_url(),
    data: {p: "create_tiny_url", url: get_permalink()},
    type: 'POST',
    dataType: 'json',
    success: function(data) {
      console.log(data);
      var tiny_url = get_base_url() + "/?t=" + data.id;
      $("#tinyurl").attr("href", tiny_url);
      $("#tinyurl").html(tiny_url);
      $("#tinyurl").toggle(true);
    },
    error: on_async_fail
  });
}

function get_base_url() {
  var href = window.location.href;
  return href.substring(0, href.indexOf('?')).substring(0, href.lastIndexOf('/'));
}

function get_som_url() {
  return "/?som=" + get_url_params().som;
}

function set_auto_redraw() {
  if (is_checked("auto_redraw")) {
    autofetch = true;
    fetch_data_and_process();
    $("#redraw").prop('value', 'Redraw');
  }
  else {
    autofetch = false;
    $("#redraw").prop('value', 'Draw');
  }
}

function serialise_params(params) {
  var keyvals = [];
  for (var p in params) {
    var v = params[p];
    var encKey = encodeURIComponent(p);
    for (var i in v) {
      var encVal = encodeURIComponent(v[i]);
      keyvals.push(encKey + "=" + encVal);
    }
  }
  return keyvals.join("&");
}

function get_minimised_params() {
  //get parameter data
  var form_data = $('form[name=optionsForm]').serialize();
  var params = extract_params(form_data);
  if (!filters_visible) {
    params["filters_visible"] = ["false"];
  }

  //remove params that are set to default values
  checkboxes_on_by_default.forEach(function (name) {
    if (name in params) delete params[name]; //if a checkbox parameter that is on by default is found in the list (is checked), remove it from the list
    else params[name] = ["off"]; //otherwise, add it to the list and indicate that it is off
  });
  Object.keys(graph_selection_defaults).forEach(function (name) {
    let is_equal;
    //check if this is a mutiselect option
    if (graph_selection_defaults[name].constructor === Array) {
      //check arrays for equality (only for array lengths of 1; otherwise just put it in the url even if the elements are the same)
      is_equal = (params[name].length === 1 && params[name][0] === graph_selection_defaults[name][0]); 
    } else {
      //otherwise compare the two strings
      is_equal = (params[name][0] === graph_selection_defaults[name]);
    }
    if (is_equal) delete params[name];
  });
  //get graph-specific options with default values removed
  var graph_options_without_defaults = jQuery.extend(true, {}, selected_graph_options); //deep copy
  for (graph_type in graph_options_without_defaults) {
    for (option in graph_options_without_defaults[graph_type]) {
      if (specific_graph_options_defaults[graph_type][option] === graph_options_without_defaults[graph_type][option]) {
        delete graph_options_without_defaults[graph_type][option];	
      }
    }
    if (jQuery.isEmptyObject(graph_options_without_defaults[graph_type])) delete graph_options_without_defaults[graph_type];
  }
  console.log("Graph Options:", graph_options_without_defaults);
  if (!jQuery.isEmptyObject(graph_options_without_defaults)) params.graph_specific_options = [JSON.stringify(graph_options_without_defaults)];
  //minimize parameter data (while excluding default filter box values)
  var minimised = {};
  for (var p in params) {
    var vs = params[p];
    var l = vs.length;
    var f = vs[0]; // first value (the only one for non-multi-selections)

    var is_show_for = (/^f_/.test(p) && params[p][0] == "0");
    var is_all_only = (/^v_/.test(p) && params[p].length == 1 && params[p][0] == "ALL");

    if (!(is_show_for || is_all_only)) {
      minimised[p] = $.map(vs, decode);
    }
  }
  return minimised;
}

function get_permalink() {
  var minimised = get_minimised_params();
  var serialised = serialise_params(minimised);
  console.log(serialised);
  if (serialised != "") serialised = "&" + serialised;
  return get_som_url() + serialised;
}

function redraw_graph() {
  fetch_data_and_process();
}

function redraw_trigger() {
  if (autofetch)
    fetch_data_and_process();
  else
    $("#redraw").css('color', '#e21b09');
}

function set_graph_title() {
  var form_data = $('form[name=optionsForm]').serialize();
  var params = extract_params(form_data);
  var set_vars = [];
  for (var param in params) {
    if (param.startsWith("v_") && params[param] != "ALL") {
      set_vars.push(param.substr(2) + " = " + params[param]);
    }
  }
  var graph_title = "";
  if (set_vars.length == 0) {
    $("#graph_title").css('display','none');
  } else {
    $("#graph_title").css('display','block');
    graph_title += "<ul>" + set_vars.map(function(v) { return "<li>" + decode(v) + "</li>" }).join("\n") + "</ul>";
  }
  $("#graph_title").html(graph_title);
}

function fetch_data_and_process() {
  $("#redraw").css('color', 'black');
  $("#tinyurl").toggle(false);
  $("#progress_img").toggle(true);
  set_graph_title();
  var som_id = url_params.som[0];
  var request = "/?p=som_data&id=" + som_id;
  var params = get_minimised_params();
  console.log("Request:", request, params);
  $.ajax({
    url: request,
    type: 'POST',
    data: params,
    dataType: 'json',
    success: on_received,
    error: on_async_fail
  });
}

function numerical_sort(array) {
  array.sort(function(a, b) {return a - b;});
}

function get_sorted_keys(o) {
  var keys = []
  for (var k in o) keys.push(k);
  numerical_sort(keys);
  return keys;
}

function has_labels(o, axis) {
  return (axis + "_labels") in o;
}

var graph_object = new GraphObject();

function on_received(o) {
  console.log("Received:", o);
  var view = $('#view').val();
  if (view == "Graph") {
    $('#table').hide();
    $('#graph').show();
    graph_object.draw_graph(o, on_plotting_finished);
  } else {
    if (view == "Table") {
      $('#graph').hide();
      $('#table').show();
      make_table(o, on_plotting_finished);
    } else console.log("Unknown view.");
  }
}

// Called when the user clicks on the Stop button.
function on_stop_plotting() {
  graph_object.stop_plotting();
  on_plotting_finished();
}

// Called after a successful plot, or when user clicks on Stop.
function on_plotting_finished(plot_time) {
  if (plot_time) console.log("Plotting took " + plot_time + "ms.");
  $("#stop_plotting").prop("disabled", true);
  $("#progress_img").toggle(false);
}

function is_checked(cb_name) {
  return $("input[name='" + cb_name + "']").is(":checked");
}

var tooltip_counter = 0;

function GraphObject() {
  this.draw_graph = draw_graph;
  this.stop_plotting = stop_plotting;

  var flot_object = null;
  var series = [];
  var num_series = 0;

  // converts "[[x, y]]" to "[[x, [y1,y2,..]]]"
  function group_by_x(data) {
    var x_to_ys = {};
    for (i in data) {
      var point = data[i];
      var x = point[0];
      if (!(x in x_to_ys)) x_to_ys[x] = [];
      x_to_ys[x].push(point[1]);
    }
    var xs = Object.keys(x_to_ys);
    numerical_sort(xs);
    var x_ys_array = [];
    for (i in xs) {
      var x = xs[i];
      x_ys_array.push([x, x_to_ys[x]]);
    }
    return x_ys_array;
  }

  function get_averages(data) {
    var avgs = [];
    var plus = function(a, b) {return a + b;};
    $.each(group_by_x(data), function(i, x_ys) {
      var x = x_ys[0], ys = x_ys[1];
      avgs.push([x, ys.reduce(plus, 0) / ys.length]);
    });
    return avgs;
  }

  function get_distribution_lines(data) {
    // var avgs = [], min_maxs = [], std_devs = [];
    var medians = [], prc40to60s = [], prc25to75s = [], prc15to85s = [];
    var plus = function(acc, x) {return acc + x;};
    var plus_sq = function(acc, x) {return acc + x*x;};
    var min = function(acc, x) {return acc < x ? acc : x;};
    var max = function(acc, x) {return acc < x ? x : acc;};
    $.each(group_by_x(data), function(i, x_ys) {
      var x = x_ys[0], ys = x_ys[1];
      numerical_sort(ys);
      var n = ys.length;
      var avg = ys.reduce(plus) / n;
      // avgs.push([x, avg]);
      // min_maxs.push([x, ys.reduce(max, -Infinity), ys.reduce(min, Infinity)]);
      // var std_dev = Math.sqrt(ys.reduce(plus_sq) / n - avg*avg);
      // std_devs.push([x, avg + std_dev, avg - std_dev]);
      medians.push([x, ys[n / 2], ys[n / 2]]);
      prc40to60s.push([x, ys[Math.floor(n * 0.60)], ys[Math.floor(n * 0.40)]]);
      prc25to75s.push([x, ys[Math.floor(n * 0.75)], ys[Math.floor(n * 0.25)]]);
      prc15to85s.push([x, ys[Math.floor(n * 0.85)], ys[Math.floor(n * 0.15)]]);
    });
    return { // min_max: min_maxs, std_dev: std_devs,
      median: medians, prc40to60: prc40to60s, prc25to75: prc25to75s,
      prc15to85: prc15to85s};
  }

  function draw_graph(o, cb) { //will call callback function and pass in the time that plotting started
    stop_plotting();
    // default options
    point_series = o.series;
    num_series = point_series.length;
    // Prompt if there are lots of points to plot
    total_points = 0;
    for (var i = 0; i< num_series; i++) {
      total_points += point_series[i].data.length;
    }
    if (total_points > 10000) {
      if (!window.confirm("About to plot " + total_points + " points. This could take a while. Continue?")) {
	if (typeof cb === "function") cb();
	return;
      }
    }

    series = is_checked("show_points") ? point_series : [];
    // averages and distributions
    for (var i = 0; i < num_series; i++) {
      if (is_checked("show_dist")) {
        var dist = get_distribution_lines(point_series[i].data);
        var add_percentile = function(i, data, fill) {
          series.push({color: point_series[i].color, data: data,
                       label: null, points: {show: false},
                       lines: {show: true, lineWidth: 0, fill: fill}});
        };
        add_percentile(i, dist.prc15to85, 0.2);
        add_percentile(i, dist.prc25to75, 0.4);
        add_percentile(i, dist.prc40to60, 0.6);
        var label_shown = is_checked("show_points") || is_checked("show_avgs");
        series.push({color: point_series[i].color, data: dist.median,
                     label: label_shown ? null : point_series[i].label,
                     points: {show: false}, shadowSize: 0.7,
                     lines: {show: true}});
      }
      if (is_checked("show_avgs"))
        series.push({
          color: point_series[i].color, data: get_averages(point_series[i].data),
          label: is_checked("show_points") ? null : point_series[i].label,
          tooltiplabel : point_series[i].label + " (mean)",
          points: {show: !is_checked("show_points")},
          lines: {show: true}
        });
    }
    // at this point, o.series and series may be different (use the data from series)

    $("#stop_plotting").prop("disabled", false);

    console.log("Options:", o);
    let graph_type = $("#graph_option").val();
    switch (graph_type) {
	case "flot":
    	    flot_object = flot_graph(series, o, cb);
	    break;
	case "d3":
	    d3_graph(series, o, cb);
	    break;
	case "c3":
	    c3_graph(series, o, cb);
	    break;
    }
  }

  // Called when starting a new plot, or when user clicks on Stop.
  function stop_plotting() {
    if (flot_object) flot_object.shutdown();
  }
}


function make_table(o, cb) {
  var content = '';
  // shell begin
  content += '<table border="1" class="tablesorter">';
  // captions
  var captions = [];
  var xaxis = $("select[name='xaxis']").val();
  var yaxis = $("select[name='yaxis']").val();
  var ts = $('.filter_table th').text(function(i, t) {captions.push(t);});
  captions.push("result");
  content += '<thead><tr>';
  for (var i in captions) content += '<th>' + captions[i] + '</th>';
  content += '</tr></thead><tbody>';
  // data
  var x_has_labels = has_labels(o, "x");
  var y_has_labels = has_labels(o, "y");
  var data = o.series.length == 0 ? [] : o.series[0].data;
  for (var i = 0; i < data.length; ++i) {
    content += '<tr>';
    var point = data[i];
    var props = point[2];
    for (var j in captions) {
      var c = captions[j];
      var v;
      if (c == xaxis) {
        v = point[0];
        if (x_has_labels) v = o.x_labels[v];
      } else if (c == yaxis) {
        v = point[1];
        if (y_has_labels) v = o.y_labels[v];
      } else v = props[c];
      content += '<td>' + v + '</td>';
    }
    content += '</tr>';
  }
  // shell end + output
  content += '</tbody></table>';
  $('#table').html(content);
  //$("#table .tablesorter").tablesorter();
  if (typeof(cb) === "function") cb();
}

function on_async_fail(XMLHttpRequest, textStatus, errorThrown) {
  console.log(XMLHttpRequest);
  console.log(textStatus);
  console.log(errorThrown);
}

function load_default_graph() {
	var sel_value = $("select[name='default_graph']").val();
	$("#graph_option").val(sel_value);
	//change the graph (if necessary) and reveal graph_options
	change_graph();
}

function change_graph() {

        var sel_value = $("#graph_option").val();

        if (sel_value == "c3") {
		$(".graph_container").hide();
		$("#graph1").hide();
		$("#graph2").show();

	} else if (sel_value == "d3") {
                $(".graph_container").hide();
		$("#graph2").hide();
                $("#graph1").show();

        } else if (sel_value == "flot") {
                $("#graph1").hide();
		$("#graph2").hide();
                $(".graph_container").show();
        }

	//reset id and remove old options
	var options_container = $(".specific_graph_options");
	options_container.attr('id', sel_value).find("*").remove();
	//add new options
	var options_elements = generate_option_elements(specific_graph_options[sel_value]);
	options_container.append(options_elements);
	options_container.find("select").change(function () {
		selected_graph_options[sel_value][$(this).attr("id")] = $(this).val();
		redraw_trigger();
	});
	options_container.find("input").change(function () {
		selected_graph_options[sel_value][$(this).attr("id")] = $(this).is(":checked");
		redraw_trigger();
	});
	// apply selected options
	Object.keys(selected_graph_options[sel_value]).forEach(function (option) {
		if (specific_graph_options[sel_value][option].type === "select") {
			options_container.find('#' + option).val(selected_graph_options[sel_value][option]);
		} else { //checkbox
			options_container.find('#' + option).prop("checked", selected_graph_options[sel_value][option]);
		}
	});
	//trigger redraw
	redraw_trigger();
}

function generate_option_elements(options) {
	var elements = "";
	Object.keys(options).forEach(function (key) {
		let o = options[key];
		if (o.type === "select") {
			//construct dropdown
			let options = "";
			o.options.forEach(function (item) {
				options += "<option value='" + item[0] + "'>" + item[1] + "</option>";
			});
			elements += "<div><b>" + o.label + "</b><select id='" + key + "'>" + options + "</select></div>";
		} else if (o.type === "checkbox") {
			//construct checkbox
			elements += "<div><b>" + o.label + "</b><input id='" + key + "' type='checkbox'></div>";
		} else {
			console.error("unknown type for graph-specific options");
		}
	});
	return elements;
}

// Presets
function _setSelected(sel, val, selected){
    options = Array.from($(`select[name=${sel}] option`));
    for(var i=0; i<options.length; i++){
        option = options[i];
        if(option.value == val){
            option.selected = selected;
            break;
        }
    }
    $(`select[name=${sel}`).trigger('change');
}

const unselect = (sel, val) => _setSelected(sel, val, false);
const select = (sel, val) => _setSelected(sel, val, true);
const unselectAll = (sel) => {
    Array.from($(`select[name=${sel}] option`)).map(opt => opt.selected = false); 
    $(`select[name=${sel}`).trigger('change');
};

const setPresetBriefReport = () => {
    // unselect build tag, select build date
    unselect('xaxis', 'build_tag');
    select('xaxis', 'build_date');

    // select master
    select('v_branch', 'master');

    // split by branch
    select('f_branch', 1); // 0=show for, 1=split by

    // Select 'All' build number
    unselectAll('v_build_number');
    select('v_build_number', 'ALL');

    // Select 'All' build tag
    unselectAll('v_build_tag');
    select('v_build_tag', 'ALL');

    // Select SW legend position, our interesting data is usually NE
    select('legend_position', 'sw');

    // Enable autodraw - jquery to trigger its jquery change event
    $('input[name=auto_redraw]').prop('checked', true);
    redraw_graph();
};

// Set the preset brief report analysis button to call the setPresetBriefReport method
document.querySelector('#preset-brief').addEventListener('click', (e) => setPresetBriefReport())

if(location.hash == '#brief_report_analysis'){
    setPresetBriefReport();
}
