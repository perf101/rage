/*
Invariants (also reflected on server side):
- Default value for field "xaxis" is "branch".
- Default value for field "yaxis" is "result".
- Y minimum set to zero ("y_fromto_zero") is selected by default.
- All other checkboxees are not selected by default.
- "SHOW FOR" is the first (default) option for filters ("f_").
- "ALL" is the first (default) option for filter values ("v_").
*/

// === GLOBAL VARIABLES --- start ===
var autofetch = true; // if false, the following triggers have no effect
var checkboxes_on_by_default = ["show_points", "show_avgs", "y_fromto_zero", "auto_redraw"];
var graph_only_fields = [
  "#xaxis", "#yaxis", "#show_points", "#show_avgs", "#show_dist",
  "#x_from_zero", "#y_fromto_zero", "#x_as_seq", "#y_as_seq", "#x_as_num", "#show_all_meta",
  "#symbol", "#xaxis_log", "#yaxis_log", "#legend_position", "#get_img", "#auto_redraw"
]
var url_params = get_url_params();
var debug = ("debug" in url_params);

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
  $("select[name='legend_position']").change(redraw_trigger);
  $("select[name='symbol']").change(redraw_trigger);
  $(".filterselect").change(redraw_trigger);
  $(".multiselect").change(redraw_trigger);
  // fetch and process data immediately
  preselect_fields_based_on_params();
  set_auto_redraw();
  if (autofetch) fetch_data_and_process();
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
    $("th[name='title_" + param + "']").css({'color':'red'});
  }
  for (var i in checkboxes_on_by_default) {
    var cb_name = checkboxes_on_by_default[i];
    if (cb_name in params) continue;
    $("input[name='" + cb_name + "']").prop("checked", true);
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
    var value = pair.slice(1, pair.length).join("=");
    if (!result[key]) result[key] = [];
    if (typeof(pair[1]) === 'undefined') value = "";
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
  if (is_checked("auto_redraw"))
    autofetch = true;
  else
    autofetch = false;
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
  var form_data = $('form[name=optionsForm]').serialize();
  var params = extract_params(form_data);
  $.each($('form[name=optionsForm] input[type=checkbox]'), function(i, cb) {
    if (!on_by_default(cb.name)) return;
    if (cb.name in params) delete params[cb.name];
    else params[cb.name] = ["off"];
  });
  var minimised = {};
  for (var p in params) {
    var vs = params[p];
    var l = vs.length;
    var f = vs[0]; // first value (the only one for non-multi-selections)
    var is_xaxis_branch = p == "xaxis" && vs == ["branch"];
    var is_yaxis_result = p == "yaxis" && f == "result";
    var is_show_for = p.indexOf("f_") == 0 && f == "0";
    var is_all_only = p.indexOf("v_") == 0 && l == 1 && f == "ALL";
    var is_legend_position_ne = p == "legend_position" && f == "ne";
    var is_symbol_circle = p == "symbol" && f == "Circle";
    if (!(is_xaxis_branch || is_yaxis_result || is_show_for || is_all_only
        || is_legend_position_ne || is_symbol_circle))
      minimised[p] = $.map(vs, decode);
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
  if (!autofetch) return;
  fetch_data_and_process();
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
      make_table(o);
    } else console.log("Unknown view.");
    on_plotting_finished();
  }
}

// Called when the user clicks on the Stop button.
function on_stop_plotting() {
  graph_object.stop_plotting();
  on_plotting_finished();
}

// Called after a successful plot, or when user clicks on Stop.
function on_plotting_finished() {
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

  var graph_data = {};
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

  function safe_log(x) {
    if (x <= 0) x = 0.0001;
    return Math.log(x);
  }

  function configure_labels(o, axis, options) {
    var axis_options = options[axis + "axis"];
    var quantity = o[axis + "axis"];
    if (quantity == "build_number") {
      axis_options.ticks = 10;
      axis_options.tickDecimals = 0;
    }
    if (!has_labels(o, axis)) return;
    var labels = o[axis + "_labels"];
    axis_options.min = 1;
    axis_options.tickFormatter = function(val, axis) {
      return (val in labels) ? labels[val] : '';
    };
    axis_options.tickSize = 1;
  }

  function create_log_ticks(axis) {
    var result = [];
    var start = Math.floor(axis.min);
    if (start <= 1) start = 1;
    var end = Math.ceil(axis.max);
    var current = start;
    while (current < end) {
      result.push(current);
      var exp = Math.floor(safe_log(current)/Math.log(10));
      current += Math.pow(10, exp);
    }
    result.push(end);
    return result;
  }

  function show_tooltip(graph, page_x, page_y, contents) {
    var graph_pos = graph.offset();
    var x = page_x - graph_pos.left;
    var y = page_y - graph_pos.top;
    var tooltip = $(contents).css({
      'top': y, 'left': x + 5
    }).appendTo(graph).fadeIn(200);
    tooltip.children("img").click(function () {
      $(this).parent().remove();
    });
  }

  function get_metadata(item) {
    var o = graph_data;
    var x = item.datapoint[0].toFixed(2);
    var y = item.datapoint[1].toFixed(2);
    var xl = has_labels(o, "x") ? o.x_labels[Math.floor(x)] : x;
    var yl = has_labels(o, "y") ? o.y_labels[Math.floor(y)] : y;
    var label = "";
    if ("tooltiplabel" in item.series)
      label = item.series.tooltiplabel;
    else if ("label" in item.series)
      label = item.series.label;
    else if (item.seriesIndex >= num_series) {
      var s = series[item.seriesIndex - num_series];
      if ("label" in s) label = s.label + " (mean)";
    }
    var metadata = [];
    if (label)
      metadata.push(['series', label]);
    metadata.push(['x', xl]);
    metadata.push(['y', yl]);
    var itemData = item.series.data[item.dataIndex];
    if (2 in itemData) {
      var props = itemData[2];
      for (p in props)
        metadata.push([p, props[p]]);
    }
    return metadata;
  }

  function generate_tooltip(item, id, diffs) {
    var click_tooltip = typeof id === 'undefined';
    id = typeof id === 'undefined' ? "tooltip_" + tooltip_counter++ : id;
    var body = "<div id='" + id + "' class='tooltip'><table width='100%'>";
    var metadata = get_metadata(item);
    for (var i in metadata) {
      var kv = metadata[i];
      body += "<tr><th>" + kv[0] + ":</th><td>" + kv[1] + "</td></tr>";
    }
    body += "</table>";
    if (click_tooltip)
      body += "<img src='/close.png' />";
    else if (Object.keys(diffs).length > 0) {
      body += "<br/><br/><table>";
      body += "<tr><th colspan='3'>Differences from latest selection:</th></tr>";
      body += "<tr><th>Key</th><th>Latest selected point</th><th>This point</th><th>% Diff</th></tr>";
      for (diff in diffs) {
        d = diffs[diff];
        prev_val = parseFloat(d[0]);
        new_val =parseFloat(d[1]);
        percent_diff = (isNaN(prev_val) || isNaN(new_val))? '' : ((new_val-prev_val)/prev_val*100).toFixed(2);
        body += "<tr><th>" + diff + "</th><td>" + d[0] + "</td><td>" + d[1] + "</td><td>" + percent_diff + "</td></tr>";
      }
      body += "</table>";
    }
    body += "</div>";
    return body;
  }

  function metadata_diff(x, y) {
    // Convert both to dictionaries
    var xdict = {};
    var ydict = {};
    for (var i in x) {
      var kv = x[i];
      xdict[kv[0]] = kv[1];
    }
    for (var i in y) {
      var kv = y[i];
      ydict[kv[0]] = kv[1];
    }
    // Find the differences
    diffs = {}
    for (var k in xdict) {
      if (k in ydict) {
	// compare values
	if (xdict[k] != ydict[k]) {
	  diffs[k] = [xdict[k], ydict[k]];
	}
      }
    }
    return diffs;
  }

  function draw_graph(o, cb) {
    stop_plotting();
    graph_data = o;
    var graph = $("#" + o.target);
    // HTML graph labels
    graph.siblings(".xaxis").html(o.xaxis);
    graph.siblings(".yaxis").html((o.yaxis == "result") ? yaxis = $("span[class='som_name']").text() : o.yaxis); // use the name of the SOM rather than "result"
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
	on_plotting_finished();
	return;
      }
    }
    var symbol = $("select[name='symbol']").val().toLowerCase();
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
          points: {show: !is_checked("show_points"), symbol: symbol},
          lines: {show: true}
        });
    }
    // options
    var tickGenerator = function(axis) {
      var result = [];
      var step = (axis.max - axis.min) / 10;
      var current = axis.min;
      while (current <= axis.max) {
        result.push(current);
        current += step;
      }
      return result;
    };
    var options = {
      xaxis: {axisLabel: o.xaxis, labelAngle: 285},
      yaxis: {axisLabel: o.yaxis},
      grid: {
        clickable: true,
        hoverable: true,
        canvasText: {show: true}
      },
      legend: {
        type: "canvas",
        backgroundColor: "white",
        position: $("select[name='legend_position']").val(),
      },
      points: {show: true, symbol: symbol}
    };
    // force X from 0
    if (is_checked("x_from_zero"))
      options.xaxis.min = 0;
    // force Y from/to 0
    if (is_checked("y_fromto_zero"))
      options.yaxis[o.positive ? "min" : "max"] = 0;
    // labels
    configure_labels(o, "x", options);
    configure_labels(o, "y", options);
    // log scale
    if (is_checked("xaxis_log")) {
      options.xaxis.transform = safe_log;
      options.xaxis.inverseTransform = Math.exp;
      options.xaxis.ticks = create_log_ticks;
    }
    if (is_checked("yaxis_log")) {
      options.yaxis.transform = safe_log;
      options.yaxis.inverseTransform = Math.exp;
      options.yaxis.ticks = create_log_ticks;
    }
    $("#stop_plotting").prop("disabled", false);
    var start = new Date();
    flot_object = $.plot(graph, series, options, function() {
      console.log("Plotting took " + (new Date() - start) + "ms.");
      // click
      graph.unbind("plotclick");
      var latest_selection = null;
      graph.bind("plotclick", function (event, pos, item) {
        if (!item) return;
        show_tooltip(graph, item.pageX + 10, item.pageY, generate_tooltip(item));
	latest_selection = item;
      });
      // hover
      var previousPoint = null;
      graph.unbind("plothover");
      graph.bind("plothover", function (event, pos, item) {
        if (!item) {
          $("#hover_tooltip").remove();
          previousPoint = null;
        } else if (previousPoint != item.dataIndex) {
          previousPoint = item.dataIndex;
          $("#hover_tooltip").remove();
          // Generate the diffs since latest_selection
          var diffs;
          if (latest_selection == null)
            diffs = {};
          else
            diffs = metadata_diff(get_metadata(latest_selection), get_metadata(item));
          // Display tooltip
          var contents = generate_tooltip(item, "hover_tooltip", diffs);
          show_tooltip(graph, item.pageX + 10, item.pageY, contents);
        }
      });
      if (typeof cb === "function") cb();
    });
  }

  // Called when starting a new plot, or when user clicks on Stop.
  function stop_plotting() {
    if (flot_object) flot_object.shutdown();
  }
}


function make_table(o) {
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
  $("#table .tablesorter").tablesorter();
}

function on_async_fail(XMLHttpRequest, textStatus, errorThrown) {
  console.log(XMLHttpRequest);
  console.log(textStatus);
  console.log(errorThrown);
}

