/*
Invariants (also reflected on server side):
- Default value for field "xaxis" is "branch".
- Default value for field "yaxis" is "result".
- Show averages ("show_avgs") is selected by default.
- Y minimum set to zero ("y_from_zero") is selected by default.
- All other checkboxees are not selected by default.
- "SHOW FOR" is the first (default) option for filters ("f_").
- "ALL" is the first (default) option for filter values ("v_").
*/

// ======== MAIN --- begin ===========
// automatically change available configuration given view
$("#view").change(view_change);
// reset configuration button
$("#reset_config").click(function() {window.location.href = get_som_url();});
// automatic refresh on change
var autofetch = true; // if false, the following triggers have no effect
$("select[name='xaxis']").change(fetch_data_and_process);
$("select[name='yaxis']").change(fetch_data_and_process);
$("input[name='show_avgs']").change(fetch_data_and_process);
$("input[name='x_from_zero']").change(fetch_data_and_process);
$("input[name='y_from_zero']").change(fetch_data_and_process);
$("input[name='show_all_meta']").change(fetch_data_and_process);
$("input[name='yaxis_log']").change(fetch_data_and_process);
$(".filterselect").change(fetch_data_and_process);
$(".multiselect").change(fetch_data_and_process);
// fetch and process data immediately
var checkboxes_on_by_default = ["show_avgs", "y_from_zero"];
preselect_fields_based_on_params();
fetch_data_and_process();
// extract image button
load_get_image_if_not_ie();
// tiny url
$("#get_tinyurl").click(get_tinyurl);
// ========= MAIN --- end ============

var graph_only_fields = [
  "#xaxis", "#yaxis", "#show_avgs", "#x_from_zero", "#y_from_zero",
  "#show_all_meta", "#yaxis_log", "#get_img"
]

function view_change() {
  autofetch = false;
  var view = $('#view').val();
  var is_graph = (view == "Graph");
  for (var i in graph_only_fields)
    $(graph_only_fields[i]).toggle(is_graph);
  $("input[name='show_avgs']").prop("checked", is_graph);
  $("input[name='show_all_meta']").prop("checked", !is_graph);
  $("input[name='yaxis_log']").prop("checked", false);
  autofetch = true;
  fetch_data_and_process();
}

function on_by_default(name) {
  return $.inArray(name, checkboxes_on_by_default) >= 0;
}

function preselect_fields_based_on_params() {
  var params = get_url_params();
  delete params.som;
  for (var param in params)
    $("[name='" + param + "']").val(params[param]);
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
    if (!result[pair[0]]) result[pair[0]] = [];
    result[pair[0]].push(pair[1].replace(/\+/g, " "));
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
    data: {action: "CreateTiny", url: get_permalink()},
    method: 'POST',
    dataType: 'json',
    success: function(data) {
      console.log(data);
      var tiny_url = get_base_url() + "/?t=" + data.id;
      $("#tinyurl").attr("href", tiny_url);
      $("#tinyurl").html(tiny_url);
      $("#tinyurl").toggle(true);
    },
    error: onAsyncFail
  });
}

function get_base_url() {
  var href = window.location.href;
  return href.substring(0, href.lastIndexOf('/'));
}

function get_som_url() {
  return get_base_url() + "/?som=" + get_url_params().som;
}

function serialise_params(params) {
  var keyvals = [];
  for (var p in params) {
    var v = params[p];
    for (var i in v)
      keyvals.push(p + "=" + v[i]);
  }
  return keyvals.join("&");
}

function get_permalink() {
  var form_data = $('form[name=optionsForm]').serialize();
  var params = extract_params(form_data);
  $.each($('form[name=optionsForm] input[type=checkbox]'), function(i, cb) {
    if (!on_by_default(cb.name)) return;
    if (cb.name in params) delete params[cb.name];
    else params[cb.name] = ["off"];
  });
  var minimised = {};
  for (var p in params) {
    var v = params[p];
    var l = v.length;
    var f = v[0];
    var is_xaxis_branch = p == "xaxis" && f == "branch";
    var is_yaxis_result = p == "yaxis" && f == "result";
    var is_show_for = p.indexOf("f_") == 0 && f == "0";
    var is_all_only = p.indexOf("v_") == 0 && l == 1 && f == "ALL";
    if (!(is_xaxis_branch || is_yaxis_result || is_show_for || is_all_only))
      minimised[p] = params[p];
  }
  var serialised = serialise_params(minimised);
  console.log(serialised);
  if (serialised != "") serialised = "&" + serialised;
  return get_som_url() + serialised;
}

function fetch_data_and_process() {
  if (!autofetch) return;
  $("#tinyurl").toggle(false);
  $("#progress_img").toggle(true);
  var request = get_permalink() + "&async=true";
  console.log(request);
  $.ajax({
    url: request,
    method: 'GET',
    dataType: 'json',
    success: onReceived,
    error: onAsyncFail
  });
}

function get_sorted_keys(o) {
  var keys = []
  for (var k in o) keys.push(k);
  keys.sort(function(a, b) {return a - b;});
  return keys;
}

function get_averages_for(series) {
  var sum_map = {};
  var count_map = {};
  for (i in series) {
    var point = series[i];
    var x = point[0];
    if (!(x in sum_map)) {sum_map[x] = 0; count_map[x] = 0;}
    sum_map[x] += point[1];
    count_map[x] += 1;
  }
  var result = [];
  var sorted_keys = get_sorted_keys(sum_map);
  for (i in sorted_keys) {
    var key = sorted_keys[i];
    result.push([key, sum_map[key] / count_map[key]]);
  }
  return result;
}

function has_labels(o, axis) {
  return (axis + "_labels") in o;
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

function show_tooltip(x, y, contents) {
  $('<div id="tooltip">' + contents + '</div>').css({
    'top': y + 5, 'left': x + 5
  }).appendTo("body").fadeIn(200);
}

function onReceived(o) {
  console.log("Received: ", o);
  var view = $('#view').val();
  if (view == "Graph") {
    $('#table').hide();
    drawGraph(o);
    $('#graph').show();
  } else if (view == "Table") {
    $('#graph').hide();
    makeTable(o);
    $('#table').show();
  } else console.log("Unknown view.");
  $("#progress_img").toggle(false);
}

var series = [];

function drawGraph(o) {
  var graph = $("#graph");
  // default options
  series = o.series;
  var num_series = series.length;
  // averages
  if ($("input[name='show_avgs']").is(":checked")) {
    var i = 0;
    for (i = 0; i < num_series; i++) {
      var avgs = get_averages_for(series[i].data);
      series.push({color: series[i].color, data: avgs,
                   points: {show: false}, lines: {show: true}});
    }
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
    xaxis: {labelAngle: 285},
    yaxis: {},
    grid: {
      hoverable: true,
      canvasText: {show: true}
    },
    legend: {type: "canvas", backgroundColor: "white"},
    points: {show: true}
  };
  // force X or Y from 0
  if ($("input[name='x_from_zero']").is(":checked"))
    options.xaxis.min = 0;
  if ($("input[name='y_from_zero']").is(":checked"))
    options.yaxis.min = 0;
  // labels
  configure_labels(o, "x", options);
  configure_labels(o, "y", options);
  // log scale
  if ($("input[name='yaxis_log']").is(":checked")) {
    options.yaxis.transform = safe_log;
    options.yaxis.inverseTransform = Math.exp;
    options.yaxis.ticks = create_log_ticks;
  }
  var plot = $.plot(graph, series, options);
  // hover
  var previousPoint = null;
  graph.bind("plothover", function (event, pos, item) {
    if (!item) {
      $("#tooltip").remove();
      previousPoint = null;
    } else if (previousPoint != item.dataIndex) {
      previousPoint = item.dataIndex;
      $("#tooltip").remove();
      var x = item.datapoint[0].toFixed(2);
      var y = item.datapoint[1].toFixed(2);
      var xl = has_labels(o, "x") ? o.x_labels[Math.floor(x)] : x;
      var yl = has_labels(o, "y") ? o.y_labels[Math.floor(y)] : y;
      var body = "<table>";
      var label = "";
      if ("label" in item.series)
        label = item.series.label;
      else if (item.seriesIndex >= num_series) {
        var s = series[item.seriesIndex - num_series];
        if ("label" in s) label = s.label + " (mean)";
      }
      if (label)
        body += "<tr><th>series:</th><td>" + label + "</td></tr>";
      body += "<tr><th>x:</th><td>" + xl + "</td></tr>";
      body += "<tr><th>y:</th><td>" + yl + "</td></tr>";
      var itemData = item.series.data[item.dataIndex];
      if (2 in itemData) {
        var props = itemData[2];
        for (p in props)
          body += "<tr><th>" + p + ":</th><td>" + props[p] + "</td></tr>";
      }
      body += "</table>";
      show_tooltip(item.pageX + 10, item.pageY, body);
    }
  });
}

function makeTable(o) {
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

function onAsyncFail(XMLHttpRequest, textStatus, errorThrown) {
  console.log(XMLHttpRequest);
  console.log(textStatus);
  console.log(errorThrown);
}

