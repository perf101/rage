// ======== MAIN --- begin ===========
preselectFieldsBasedOnParams();
// reset configuration button
$("#reset_config").click(function() {window.location.href = getBaseUrl();});
// automatic refresh on change
$("select[name='xaxis']").change(refresh);
$("select[name='yaxis']").change(refresh);
$(".filterselect").change(refresh);
$(".multiselect").change(refresh);
$("#yaxis_log").change(refresh);
// draw immediately
refresh();
// extract image button
$('#get_img').click(get_image);
// ========= MAIN --- end ============

function preselectFieldsBasedOnParams() {
  var params = getUrlParams();
  delete params.som;
  for (var param in params)
    $("[name='" + param + "']").val(params[param]);
}

function getUrlParams() {
  var result = {};
  var href = window.location.href;
  var pairs = href.slice(href.indexOf('?') + 1).split('&');
  for(var i in pairs) {
    var pair = pairs[i].split('=');
    result[pair[0]] = pair[1].replace("+", " ");
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

function getBaseUrl() {
  var href = window.location.href;
  var origin = href.substring(0, href.lastIndexOf('/'));
  return origin + "/?som=" + getUrlParams().som;
}

// query server with current form settings and replot
function refresh() {
  $("#progress_img").toggle(true);
  var formData = $('form[name=optionsForm]').serialize();
  var baseUrl = getBaseUrl();
  var permalink = baseUrl + "&" + formData;
  $(".permalink").attr("href", permalink);
  var request = permalink + "&async=true&prototype=true";
  console.log(request);
  $.ajax({
    url: request,
    method: 'GET',
    dataType: 'json',
    success: onReceived,
    error: onAsyncFail,
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
  var labels = o[axis + "_labels"];
  var axis_options = options[axis + "axis"];
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

function showTooltip(x, y, contents) {
  $('<div id="tooltip">' + contents + '</div>').css({
    'top': y + 5, 'left': x + 5,
  }).appendTo("body").fadeIn(200);
}

var series = [];

function onReceived(o) {
  console.log("Received: ", o);
  //var data = o.data;
  var graph = $("#graph");
  // default options
  series = o.series;
  // averages
  var num_series = series.length;
  var i = 0;
  for (i = 0; i < num_series; i++) {
    var avgs = get_averages_for(series[i].data);
    series.push({color: series[i].color, data: avgs,
                 points: {show: false}, lines: {show: true}});
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
      canvasText: {show: true},
    },
    legend: {type: "canvas", backgroundColor: "white"},
    points: {show: true},
  };
  // labels
  var has_x_labels = has_labels(o, "x");
  var has_y_labels = has_labels(o, "y");
  if (has_x_labels) configure_labels(o, "x", options);
  if (has_y_labels) configure_labels(o, "y", options);
  // log scale
  if ($("#yaxis_log").is(":checked")) {
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
      var xl = has_x_labels ? o.x_labels[Math.floor(x)] : x;
      var yl = has_y_labels ? o.y_labels[Math.floor(y)] : y;
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
      showTooltip(item.pageX + 10, item.pageY, body);
    }
  });
  $("#progress_img").toggle(false);
}

function onAsyncFail(XMLHttpRequest, textStatus, errorThrown) {
  console.log(XMLHttpRequest);
  console.log(textStatus);
  console.log(errorThrown);
}

