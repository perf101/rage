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

// === GLOBAL VARIABLES --- start ===
var autofetch = true; // if false, the following triggers have no effect
var checkboxes_on_by_default = ["show_avgs", "y_from_zero"];
var graph_only_fields = [
  "#xaxis", "#yaxis", "#show_avgs", "#x_from_zero", "#y_from_zero",
  "#x_as_seq", "#y_as_seq", "#show_all_meta", "#xaxis_log", "#yaxis_log",
  "#get_img"
]
var url_params = get_url_params();
// ==== GLOBAL VARIABLES --- end ====

// ======== MAIN --- begin ===========
decode_all();
if ("som" in url_params)
  som_page_init();
else if ("report" in url_params)
  report_page_init();
else if ("report_generator" in url_params)
  report_generator_page_init();
// ========= MAIN --- end ============

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
  // automatic refresh on change
  $("select[name='xaxis']").change(fetch_data_and_process);
  $("select[name='yaxis']").change(fetch_data_and_process);
  $("input[name='show_avgs']").change(fetch_data_and_process);
  $("input[name='x_from_zero']").change(fetch_data_and_process);
  $("input[name='y_from_zero']").change(fetch_data_and_process);
  $("input[name='x_as_seq']").change(fetch_data_and_process);
  $("input[name='y_as_seq']").change(fetch_data_and_process);
  $("input[name='show_all_meta']").change(fetch_data_and_process);
  $("input[name='xaxis_log']").change(fetch_data_and_process);
  $("input[name='yaxis_log']").change(fetch_data_and_process);
  $(".filterselect").change(fetch_data_and_process);
  $(".multiselect").change(fetch_data_and_process);
  // fetch and process data immediately
  preselect_fields_based_on_params();
  fetch_data_and_process();
  // extract image button
  load_get_image_if_not_ie();
  // tiny url
  $("#get_tinyurl").click(get_tinyurl);
}

function report_page_init() {
  var report_id = parseInt(url_params.report[0]);
  console.log("report page", report_id);
  var request = window.location.href + "&async=true";
  console.log(request);
  $.ajax({
    url: request,
    method: 'GET',
    dataType: 'json',
    success: on_report_received,
    error: on_async_fail
  });
}

function get_options_for(name) {
  var options = $("select[name='" + name + "']").val();
  return options == null ? [] : options;
}

function on_report_generator_checkbox_change() {
  var som_id = parseInt($(this).attr("name").substring(12));
  $("#view_container_" + som_id).toggle(false);
  $("#configs_" + som_id).toggle(false);
  $("#points_" + som_id).toggle(false);
  if (!$(this).is(":checked")) return;
  var request = "/?som=" + som_id;
  var tc_fqn = $("#tc_of_" + som_id).html();
  var builds = [];
  var prim_std_builds = get_options_for("primary_standard_builds");
  if ($.inArray("ALL", prim_std_builds) != -1) {
    $("select[name='primary_standard_builds'] option").each(function() {
      var value = $(this).attr("value");
      builds.push(value);
    });
  } else builds = prim_std_builds;
  builds = builds.concat(get_options_for("primary_all_builds"));
  builds = $.grep(builds, function(v) {return v != "NONE" && v != "ALL";});
  for (var i in builds) request += "&v_build_number=" + builds[i];
  var num_configs = 1;
  $("select[name^='tc-" + tc_fqn + "']").each(function() {
    var name = $(this).attr("name");
    var config = name.substring(name.indexOf("_") + 1);
    var vals = $(this).val();
    if ($.inArray("ALL", vals) != -1)
      num_configs *= $(this).children().length - 1;
    else num_configs *= vals.length;
    for (var i in vals) request += "&v_" + config + "=" + vals[i];
  });
  $("select[name^='som-" + som_id + "']").each(function() {
    var name = $(this).attr("name");
    var config = name.substring(name.indexOf("_") + 1);
    var vals = $(this).val();
    if ($.inArray("ALL", vals) != -1)
      num_configs *= $(this).children().length - 1;
    else num_configs *= vals.length;
    for (var i in vals) request += "&v_" + config + "=" + vals[i];
  });
  $("#view_" + som_id).attr("href", request);
  $("#view_container_" + som_id).toggle(true);
  var field = $("#configs_" + som_id);
  field.html("(configs = " + num_configs + ")");
  field.toggle(true);
  request += "&target=" + som_id;
  request += "&async=true";
  console.log(request);
  $.ajax({
    url: request,
    method: 'GET',
    dataType: 'json',
    success: on_report_generator_checkbox_received,
    error: on_async_fail
  });
}

function on_report_generator_checkbox_received(o) {
  console.log(o);
  if (o == null) result = "error"
  else if (o.series.length == 0) result = 0;
  else result = o.series[0].data.length;
  var field = $("#points_" + o.target);
  field.html("(samples = " + result + ")");
  field.toggle(true);
}

function report_generator_page_init() {
  $("input[type='checkbox']").change(on_report_generator_checkbox_change);
  if (!("id" in url_params)) {
    enable_report_generator_triggers();
    return;
  }
  var report_id = parseInt(url_params.id[0]);
  var request = "/?report=" + report_id + "&async=true";
  console.log(request);
  $.ajax({
    url: request,
    method: 'GET',
    dataType: 'json',
    success: on_report_received_edit,
    error: on_async_fail
  });
}

function on_report_received_edit(o) {
  console.log(o);
  var report_id = url_params.id;
  var id_input = "<input type='hidden' name='id' value='" + report_id + "' />";
  $('input[name="report_create"]').after(id_input)
  $('input[name="desc"]').val(decode(o.desc));
  var primary_bns = Object.keys(extract_build_numbers(o.builds.primary));
  var secondary_bns = Object.keys(extract_build_numbers(o.builds.secondary));
  $('[name="primary_standard_builds"]').val(primary_bns);
  $('[name="primary_all_builds"]').val(primary_bns);
  $('[name="secondary_standard_builds"]').val(secondary_bns);
  $('[name="secondary_all_builds"]').val(secondary_bns);
  var tccs = {};  // <tc_fqn>_<tc_config_name> ==> <tc_config_value> ==> true
  var soms = {};  // <som_id> ==> true
  var somcs = {}; // <som_id>_<som_config_name> ==> <som_config_value> ==> true
  for (var i in o.configs) {
    var c = o.configs[i];
    var som_id = c.som_id;
    soms[som_id] = true;
    var tc_fqn = c.tc_fqn;
    var tc_config = c.tc_config;
    for (var tcc_k in tc_config) {
      var tcc_v = tc_config[tcc_k];
      var tcc_fqn = tc_fqn + "_" + tcc_k;
      if (!(tcc_fqn in tccs)) tccs[tcc_fqn] = {};
      tccs[tcc_fqn][tcc_v] = true;
    }
    if ("som_config" in c) {
      var som_config = c.som_config;
      for (var sc_k in som_config) {
        var sc_v = som_config[sc_k];
        var sc_fqn = som_id + "_" + sc_k;
        if (!(sc_fqn in somcs)) somcs[sc_fqn] = {};
        somcs[sc_fqn][sc_v] = true;
      }
    }
  }
  for (var tcc in tccs)
    $('[name="tc-' + tcc + '"]').val(Object.keys(tccs[tcc]));
  for (var sc in somcs)
    $('[name="som-' + sc + '"]').val(Object.keys(somcs[sc]));
  for (var som_id in soms)
    $("input[name='include_som_" + som_id + "']").prop("checked", true).change();
  enable_report_generator_triggers();
}

function enable_report_generator_triggers() {
  var update_all_soms = function() {
    $("input[name^='include_som_']").each(function() {
      if ($(this).is(":checked")) $(this).change();
    });
  }
  $("select[name='primary_standard_builds']").change(update_all_soms);
  $("select[name='primary_all_builds']").change(update_all_soms);
  $("select[name^='tc-']").change(function() {
    var name = $(this).attr("name");
    var caller = name.substring(3, name.indexOf("_"));
    $("input[name^='include_som_']").each(function() {
      if (!$(this).is(":checked")) return;
      var som_id = $(this).attr("name").substring(12);
      var tc_fqn = $("#tc_of_" + som_id).html();
      if (caller == tc_fqn) $(this).change();
    });
  });
  $("select[name^='som-']").change(function() {
    var name = $(this).attr("name");
    var som_id = name.substring(4, name.indexOf("_"));
    var checkbox = $("input[name='include_som_" + som_id + "']");
    if (checkbox.is(":checked")) checkbox.change();
  });
}

function get_table_for(o) {
  if (o.length == 0) return "N/A";
  var s = "<table border='1'>";
  var ks = get_sorted_keys(o[0]);
  s += "<tr>";
  for (var k in ks)
    s += "<th>" + ks[k] + "</th>";
  for (var i in o) {
    s += "<tr>";
    for (var k in ks)
      s += "<td>" + o[i][ks[k]] + "</td>";
    s += "</tr>";
  }
  s += "</tr>";
  s += "</table>";
  return s;
}

function get_config_list(desc, o) {
  var s = "";
  for (var k in o)
    s += desc + " config '" + k + "': " + o[k] + "<br />";
  return s;
}

function builds_for(builds) {
  var s = "";
  for (var i in builds)
    s += "&v_build_number=" + builds[i].build_number;
  return s;
}

function string_to_polarity(s) {
  if (s == "t") return "More is better.";
  if (s == "f") return "Less is better.";
  if (s == "NULL") return "(Not set.)";
  return "(Corrupt: " + s + ")"
}

function string_to_units(s) {
  if (s == "NULL") return "(Not set.)";
  return s;
}

function extract_build_numbers(builds) {
  var build_numbers = {};
  for (var i in builds)
    build_numbers[builds[i].build_number] = true;
  return build_numbers;
}

var report_primary_builds = {}

function on_report_received(r) {
  console.log(r);
  // store primary builds globally
  report_primary_builds = extract_build_numbers(r.builds.primary);
  // basic metadata
  var s = "";
  s += "<h2>Report ID</h2>" + r.id;
  s += "<h2>Report description</h2>" + r.desc;
  // builds
  s += "<h2>Primary builds</h2>";
  s += get_table_for(r.builds.primary);
  s += "<h2>Secondary builds</h2>";
  s += get_table_for(r.builds.secondary);
  s += "<input type='checkbox' name='y_from_zero' ";
  s += "checked='checked' style='display: none' />";
  $('body').append(s);
  // configs
  var builds = builds_for(r.builds.primary) + builds_for(r.builds.secondary);
  for (var i in r.configs) {
    var c = r.configs[i];
    var part = 1 + parseInt(i);
    var som_config_id_str = c.som_config_id == -1 ? "N/A" : c.som_config_id;
    // print info
    s = "";
    s += "<h3>Part " + part + "</h3>";
    s += "TC fqn: " + c.tc_fqn + "<br />";
    s += "TC description: " + c.tc_desc + "<br />";
    s += "TC configuration ID: " + c.tc_config_id + "<br />";
    s += get_config_list("TC", c.tc_config);
    s += "SOM ID: " + c.som_id + "<br />";
    s += "SOM name: " + c.som_name + "<br />";
    s += "SOM configuration ID: " + som_config_id_str + "<br />";
    s += get_config_list("SOM", c.som_config);
    s += "SOM polarity: " + string_to_polarity(c.som_polarity) + "<br />";
    s += "SOM units: " + string_to_units(c.som_units) + "<br />";
    var graph_id = "graph_" + part;
    var graph_style = "width: 1000px; height: 600px";
    s += "<div id='" + graph_id + "' style='" + graph_style + "'></div>";
    $('body').append(s);
    // fetch data
    var request = "/";
    request += "?som=" + c.som_id;
    request += "&xaxis=build_number";
    request += "&v_tc_config_id=" + c.tc_config_id;
    if (c.som_config_id != -1)
      request += "&v_som_config_id=" + c.som_config_id;
    request += builds;
    request += "&target=" + graph_id;
    request += "&show_all_meta=on";
    request += "&async=true";
    console.log(request);
    $.ajax({
      url: request,
      method: 'GET',
      dataType: 'json',
      success: on_report_part_received,
      error: on_async_fail
    });
  }
  // DOM generation
}

function on_report_part_received(o) {
  console.log(o);
  var primary_builds = $.extend({}, report_primary_builds);
  if (o.series.length > 0) {
    var points = o.series[0].data;
    for (var i in points) {
      if (Object.keys(primary_builds).length == 0) break;
      delete primary_builds[points[i][0]];
    }
  }
  if (Object.keys(primary_builds).length == 0)
    draw_graph(o);
  else {
    var target = $("#" + o.target);
    target.toggle(false);
    var s = "<p class='error'>";
    s += "NO DATA FOR PRIMARY BUILDS: ";
    s += Object.keys(primary_builds).join(", ");
    s += ".</p>";
    $(s).insertAfter(target);
  }
}

function view_change() {
  autofetch = false;
  var view = $('#view').val();
  var is_graph = (view == "Graph");
  for (var i in graph_only_fields)
    $(graph_only_fields[i]).toggle(is_graph);
  $("input[name='show_avgs']").prop("checked", is_graph);
  $("input[name='show_all_meta']").prop("checked", !is_graph);
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
    if (typeof(pair[1]) === 'undefined') pair[1] = "";
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
    error: on_async_fail
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
    success: on_received,
    error: on_async_fail
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

function on_received(o) {
  console.log("Received: ", o);
  var view = $('#view').val();
  if (view == "Graph") {
    $('#table').hide();
    $('#graph').show();
    draw_graph(o, on_plotting_finished);
  } else {
    if (view == "Table") {
      $('#graph').hide();
      $('#table').show();
      make_table(o);
    } else console.log("Unknown view.");
    on_plotting_finished();
  }
}

var last_received_graph_data = {};
var flot_object = null;
var series = [];
var num_series = 0;

function draw_graph(o, cb) {
  stop_plotting();
  last_received_graph_data = o;
  var graph = $("#" + o.target);
  // default options
  series = o.series;
  num_series = series.length;
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
      clickable: true,
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
  if ($("input[name='xaxis_log']").is(":checked")) {
    options.xaxis.transform = safe_log;
    options.xaxis.inverseTransform = Math.exp;
    options.xaxis.ticks = create_log_ticks;
  }
  if ($("input[name='yaxis_log']").is(":checked")) {
    options.yaxis.transform = safe_log;
    options.yaxis.inverseTransform = Math.exp;
    options.yaxis.ticks = create_log_ticks;
  }
  $("#stop_plotting").prop("disabled", false);
  var start = new Date();
  flot_object = $.plot(graph, series, options, function() {
    console.log("Plotting took " + (new Date() - start) + "ms.");
    $("#stop_plotting").prop("disabled", true);
    // click
    graph.unbind("plotclick");
    graph.bind("plotclick", function (event, pos, item) {
      if (!item) return;
      show_tooltip(graph, item.pageX + 10, item.pageY, generate_tooltip(item));
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
        var contents = generate_tooltip(item, "hover_tooltip");
        show_tooltip(graph, item.pageX + 10, item.pageY, contents);
      }
    });
    cb();
  });
}

function on_stop_plotting() {
  stop_plotting();
  on_plotting_finished();
}

function stop_plotting() {
  if (flot_object) flot_object.shutdown();
}

function on_plotting_finished() {
  $("#progress_img").toggle(false);
}

var tooltip_counter = 0;

function generate_tooltip(item, id) {
  var click_tooltip = typeof id === 'undefined';
  id = typeof id === 'undefined' ? "tooltip_" + tooltip_counter++ : id;
  var body = "<div id='" + id + "' class='tooltip'><table>";
  var o = last_received_graph_data;
  var x = item.datapoint[0].toFixed(2);
  var y = item.datapoint[1].toFixed(2);
  var xl = has_labels(o, "x") ? o.x_labels[Math.floor(x)] : x;
  var yl = has_labels(o, "y") ? o.y_labels[Math.floor(y)] : y;
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
  if (click_tooltip)
    body += "<img src='/close.png' />";
  body += "</div>";
  return body;
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

