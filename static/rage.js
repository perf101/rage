/*
Invariants (also reflected on server side):
- Default value for field "xaxis" is "branch".
- Default value for field "yaxis" is "result".
- Show points ("show_points") is selected by default.
- Show averages ("show_avgs") is selected by default.
- Y minimum set to zero ("y_fromto_zero") is selected by default.
- All other checkboxees are not selected by default.
- "SHOW FOR" is the first (default) option for filters ("f_").
- "ALL" is the first (default) option for filter values ("v_").
*/

// === GLOBAL VARIABLES --- start ===
var autofetch = true; // if false, the following triggers have no effect
var checkboxes_on_by_default = ["show_points", "show_avgs", "y_fromto_zero"];
var graph_only_fields = [
  "#xaxis", "#yaxis", "#show_points", "#show_avgs", "#x_from_zero", "#y_fromto_zero",
  "#x_as_seq", "#y_as_seq", "#show_all_meta", "#xaxis_log", "#yaxis_log",
  "#legend_position", "#get_img"
]
var url_params = get_url_params();
// ==== GLOBAL VARIABLES --- end ====

// ======== MAIN --- begin ===========
decode_all();
if ("som" in url_params)
  som_page_init();
else if ("somsbytc" in url_params)
  soms_by_tc_page_init();
else if ("report" in url_params)
  report_page_init();
else if ("report_generator" in url_params)
  report_generator_page_init();
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
  // automatic refresh on change
  $("select[name='xaxis']").change(fetch_data_and_process);
  $("select[name='yaxis']").change(fetch_data_and_process);
  $("input[name='show_points']").change(fetch_data_and_process);
  $("input[name='show_avgs']").change(fetch_data_and_process);
  $("input[name='x_from_zero']").change(fetch_data_and_process);
  $("input[name='y_fromto_zero']").change(fetch_data_and_process);
  $("input[name='x_as_seq']").change(fetch_data_and_process);
  $("input[name='y_as_seq']").change(fetch_data_and_process);
  $("input[name='show_all_meta']").change(fetch_data_and_process);
  $("input[name='xaxis_log']").change(fetch_data_and_process);
  $("input[name='yaxis_log']").change(fetch_data_and_process);
  $("select[name='legend_position']").change(fetch_data_and_process);
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

function soms_by_tc_page_init() {
  $.ajax({
    url: get_base_url() + "/?somsasync",
    type: 'GET',
    dataType: 'json',
    success: on_soms_by_tc_received,
    error: on_async_fail
  });
}

function on_soms_by_tc_received(o) {
  console.log(o);
  var tc_to_soms = {};
  $.each(o.soms, function(som_id, som) {
    if (!(som.tc in tc_to_soms)) tc_to_soms[som.tc] = [];
    tc_to_soms[som.tc].push(som_id);
  });
  var base_url = get_base_url();
  var s = "";
  $.each(o.tcs, function(tc_fqn, tc) {
    s += "<h2>" + tc_fqn + " (" + tc.desc + ")</h2>";
    s += "<ul>";
    $.each(tc_to_soms[tc_fqn], function(i, som_id) {
      var som_url = base_url + "/?som=" + som_id;
      var som_caption = som_id + " (" + o.soms[som_id].name + ")";
      s += "<li><a href='" + som_url + "'>" + som_caption + "</a></li>";
    });
    s += "</ul>";
  });
  $("body").append(s);
}

function report_page_init() {
  var report_id = parseInt(url_params.report[0]);
  var request = window.location.href + "&async=true";
  console.log("Request:", request);
  $.ajax({
    url: request,
    type: 'GET',
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
  determine_report_axes_possibilities();
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
    if (endsWith(name, "_split")) return;
    var config = name.substring(name.indexOf("_") + 1);
    var vals = $(this).val();
    if ($.inArray("ALL", vals) != -1)
      num_configs *= $(this).children().length - 1;
    else num_configs *= vals.length;
    for (var i in vals) request += "&v_" + config + "=" + vals[i];
  });
  $("select[name^='som-" + som_id + "']").each(function() {
    var name = $(this).attr("name");
    if (endsWith(name, "_split")) return;
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
    type: 'GET',
    dataType: 'json',
    success: on_report_generator_checkbox_received,
    error: on_async_fail
  });
}

function determine_report_axes_possibilities() {
  var processed = 0;
  var common_configs = {};
  // go through all selected checkboxes
  $("input[name^='include_som_']").each(function() {
    if (!$(this).is(":checked")) return;
    var som_id = parseInt($(this).attr("name").substring(12));
    var tc_fqn = $("#tc_of_" + som_id).html();
    // get union of tc and som configs
    var tc_som_configs = {};
    $("select[name^='tc-" + tc_fqn + "']").each(function() {
      var name = $(this).attr("name");
      var config = name.substring(name.indexOf("_") + 1);
      tc_som_configs[config] = true;
    });
    $("select[name^='som-" + som_id + "']").each(function() {
      var name = $(this).attr("name");
      var config = name.substring(name.indexOf("_") + 1);
      tc_som_configs[config] = true;
    });
    // intersect union with result (if none unioned, adopt set)
    if (processed++ > 0)
      for (var c in tc_som_configs)
        if (!(c in common_configs))
          delete tc_som_configs[c];
    common_configs = tc_som_configs;
  });
  var configs = Object.keys(common_configs);
  // set values in xaxis and yaxis
  update_axes("x", configs);
  update_axes("y", configs);
}

function update_axes(axis, extra_axes) {
  var axis_sel = $("select[name='" + axis + "axis']");
  var axis_val = axis_sel.val();
  axis_sel.empty();
  var std_axes = last_axes_received["std_" + axis + "_axes"];
  var axes = std_axes.concat(extra_axes);
  set_select_options(axis_sel, axes);
  if ($.inArray(axis_val, axes)) axis_sel.val(axis_val);
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

var last_axes_received;

function report_generator_page_init() {
  $("input[type='checkbox']").change(on_report_generator_checkbox_change);
  $.ajax({
    url: "/?axesasync",
    type: 'GET',
    dataType: 'json',
    success: function(axes) {
      last_axes_received = axes;
      set_select_options($('select[name="xaxis"]'), axes.std_x_axes);
      set_select_options($('select[name="yaxis"]'), axes.std_y_axes);
      report_generator_page_init_stage_two();
    },
    error: on_async_fail
  });
}

function report_generator_page_init_stage_two() {
  if (!("id" in url_params)) {
    enable_report_generator_triggers();
    return;
  }
  var report_id = parseInt(url_params.id[0]);
  var request = "/?report=" + report_id + "&async=true";
  console.log(request);
  $.ajax({
    url: request,
    type: 'GET',
    dataType: 'json',
    success: on_report_received_edit,
    error: on_async_fail
  });
  $("input[type='submit']").val("Update Report");
}

var last_report_received;

function set_select_options(sel_obj, options) {
  $.each(options, function(i, e) {
     sel_obj.append($("<option></option>").attr("value", e).text(e));
  });
}

// the specified option is added if not present
function select_option(sel_obj, option) {
  var present = false;
  sel_obj.children("option").each(function() {
    if ($(this).val() == option) {
      present = true;
      return false;
    }
  });
  if (!present)
     sel_obj.append($("<option></option>").attr("value", option).text(option));
  sel_obj.val(option);
}

function on_report_received_edit(o) {
  console.log(o);
  last_report_received = o;
  var report_id = url_params.id;
  var id_input = "<input type='hidden' name='id' value='" + report_id + "' />";
  $('input[name="report_create"]').after(id_input)
  $('input[name="desc"]').val(decode(o.desc));
  select_option($('select[name="xaxis"]'), decode(o.xaxis));
  select_option($('select[name="yaxis"]'), decode(o.yaxis));
  var primary_bns = Object.keys(extract_build_numbers(o.builds.primary));
  var secondary_bns = Object.keys(extract_build_numbers(o.builds.secondary));
  $('[name="primary_standard_builds"]').val(primary_bns);
  $('[name="primary_all_builds"]').val(primary_bns);
  $('[name="secondary_standard_builds"]').val(secondary_bns);
  $('[name="secondary_all_builds"]').val(secondary_bns);
  var tccs = {};   // <tc_fqn>_<tc_config_name> ==> <tc_config_value> ==> true
  var soms = {};   // <som_id> ==> true
  var somcs = {};  // <som_id>_<som_config_name> ==> <som_config_value> ==> true
  var splits = {}; // <split_id> ==> <type>
  $.each(o.plots, function(i, p) {
    var p = o.plots[i];
    var som_id = p.som_id;
    soms[som_id] = true;
    var tc_fqn = p.tc_fqn;
    $.each(p.tc_configs, function(tc_config_id, tc_config) {
      $.each(tc_config, function(tcc_k, tcc_v) {
        var tcc_fqn = tc_fqn + "_" + tcc_k;
        if (!(tcc_fqn in tccs)) tccs[tcc_fqn] = {};
        tccs[tcc_fqn][tcc_v] = true;
      });
    });
    $.each(p.som_configs, function(som_config_id, som_config) {
      $.each(som_config, function(sc_k, sc_v) {
        var sc_fqn = som_id + "_" + sc_k;
        if (!(sc_fqn in somcs)) somcs[sc_fqn] = {};
        somcs[sc_fqn][sc_v] = true;
      });
    });
    $.each(p.split_bys, function(property, ty) {
      if (property.indexOf("tc-") == 0) return;
      $('[name="' + property + '_split"]').val("split_by_" + ty);
    });
  });
  for (var tcc in tccs)
    $('[name="tc-' + tcc + '"]').val(Object.keys(tccs[tcc]));
  for (var sc in somcs)
    $('[name="som-' + sc + '"]').val(Object.keys(somcs[sc]));
  for (var som_id in soms)
    $("input[name='include_som_" + som_id + "']")
      .prop("checked", true).change();
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

var report_primary_build_numbers = {}

function on_report_received(r) {
  console.log("Response:", r);
  last_report_received = r;
  // store primary builds globally
  report_primary_build_numbers = extract_build_numbers(r.builds.primary);
  // basic metadata
  var s = "";
  s += "<h2>Basic information</h2>"
  s += "ID: " + r.id + "<br />";
  s += "Description: " + r.desc + "<br />";
  s += "X axis: " + r.xaxis + "<br />";
  s += "Y axis: " + r.yaxis + "<br />";
  // builds
  s += "<h2>Primary builds</h2>";
  s += get_table_for(r.builds.primary);
  s += "<h2>Secondary builds</h2>";
  s += get_table_for(r.builds.secondary);
  s += "<input type='checkbox' name='y_fromto_zero' ";
  s += "checked='checked' style='display: none' />";
  $('body').append(s);
  // process report parts iteratively
  process_report_part(0);
}

function process_report_part(i) {
  var r = last_report_received;
  if (r.plots.length <= i) return;
  var p = r.plots[i];
  var part = 1 + parseInt(i);
  // print info
  s = "";
  s += "<h3>Part " + part + "</h3>";
  s += "TC fqn: " + p.tc_fqn + "<br />";
  s += "TC description: " + p.tc_desc + "<br />";
  s += "TC configuration IDs: " + Object.keys(p.tc_configs) + "<br />";
  s += "SOM ID: " + p.som_id + "<br />";
  s += "SOM name: " + p.som_name + "<br />";
  s += "SOM configuration IDs: " + Object.keys(p.som_configs) + "<br />";
  s += "SOM polarity: " + string_to_polarity(p.som_polarity) + "<br />";
  s += "SOM units: " + string_to_units(p.som_units) + "<br />";
  s += "Split by property/ies: " + p.split_bys + "<br />";
  // s += get_config_list("TC", p.tc_config);
  // s += get_config_list("SOM", p.som_config);
  var graph_id = "graph_" + part;
  var graph_style = "width: 1000px; height: 600px";
  s += "<div id='" + graph_id + "' style='" + graph_style + "'></div>";
  $('body').append(s);
  // build request and fetch data
  var get_build_numbers = function(builds) {
    return $.map(r.builds.primary, function(b) {return b.build_number;});
  };
  var request = "/?som=" + p.som_id + "&async=true";
  var params = {
    f_machine_type: 1,
    part: i,
    show_all_meta: "on",
    target: graph_id,
    v_build_number: get_build_numbers(r.builds.primary + r.builds.secondary),
    v_som_config_id: Object.keys(p.som_configs),
    v_tc_config_id: Object.keys(p.tc_configs),
    xaxis: r.xaxis,
    yaxis: r.yaxis,
  };
  for (var property in p.split_bys)
    if (p.split_bys[property] == "line")
      params["f_" + property] = 1;
  console.log("Request:", request, params);
  $.ajax({
    url: request,
    type: 'POST',
    data: params,
    dataType: 'json',
    success: on_report_part_received,
    error: on_async_fail
  });
}

function on_report_part_received(o) {
  console.log("Response:", o);
  // check if payload includes data for primary builds
  // (currently, we only check against the first series)
  var primary_build_numbers = $.extend({}, report_primary_build_numbers);
  var selector = function(p) {return p[2].build_number;};
  if (o.xaxis == "build_number") selector = function(p) {return p[0];};
  if (o.yaxis == "build_number") selector = function(p) {return p[1];};
  if (o.series.length > 0) {
    var points = o.series[0].data;
    for (var i in points) {
      if (Object.keys(primary_build_numbers).length == 0) break;
      delete primary_build_numbers[selector(points[i])];
    }
  }
  if (Object.keys(primary_build_numbers).length == 0) {
    var graph = new GraphObject();
    graph.draw_graph(o, null);
  } else {
    var target = $("#" + o.target);
    target.toggle(false);
    var s = "<p class='error'>";
    s += "NO DATA FOR PRIMARY BUILDS: ";
    s += Object.keys(primary_build_numbers).join(", ");
    s += ".</p>";
    $(s).insertAfter(target);
  }
  process_report_part(o.part + 1);
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
    var v = params[p];
    var l = v.length;
    var f = v[0];
    var is_xaxis_branch = p == "xaxis" && f == "branch";
    var is_yaxis_result = p == "yaxis" && f == "result";
    var is_show_for = p.indexOf("f_") == 0 && f == "0";
    var is_all_only = p.indexOf("v_") == 0 && l == 1 && f == "ALL";
    var is_legend_position_ne = p == "legend_position" && f == "ne";
    if (!(is_xaxis_branch || is_yaxis_result || is_show_for || is_all_only
        || is_legend_position_ne))
      minimised[p] = params[p];
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

function fetch_data_and_process() {
  if (!autofetch) return;
  $("#tinyurl").toggle(false);
  $("#progress_img").toggle(true);
  var som_id = url_params.som[0];
  var request = "/?som=" + som_id + "&async=true";
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

function get_sorted_keys(o) {
  var keys = []
  for (var k in o) keys.push(k);
  keys.sort(function(a, b) {return a - b;});
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


var tooltip_counter = 0;

function GraphObject() {
  this.draw_graph = draw_graph;
  this.stop_plotting = stop_plotting;

  var graph_data = {};
  var flot_object = null;
  var series = [];
  var num_series = 0;

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

  function generate_tooltip(item, id) {
    var click_tooltip = typeof id === 'undefined';
    id = typeof id === 'undefined' ? "tooltip_" + tooltip_counter++ : id;
    var body = "<div id='" + id + "' class='tooltip'><table>";
    var o = graph_data;
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

  function draw_graph(o, cb) {
    stop_plotting();
    graph_data = o;
    var graph = $("#" + o.target);
    // default options
    point_series = o.series;
    num_series = point_series.length;
    if ($("input[name='show_points']").is(":checked")) {
      series = point_series;
    } else {
      series = [];
    }
    // averages
    if ($("input[name='show_avgs']").is(":checked")) {
      var i = 0;
      for (i = 0; i < num_series; i++) {
        var avgs = get_averages_for(point_series[i].data);
        series.push({color: point_series[i].color, data: avgs,
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
      legend: {
        type: "canvas",
        backgroundColor: "white",
        position: $("select[name='legend_position']").val()
      },
      points: {show: true}
    };
    // force X from 0
    if ($("input[name='x_from_zero']").is(":checked"))
      options.xaxis.min = 0;
    // force Y from/to 0
    if ($("input[name='y_fromto_zero']").is(":checked"))
      options.yaxis[o.positive ? "min" : "max"] = 0;
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

