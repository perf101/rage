function flot_graph (series, o, cb) {
    var graph = $("#" + o.target);
    // HTML graph labels
    graph.siblings(".xaxis").html(o.xaxis);
    // use the name of the SOM rather than "result"
    graph.siblings(".yaxis").html((o.yaxis == "result") ? yaxis = $("span[class='som_name']").text() : o.yaxis);
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
    var symbol = $("select[name='symbol']").val().toLowerCase();
    var options = {
        xaxis: { axisLabel: o.xaxis, labelAngle: 285 },
        yaxis: { axisLabel: o.yaxis },
        grid: {
            clickable: true,
            hoverable: true,
            canvasText: { show: true }
        },
        legend: {
            type: "canvas",
            backgroundColor: "white",
            position: $("select[name='legend_position']").val(),
        },
        points: {show: true, symbol: symbol}
    };

    function is_checked(cb_name) {
        return $("input[name='" + cb_name + "']").is(":checked");
    }

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
    var start = new Date();

    return flot_object = $.plot(graph, series, options, function() {
        // click
        graph.unbind("plotclick");
        var latest_selection = null;
        graph.bind("plotclick", function (event, pos, item) {
            if (!item) return;
            show_tooltip(graph, item.pageX + 10, item.pageY, generate_tooltip(o, item));
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
                if (latest_selection == null) {
                    diffs = {};
                } else {
                    diffs = metadata_diff(get_metadata(o, latest_selection), get_metadata(o, item));
                }
                // Display tooltip
                var contents = generate_tooltip(o, item, "hover_tooltip", diffs);
                show_tooltip(graph, item.pageX + 10, item.pageY, contents);
            }
        });
        if (typeof cb === "function") cb(new Date() - start);
    });
}

function configure_labels(o, axis, options) {
    var axis_options = options[axis + "axis"];
    var quantity = o[axis + "axis"];
    if (quantity == "build_number") {
        axis_options.ticks = 10;
        axis_options.tickDecimals = 0;
    }
    if (!o[axis + "_labels"]) return;
    var labels = o[axis + "_labels"];
    axis_options.min = 1;
    axis_options.tickFormatter = function(val, axis) {
        return (val in labels) ? labels[val] : '';
    };
    axis_options.tickSize = 1;
}

function safe_log(x) {
    if (x <= 0) x = 0.0001;
    return Math.log(x);
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

function get_metadata(o, item) {
    var x = item.datapoint[0].toFixed(2);
    var y = item.datapoint[1].toFixed(2);
    var xl = o.x_labels ? o.x_labels[Math.floor(x)] : x;
    var yl = o.y_labels ? o.y_labels[Math.floor(y)] : y;
    var label = "";
    if ("tooltiplabel" in item.series) {
        label = item.series.tooltiplabel;
    } else if ("label" in item.series) {
        label = item.series.label;
    } else if (item.seriesIndex >= num_series) {
        var s = series[item.seriesIndex - num_series];
        if ("label" in s) label = s.label + " (mean)";
    }
    var metadata = [];
    if (label) metadata.push(['series', label]);
    metadata.push(['x', xl]);
    metadata.push(['y', yl]);
    var itemData = item.series.data[item.dataIndex];
    if (2 in itemData) {
        var props = itemData[2];
        for (p in props) metadata.push([p, props[p]]);
    }
    return metadata;
}

function generate_tooltip(o, item, id, diffs) {
    var click_tooltip = typeof id === 'undefined';
    id = typeof id === 'undefined' ? "tooltip_" + tooltip_counter++ : id;
    var body = "<div id='" + id + "' class='tooltip'><table width='100%'>";
    var metadata = get_metadata(o, item);
    for (var i in metadata) {
        var kv = metadata[i];
        body += "<tr><th>" + kv[0] + ":</th><td>" + kv[1] + "</td></tr>";
    }
    body += "</table>";
    if (click_tooltip) {
        body += "<img src='/close.png' />";
    } else if (Object.keys(diffs).length > 0) {
        body += "<br/><br/><table>";
        body += "<tr><th colspan='3'>Differences from latest selection:</th></tr>";
        body += "<tr><th>Key</th><th>Latest selected point</th><th>This point</th><th>% Diff</th></tr>";
        for (diff in diffs) {
            d = diffs[diff];
            prev_val = parseFloat(d[0]);
            new_val = parseFloat(d[1]);
            percent_diff = (isNaN(prev_val) || isNaN(new_val))? '' : ((new_val-prev_val)/prev_val*100).toFixed(2);
            body += "<tr><th>" + diff + "</th><td>" + d[0] + "</td><td>" + d[1] + "</td><td>" + percent_diff + "</td></tr>";
        }
        body += "</table>";
    }
    body += "</div>";
    return body;
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
