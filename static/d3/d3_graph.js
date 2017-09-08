$( window ).load(function() {
  $("#graph1").hide();
  $("#graph1").css("background-color", "white");
});

function zip(a, b) {

return a.map(function (e, i) {
    return [e, b[e]];
});


}


function get_max_xy(series) {

  max_x = Math.max.apply(null, series.map(function(x) {return Math.max.apply(null, x.data.map(function(y){return y[0];   })); }));
 
  max_y = Math.max.apply(null, series.map(function(x) {return Math.max.apply(null, x.data.map(function(y){return y[1];
})); })); 
 
  var xy = [max_x, max_y]; 
  return xy; 

}

function get_xs(series, args=null) {

var my_set = new Set( 
([].concat.apply([], series.map(function (d) { return d.data; }))).map(function(xy) {return xy[0]; } 
)); 
return my_set; 

}

function d3_graph(graph, series, options, o, name="default") {

xaxis_label = options.xaxis.axisLabel;  
yaxis_label = $("span[class='som_name']").text();
$("[name_d3graph='default']").remove(); //remove the old svgs
$("[class='tooltip']").remove();  

// keep only elements that contain useful information for the graph
series = series.filter(function (x) {return !('lines' in x)});

// sort x elements in each line/
series = series.map(function (x) {x.data.sort(function(a, b){return a[0]-b[0]}); return x});  
xyz = get_max_xy(series); //get max x and max y in the xyz array 



 graph = new SimpleGraph("graph1",name, {
          "xmax": xyz[0], "xmin": 0,
          "ymax": xyz[1], "ymin": 0, 
          "title": "Simple Graph1",
          "xlabel": xaxis_label, 
        "ylabel": yaxis_label  
        }, series, o);
}; 

registerKeyboardHandler = function(callback) {
  var callback = callback;
  d3.select(window).on("keydown", callback);  
};

SimpleGraph = function(elemid, name, options, series, o) {


  if ($("#graph1").is(":hidden")) {     //if you click on the btn "redraw" while #graph1 is hidden, it won't redraw #graph1. Then if redraw is called while #graph1 
			                //is hidden, it first show, redraw and then hide again at the end of this function
	$("#graph1").show();
	var will_hide = 1; 

} 

  var self = this;
  this.o = o; 
  this.color = colors(series.length);
  this.series = series; 
  this.chart = document.getElementById(elemid);
  this.cx = this.chart.clientWidth;
  this.cy = this.chart.clientHeight; 
  this.options = options || {};
  this.options.xmax = options.xmax || 30;
  this.options.xmin = options.xmin || 0;
  this.options.ymax = options.ymax || 10;
  this.options.ymin = options.ymin || 0;

  this.padding = {
     "top":    this.options.title  ? 40 : 20,
     "right":                 30,
     "bottom": this.options.xlabel ? 60 : 10,
     "left":   this.options.ylabel ? 70 : 45
  };

  this.size = {
    "width":  this.cx - this.padding.left - this.padding.right,
    "height": this.cy - this.padding.top  - this.padding.bottom
  };

  // x-scale
  this.x = d3.scale.linear()
      .domain([this.options.xmin, this.options.xmax])
      .range([0, this.size.width]);
  // drag x-axis logic
  this.downx = Math.NaN;

  // y-scale (inverted domain)
  this.y = d3.scale.linear()
      .domain([this.options.ymax, this.options.ymin])
      .nice()
      .range([0, this.size.height])
      .nice();

  // drag y-axis logic
  this.downy = Math.NaN;

  //this.dragged = this.selected = null;

  this.line = d3.svg.line()
      .x(function(d, i) { return self.x(d[0]); })
      .y(function(d, i) { return self.y(d[1]); });

  var xrange =  (this.options.xmax - this.options.xmin),
      yrange2 = (this.options.ymax - this.options.ymin) / 2,
      yrange4 = yrange2 / 2,
      datacount = this.size.width/30;

  this.vis = d3.select(this.chart).append("svg")
      .attr("name_d3graph", name) 
      .attr("width",  this.cx)
      .attr("height", this.cy + 40)
      .attr("style", "background-color:white; padding:20px")
      .append("g")
        .attr("transform", "translate(" + this.padding.left + "," + this.padding.top + ")");

  this.plot = this.vis.append("rect")
      .attr("id", "graph1_rect")
      .attr("width", this.size.width)
      .attr("height", this.size.height)
      .style("fill", "#fff")
      .attr("pointer-events", "all")
      //.on("mousedown.drag", self.plot_drag())
      //.on("touchstart.drag", self.plot_drag())


      this.plot.call(d3.behavior.zoom().x(this.x).y(this.y).on("zoom", this.redraw()));


  this.vis.append("svg")
      .attr("top", 0)
      .attr("left", 0)
      .attr("width", this.size.width)
      .attr("height", this.size.height)
      .attr("viewBox", "0 0 "+this.size.width+" "+this.size.height)
      .selectAll(".line")
      .data(this.series)
      .enter()
      .append("path")
          .attr("class", "line")
	  .attr("fill", "none") 
          .attr("stroke-width", 0)
          .attr("stroke", function(d) { return self.color[d.color] })
          .attr("d", function(d) { return self.line(d.data)} )	

	  // add Chart Title

  // Add the x-axis label
  if (this.options.xlabel) {
    this.vis.append("text")
        .attr("class", "axis")
        .text(this.options.xlabel)
        .attr("x", this.size.width/2)
        .attr("y", this.size.height - this.size.width/2 - 20)
        .attr("dy","2.4em")
        .style("text-anchor","middle");
  }
  // add y-axis label
  if (this.options.ylabel) {
    this.vis.append("g").append("text")
        .attr("class", "axis")
        .text(this.options.ylabel)
        .style("text-anchor","middle")
        .attr("transform","translate(" + -40 + " " + this.size.height/2+") rotate(-90)");
  }

  d3.select(this.chart)
      .on("mousemove.drag", self.mousemove())
      .on("touchmove.drag", self.mousemove())
      .on("mouseup.drag",   self.mouseup())
      .on("touchend.drag",  self.mouseup());

  this.redraw()();

  if (will_hide == 1) 
  $("#graph1").hide(); 
  will_hide = 0; 

};
  

// SimpleGraph methods

SimpleGraph.prototype.update = function() {
  var self = this; 
  var clicked = true; 
  var tt_div = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("display", "none")
    .style("opacity", 0)
    .style("width", "270px")
    .style("height", "200px");

  var lines = this.vis
      .selectAll(".line")
      .data(this.series)
      .attr("stroke-width", 2)
      .attr("d", function(d) { return self.line(d.data)}); 


  var circle = this.vis.select("svg").selectAll("circle")
      .data([].concat.apply([], this.series.map(function (d) {
        return d.data.map(function(pos){ pos[3] = d.color; return pos;});
      })))

  circle.enter().append("circle")
      .attr("class", function(d) { return d === self.selected ? "selected" : null; })
      .attr("cx",    function(d) { return self.x(d[0]); })
      .attr("cy",    function(d) { return self.y(d[1]); })
      .attr("r", 5.0)
      .style("cursor", "ns-resize")
      .style("fill", function(d) { return self.color[d[3]]; });
	
  var is_img = 0; 

  circle
      .attr("class", function(d) { return d === self.selected ? "selected" : null; })
      .attr("cx",    function(d) { return self.x(d[0]); })
      .attr("cy",    function(d) { return self.y(d[1]); })
      .on("mouseover", function(d) {
 		tt_div.transition()
                   .duration(100)
		   .style("display", "inline")
                   .style("opacity", 0.85)
                   .style("left", (d3.event.pageX + 3) + "px")
                   .style("top", (d3.event.pageY + 3) + "px");      
		
		tt_div 
			.html("<table class=\"center_item\">" + 
				"<tr><td>" + "x" + "</td><td>" + "</td><td>" + d[0] + "</td></tr>" + 
				"<tr><td>" + "y" + "</td><td>" + "</td><td>" + d[1] + "</td></tr>" + 
				generate_metadata(d[2]));
		tt_div 
			.style("background-color", function(n) { return self.color[d[3]]; });
		totalHeight = 0;
		totalWidth = 0;
		$(".tooltip").children("table").children("tbody").children("tr").each(function(i) {
		totalHeight= totalHeight + $(this).height(); 
		});
		totalWidth = $(".tooltip").children("table").children("tbody").children("tr").width();

		if (totalHeight > $(".tooltip").height() && totalHeight != 0) { //adjust tooltip height if there are many elements
		
		$(".tooltip").css("height", (totalHeight + 40) + "px");

		}
		if (totalWidth > $(".tooltip").width() && totalWidth != 0) {

		$(".tooltip").css("width", (totalWidth + 20) + "px");

		}

		
		if (totalHeight < $(".tooltip").height() && totalHeight != 0) { 
		
		$(".tooltip").css("height", (totalHeight + 70) + "px");

		}
		if (totalWidth < $(".tooltip").width() && totalWidth != 0) {

		$(".tooltip").css("width", (totalWidth + 20) + "px");

		}

		tt_div.on("click", function(d) {

				
			var close_img = tt_div.append("div");
			is_img = 1; 
			close_img.html("<img src='close.png'>").on("click", function(d) {

				tt_div.style("opacity", 0);
				is_img = 0; 
				
			}); 

		}) 

 })

      .on("click", function(d) {
	
	tt_div.style("opacity", 0.85)
	var close_img = tt_div.append("div");
	is_img = 1; 
	close_img.html("<img src='close.png'>").on("click", function(d) {

		tt_div.style("opacity", 0);
		is_img = 0; 
				
	}); 

})


	.on("mouseout", function(d) {
		
		if (is_img == 0) {
		tt_div.style("opacity", 0);
		}

	}); 



function generate_metadata(metadata) {
    
    var body = ""; 
    for (var i in metadata) {
      var kv = metadata[i];

	body += "<tr><td>" + i + "</td><td>" + "</td>" + "<td>" + kv + "</td></tr>";  
    }

    body += "</table>"; 
    return body;
  } 

 

  circle.exit().remove();

  if (d3.event && d3.event.keyCode) {
    d3.event.preventDefault();
    d3.event.stopPropagation();
  }
}; 

SimpleGraph.prototype.datapoint_drag = function(index) { //update points position while moving the graph 
  var self = this;
  return function(d) {
    registerKeyboardHandler(self.keydown());
    document.onselectstart = function() { return false; };
    self.selected = self.dragged = d;
    self.update();  
  }
};

SimpleGraph.prototype.mousemove = function(index) {
  var self = this;
  return function() {
    var p = d3.svg.mouse(self.vis[0][0]),
        t = d3.event.changedTouches;
    
    if (self.dragged) {
     // self.dragged.y = self.y.invert(Math.max(0, Math.min(self.size.height, p[1])));
      self.update();
    };
    if (!isNaN(self.downx)) {
      d3.select('body').style("cursor", "ew-resize");
      var rupx = self.x.invert(p[0]),
          xaxis1 = self.x.domain()[0],
          xaxis2 = self.x.domain()[1],
          xextent = xaxis2 - xaxis1;
      if (rupx != 0) {
        var changex, new_domain;
        changex = self.downx / rupx;
        new_domain = [xaxis1, xaxis1 + (xextent * changex)];
        self.x.domain(new_domain);
        self.redraw()();
      }
      d3.event.preventDefault();
      d3.event.stopPropagation();
    };
    if (!isNaN(self.downy)) {
      d3.select('body').style("cursor", "ns-resize");
      var rupy = self.y.invert(p[1]),
          yaxis1 = self.y.domain()[1],
          yaxis2 = self.y.domain()[0],
          yextent = yaxis2 - yaxis1;
      if (rupy != 0) {
        var changey, new_domain;
        changey = self.downy / rupy;
        new_domain = [yaxis1 + (yextent * changey), yaxis1];
        self.y.domain(new_domain);
        self.redraw()();
      }
      d3.event.preventDefault();
      d3.event.stopPropagation();
    }
  }
};

SimpleGraph.prototype.mouseup = function() {
  var self = this;
  return function() {
    document.onselectstart = function() { return true; };
    d3.select('body').style("cursor", "auto");
    d3.select('body').style("cursor", "auto");
    if (!isNaN(self.downx)) {
      self.redraw()();
      self.downx = Math.NaN;
      d3.event.preventDefault();
      d3.event.stopPropagation();
    };
    if (!isNaN(self.downy)) {
      self.redraw()();
      self.downy = Math.NaN;
      d3.event.preventDefault();
      d3.event.stopPropagation();
    }
    if (self.dragged) { 
      self.dragged = null 
    }
  }
}

SimpleGraph.prototype.redraw = function() {

  var will_hide = 0; 

  var self = this;
  
  $("[class='tooltip']").remove();

  $("div[id='legend_container']").remove();
  
  d3.select("#graph1")
	.append("div")
	.attr("id", "legend_container")
	.attr("width", "200px")
	.attr("height", "50px")

  d3.select("#legend_container")
	.append("table")
	.attr("id", "legend_table")
	.attr("cellspacing", "9");

  for (var i=0; i < self.series.length; i++) {	
		
		var table_tr = d3.select("#legend_table").append("tr");  
		table_tr.append("td").attr("style", function(d) {return "background-color:" + self.color[self.series[i].color] + ";";}).append("div").attr("style", "width:2em");

		table_tr.append("td").attr("class", "a30x100").append("div")

			.attr("style", "width:960px; white-space: initial;overflow: hidden;text-overflow: ellipsis; word-wrap:break-word")
			.text(self.series[i].label);
			
		
}

  if (typeof(this.o.x_labels) != "undefined") {

    var fx_label = function(d) { return d[1]; };

  } else {

	var fx_label = function(d) { return self.x.tickFormat(10)(d[0]) };

  }

  return function() {
    var tx = function(d) {
      return "translate(" + self.x(d[0]) +  ",0)"; 
    },
    ty = function(d) { 
      return "translate(0," + self.y(d) + ")";
    },
    stroke = function(d) { 
      return d ? "#ccc" : "#666"; 
    },

    fx = fx_label, 
    fy = self.y.tickFormat(10);

	var count = 0; 

    // Regenerate x-ticks…
    var xs = get_xs(self.series);  
    var gx = self.vis.selectAll("g.x")
        .data(function(d) {
			xs_ary = Array.from(xs.values());
			if (typeof(self.o.x_labels) != "undefined")
				a =  zip(xs_ary,self.o.x_labels);
			else a = zip(self.x.ticks(xs.size),self.x.ticks(xs.size))
			return a;
		}, String)

        .attr("transform", tx);
    gx.select("text")
        .text(fx);

    var gxe = gx.enter().insert("g", "a")
        .attr("class", "x")
        .attr("transform", tx);

    gxe.append("line")
        .attr("stroke", stroke)
        .attr("y1", 0)
        .attr("y2", self.size.height);

    gxe.append("text")
        .attr("class", "axis")
        .attr("y", self.size.height)
        .attr("dy", "1em")
        .attr("text-anchor", "start")
        .text(fx)
        .style("cursor", "ew-resize")
        .on("mouseover", function(d) { d3.select(this).style("font-weight", "bold");})
        .on("mouseout",  function(d) { d3.select(this).style("font-weight", "normal");})
        .on("mousedown.drag",  self.xaxis_drag())
        .on("touchstart.drag", self.xaxis_drag())
	.attr("transform", function(d) {
                return "translate(" + 80 + ", " + 30 + ")" + " rotate(12)"});
 
    gx.exit().remove();

    // Regenerate y-ticks…
    var gy = self.vis.selectAll("g.y")
        .data(self.y.ticks(10), String)
        .attr("transform", ty);

    gy.select("text")
        .text(fy);

    var gye = gy.enter().insert("g", "a")
        .attr("class", "y")
        .attr("transform", ty)
        .attr("background-fill", "#FFEEB6");

    gye.append("line")
        .attr("stroke", stroke)
        .attr("x1", 0)
        .attr("x2", self.size.width);

    gye.append("text")
        .attr("class", "axis")
        .attr("x", -3)
        .attr("dy", ".35em")
        .attr("text-anchor", "end")
        .text(fy)
        .style("cursor", "ns-resize")
        .on("mouseover", function(d) { d3.select(this).style("font-weight", "bold");})
        .on("mouseout",  function(d) { d3.select(this).style("font-weight", "normal");})
        .on("mousedown.drag",  self.yaxis_drag())
        .on("touchstart.drag", self.yaxis_drag());

    gy.exit().remove();
    self.plot.call(d3.behavior.zoom().x(self.x).y(self.y).on("zoom", self.redraw()));
    self.update();      
  }

	if (will_hide == 1) {

	$("#graph1").hide();
	will_hide = 0;

}
  
}

SimpleGraph.prototype.xaxis_drag = function() {
  var self = this;
  return function(d) {
    document.onselectstart = function() { return false; };
    var p = d3.svg.mouse(self.vis[0][0]);
    self.downx = self.x.invert(p[0]);
  }
};

SimpleGraph.prototype.yaxis_drag = function(d) {
  var self = this;
  return function(d) {
    document.onselectstart = function() { return false; };
    var p = d3.svg.mouse(self.vis[0][0]);
    self.downy = self.y.invert(p[1]);
  }

}; 
 
