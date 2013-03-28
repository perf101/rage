open Core.Std
open Utils

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    printf "<ul>\n";
    printf "<li><a href='?p=reports'>Reports</a></li>\n";
    printf "<li><a href='?p=som_index'>Scales of Measure</a></li>\n";
    printf "<li><a href='?p=soms_by_tc'>Scales of Measure by Test Case</a></li>\n";
    printf "<li><a href='?p=briefs'>Brief Reports</a></li>\n";
    printf "</ul>\n";
end
