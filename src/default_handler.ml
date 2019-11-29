open Core

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    printf "<ul class=\"link_list home\">\n";
    printf "<li><a href='?p=soms_by_tc'>Scales of Measure</a></li>\n";
    printf "<li><a href='?p=import_page'>Import Jobs</a></li>\n";
    printf "</ul>\n";
end
