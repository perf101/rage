open Core.Std
open Printf
open Utils

class t = fun ~args ->
object (self)
  inherit Handler.t ~args

  method private include_javascript =
    printf "<script src='rage.js'></script>"

  method private write_404 =
    printf "Status: 404 Not Found\n";
    printf "Content-Type: text/html\n\n";
    printf "<h1>404 --- this is not the page you are looking for ...</h1>"

  method private javascript_redirect url =
    printf "Content-type: text/html\n\n";
    printf "<html><head>\n";
    printf "<script language='javascript' type='text/javascript'>\n";
    printf "window.location.replace(decodeURIComponent('%s'));\n" url;
    printf "</script>\n</head><body></body></html>\n"

  method private write_header = self#write_html_header

  method private write_footer = cat (base_path ^ "footer.html")
end
