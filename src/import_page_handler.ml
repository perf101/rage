open Core
open Async

let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body =
    printf "<div class=\"import_page box\">\n";
    printf "<p>Data from completed XenRT jobs is imported automatically daily.</p>\n";
    printf "<p>This page can be used to force the immediate import of one or more XenRT jobs.</p>\n";
    printf "<form action='/' method='post'>\n";
    printf "<input type='hidden' name='p' value='import_jobs'/>\n";
    printf "<div class=\"import_page input_label\">Job number: </div>"; (*newline omitted as html will render a space in between these elements*)
    printf "<input type='text' name='jobid'/>";
    printf "<input type='submit' name='submit' value='Import now'/>\n";
    printf "</form>";
    printf "</div>";
    return ()

end
