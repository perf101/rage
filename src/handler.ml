open Core
open Utils

type args = {
  conn : Postgresql.connection;
  params : (string * string) list;
}

class virtual t = fun ~(args : args) ->
object (self)
  val conn = args.conn
  val params = args.params

  val base_path : string =
    let exe = (Sys.get_argv ()).(0) in
    String.sub exe ~pos:0 ~len:((String.rindex_exn exe '/') + 1)

  val mutable html_header_written : bool = false

  method private write_header = ()

  method private write_body = ()

  method private write_footer = ()

  method handle =
    self#write_header;
    self#write_body;
    self#write_footer

  method private write_html_header =
    printf "Content-type: text/html\n\n";
    cat (base_path ^ "header.html");
    html_header_written <- true

  method private fail (msg : string) : unit =
    if not html_header_written then self#write_html_header;
    failwith msg

  method private get_param key = List.Assoc.find ~equal:String.equal params key

  method private get_param_exn key = List.Assoc.find_exn ~equal:String.equal params key

  method private get_params_gen ~params key =
    List.fold params ~init:[]
      ~f:(fun acc (k, v) -> if String.(k = key) then v::acc else acc)

  method private get_params key = self#get_params_gen ~params key

  method private debug_params =
    List.iter ~f:(fun (k, v) -> debug (sprintf "%s ==> %s" k v)) params
end
