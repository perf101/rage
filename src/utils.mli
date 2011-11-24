open Core.Std

val indent : int -> string -> string

val cat : string -> unit

val print_row : string -> string list -> unit

val print_table :
  < get_all_lst : string list list; get_fnames_lst : string list; .. > ->
  unit

val natural_map_from_list : string list -> (int, string) Hashtbl.t

val reverse_natural_map : (int, string) Hashtbl.t -> (string, int) Hashtbl.t
