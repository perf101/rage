val indent : int -> string -> string

val cat : string -> unit

val print_row : string -> string list -> unit

val print_table :
  < get_all_lst : string list list; get_fnames_lst : string list; .. > ->
  unit

val natural_map_from_list : 'a list -> (int, 'a) Hashtbl.t

val reverse_map : ('a, 'b) Hashtbl.t -> ('b, 'a) Hashtbl.t
