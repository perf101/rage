type setting =
  | X_axis of axis_setting list
  | Y_axis of axis_setting list
and axis_setting =
  | Min of int
  | Max of int
  | TickFormatter of string

type settings = setting list

type labels = (int, string) Hashtbl.t

type datum = int * int

type data = datum list

val string_of_setting : setting -> string

val string_of_settings : settings -> string

val plot : ?settings:settings -> ?labels:labels -> data -> string
