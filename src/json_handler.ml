open Core

class t = fun ~args ->
object (self)
  inherit Handler.t ~args

  method private write_header = printf "Content-type: application/json\n\n"
end
