open Async
let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body = printf "<script src='rage.js'></script>"; return ()
end
