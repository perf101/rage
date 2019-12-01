let t ~args = object (self)
  inherit Html_handler.t ~args

  method private write_body = Printf.printf "<script src='rage.js'></script>"; Async.return ()
end
