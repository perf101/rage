let uint32_max = (2. ** 32.) -. 1.

(** [int32_of_01 f] returns a 32-bit unsigned integer coresponding to the [f] value.

  @param f floating-point value in the range [[0, 1]]
*)
let[@inline always] int32_of_01 f =
  (* cannot convert directly with [Int32.of_float], because it is documented
     to return an unspecified integer when the argument is out of range
     (and in particular returns the same constant value for all values that are out of range).
     Whereas the [Int64.to_int32] is documented to work modulo [2^{32}]  
   *)
  f *. uint32_max |> Int64.of_float |> Int64.to_int32

let () =
  assert (int32_of_01 0. = 0l);
  assert (int32_of_01 0.5 = Int32.max_int);
  assert (int32_of_01 1. = -1l)

(** [stream_floats gen] writes a continous stream of bits on standard output.

  @params gen generates floats in the range [[0., 1.]]
 *)
let stream_floats gen =
  assert (Sys.word_size = 64) ;
  let buf = Bytes.create 65536 in
  while true do
    for i = 0 to 65535 / 4 do
      (* Cannot use int32.of_float, because that only has 31 bits range + sign,
         and overflowing during the float conversion would result in a static value.
         Also cannot use regular int, because that only has range of 62 bits + sign
      *)
      gen () |> int32_of_01 |> Bytes.set_int32_ne buf (4*i)
    done ;
    let n = Bytes.length buf in
    if Unix.write Unix.stdout buf 0 n <> n then
      exit 1
  done

let g0 () = 0.

let g1 () = Random.float 1.

let () = Random.self_init () ; stream_floats g1
