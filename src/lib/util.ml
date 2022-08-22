open Ctypes;;
open Foreign;;
open Wasmer_ocaml__WasmerBindings;;

let wat2wasm =
  let f = foreign "wat2wasm" (ptr Byte.Vec.t @-> ptr Byte.Vec.t @-> returning void) in
  fun s dst ->
    let bytevec = Byte.Vec.of_bytes (Bytes.of_string s)
    in f (Byte.Vec.get_ptr_const bytevec) (Byte.Vec.gain_ownership_back dst);
    Byte.Vec.delete bytevec;;
let wasm_of_wat wat =
  let dst = Byte.Vec.make_new () in
  wat2wasm wat dst;
  dst;;

let (%->) args res =
  Functype.new_
    (Valtype.Vec.of_list (List.map Valtype.new_ args))
    (Valtype.Vec.of_list (List.map Valtype.new_ res));;

let load_wasm_file filename =
  let f = open_in_bin filename in try
    let flen = in_channel_length f in
    let b = Bytes.create flen in
    really_input f b 0 flen;
    let ret = Byte.Vec.of_bytes b in
    close_in f; ret
  with e ->
    close_in_noerr f;
    raise e;;
