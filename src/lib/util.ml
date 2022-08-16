open Ctypes;;
open Foreign;;
open Wasmer_ocaml__WasmerBindings;;

let wat2wasm =
  let f = foreign "wat2wasm" (ptr Byte.Vec.t @-> ptr Byte.Vec.t @-> returning void) in
  fun s dst ->
    let bytevec = Byte.Vec.of_bytes (Bytes.of_string s)
    in f bytevec dst; Byte.Vec.delete bytevec;;
let wasm_of_wat wat =
  let dst = Byte.Vec.make () in
  wat2wasm wat dst;
  dst;;

let functype_new_0_0 () =
  Functype.new_ (Valtype.Vec.make_empty ()) (Valtype.Vec.make_empty ());;
let functype_new_args_0 args =
  Functype.new_ (Valtype.Vec.of_list args) (Valtype.Vec.make_empty ());;
let functype_new_0_res res =
  Functype.new_ (Valtype.Vec.make_empty ()) (Valtype.Vec.of_list res);;
let functype_new_args_res args res =
  Functype.new_ (Valtype.Vec.of_list args) (Valtype.Vec.of_list res);;
