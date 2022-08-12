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
