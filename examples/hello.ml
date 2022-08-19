open Ctypes;;
open Wasmer_ocaml.WasmerBindings;;
open Wasmer_ocaml.Util;;

let hello_callback args results =
  print_endline "Calling back...";
  print_endline "> Hello world!";
  None;;

let () =
  let wasm =
    let f = open_in_bin "hello.wasm" in
    try
      let flen = in_channel_length f in
      let b = Bytes.create flen in
      really_input f b 0 flen;
      let ret = Byte.Vec.of_bytes b in
      close_in f; ret
    with e ->
      close_in_noerr f;
      print_endline "> Failed to load file to memory!";
      raise e in
  
  let engine = Engine.new_ () in
  let store = Store.new_ engine in
  
  if not (Module.validate store wasm) then
    (print_endline "> Error validating module!"; failwith "Invalid module!");
  
  let real_module = Module.new_ store wasm in
  
  let hello_functype = [] %-> [] in
  let hello_func = Func.new_ store hello_functype hello_callback in
  
  let imports = Extern.Vec.of_list [Extern.of_func hello_func] in
  match Instance.new_unsafe store real_module imports with
  | Error _ -> print_endline "> Error instanciating module!"; failwith "Invalid module!"
  | Ok instance ->
  
  let exports = Extern.Vec.make_new () in
  Instance.exports instance exports;
  
  let run_func = Extern.to_func (Extern.Vec.get_element exports 0) in
  if Func.is_null run_func then
    (print_endline "> Error accessing exports!"; failwith "Invalid instance!");
  
  
  let args = Val.Vec.make_empty_null () in
  let results = Val.Vec.make_empty_null () in
  
  match Func.call run_func args results with
  | Some _ -> print_endline "> Error calling the function!"; failwith "Invalid function!"
  | None ->
  
  print_endline "Done.";;
