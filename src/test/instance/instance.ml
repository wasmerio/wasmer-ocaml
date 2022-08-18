open Ctypes;;
open Wasmer_ocaml.WasmerBindings;;
open Wasmer_ocaml.Util;;

(* The API prevents deleting a vector if its ownership has been taken away.
   Thus, a call to Imports.delete imports, after the instance has been created,
    is approximately equivalent to a no-op.
   If you try to use a deleted object, the API will throw an
    `Invalid_access "..."` exception. *)

let () =
  print_endline "Generating the wasm module...";
  let wasm = wasm_of_wat
    ("(module\n"
    ^"  (type $add_one_t (func (param i32) (result i32)))\n"
    ^"  (func $add_one_f (type $add_one_t) (param $value i32) (result i32)\n"
    ^"    local.get $value\n"
    ^"    i32.const 1\n"
    ^"    i32.add)\n"
    ^"  (export \"add_one\" (func $add_one_f)))") in
  
  print_endline "Creating the store...";
  let engine = Engine.new_ () in
  let store = Store.new_ engine in
  
  print_endline "Validating the module...";
  if not (Module.validate store wasm) then
    (print_endline "> Error validating the module!"; failwith "Invalid module!");
  
  print_endline "Compiling the module...";
  let module_ = Module.new_unsafe store wasm in
  if Module.is_null module_ then
    (print_endline "> Error compiling the module!"; failwith "Invalid module!");
  Byte.Vec.delete wasm;
  
  print_endline "Creating imports...";
  let imports = Extern.Vec.make_empty_null () in
  
  print_endline "Instanciating the module...";
  match Instance.new_ store module_ imports with
  | Error _ -> print_endline "> Error instanciating the module!"; failwith "Invalid module!"
  | Ok instance ->
  print_endline "Retrieving exports...";
  let exports = Extern.Vec.make_new () in
  Instance.exports instance exports;
  
  if (Extern.Vec.get_size exports = 0) then
    (print_endline "> Error accessing exports!"; failwith "Invalid module!");
  let add_one_f = Extern.to_func (Extern.Vec.get_element_unsafe exports 0) in
  if Func.is_null add_one_f then
    (print_endline "> Error instanciating the module!"; failwith "Invalid module!");
  
  Instance.delete instance;
  Module.delete module_;
  
  print_endline "Calling the `add_one` function...";
  let arg = Val.of_i32 1l in
  let args = Val.Vec.of_list [arg] in
  let results = Val.Vec.make_uninit 1 in
  
  match Func.call add_one_f args results with
  | Some _ -> print_endline "> Error calling the function!"; failwith "Invalid function!"
  | None ->
  print_endline ("Result of `add_one`: " ^
    (Int32.to_string (Val.get_i32 (Val.Vec.get_element_unsafe results 0))));
  
  Func.delete add_one_f;
  Val.Vec.delete args; (* Also frees arg (Val.delete arg is a noop) *)
  Val.Vec.delete results;
  Extern.Vec.delete exports;
  Store.delete store;
  Engine.delete engine;;
