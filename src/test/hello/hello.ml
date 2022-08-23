open Ctypes;;
open Wasmer.WasmerBindings;;
open Wasmer.Util;;

let hello_callback store args results =
  print_endline "Calling back...";
  print_endline "> Hello world!";
  None;;

let () =
  print_endline "Initializing...";
  let eng = Engine.new_ () in
  let sto = Store.new_ eng in
  print_endline "Loading binary...";
  let wasm = load_wasm_file "hello.wasm" in
  
  print_endline "Validating module...";
  if not (Module.validate sto wasm) then
    (print_endline "> Error validating module!"; failwith "Invalid module!");
  
  print_endline "Compiling module...";
  let real_module = Module.new_unsafe sto wasm in
  if Module.is_null real_module then
    (print_endline "> Error compiling module!"; failwith "Invalid module!");
  
  print_endline "Creating callback...";
  let hello_functype = [] %-> [] in
  let hello_func = Func.new_ sto hello_functype hello_callback in
  
  print_endline "Instanciating module...";
  let imports = Extern.Vec.of_list [Extern.of_func hello_func] in
  match Instance.new_unsafe sto real_module imports with
  | Error _ -> print_endline "> Error instanciating module!"; failwith "Invalid module!"
  | Ok instance ->
  
  print_endline "Extracting exports...";
  let exports = Extern.Vec.make_new () in
  Instance.exports instance exports;
  if (Extern.Vec.get_size exports = 0) then
    (print_endline "> Error accessing exports!"; failwith "Invalid instance!");
  
  let run_func = Extern.to_func (Extern.Vec.get_element_unsafe exports 0) in
  if Func.is_null run_func then
    (print_endline "> Error accessing exports!"; failwith "Invalid instance!");
  
  print_endline "Calling the exported function...";
  let args = Val.Vec.make_empty_null () in
  let results = Val.Vec.make_empty_null () in
  
  match Func.call run_func args results with
  | Some _ -> print_endline "> Error calling the function!"; failwith "Invalid function!"
  | None ->
  
  print_endline "Shutting down...";;
