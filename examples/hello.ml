open Ctypes
open Wasmer
open Wasmer.Util

(* The file isn't necessarily copied over, so it is now embedded in this file *)
let raw_data = "\x00asm\x01\x00\x00\x00\x01\x84\x80\x80\x80\x00\x01\x60\x00\x00\x02\x8a\x80\x80\x80\x00\x01\x00\x05hello\x00\x00\x03\x82\x80\x80\x80\x00\x01\x00\x07\x87\x80\x80\x80\x00\x01\x03run\x00\x01\x0a\x8a\x80\x80\x80\x00\x01\x84\x80\x80\x80\x00\x00\x10\x00\x0b"

let hello_callback store args results =
  print_endline "Calling back...";
  print_endline "> Hello world!";
  None

let () =
  (* let wasm = load_wasm_file "hello.wasm" in *)
  let wasm = Byte.Vec.of_bytes (Bytes.of_string raw_data) in

  let engine = Engine.new_ () in
  let store = Store.new_ engine in

  if not (Module.validate store wasm) then (
    print_endline "> Error validating module!";
    failwith "Invalid module!");

  let real_module = Module.new_ store wasm in

  let hello_functype = [] %-> [] in
  let hello_func = Func.new_ store hello_functype hello_callback in

  let imports = Extern.Vec.of_list [ Extern.of_func hello_func ] in
  match Instance.new_unsafe store real_module imports with
  | Error _ ->
      print_endline "> Error instanciating module!";
      failwith "Invalid module!"
  | Ok instance -> (
      let exports = Extern.Vec.make_new () in
      Instance.exports instance exports;

      let run_func = Extern.to_func (Extern.Vec.get_element exports 0) in
      if Func.is_null run_func then (
        print_endline "> Error accessing exports!";
        failwith "Invalid instance!");

      let args = Val.Vec.make_empty_null () in
      let results = Val.Vec.make_empty_null () in

      match Func.call run_func args results with
      | Some _ ->
          print_endline "> Error calling the function!";
          failwith "Invalid function!"
      | None -> print_endline "Done.")
