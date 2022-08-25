open Ctypes
open Wasmer
open Wasmer.Util

let memory_ref = ref None

let print_str store args ret =
  match !memory_ref with
  | None ->
      Some
        (Trap.new_ store (Message.of_string "Called print_str with no memory!"))
  | Some memory ->
      let offset = Val.get_i32 (Val.Vec.get_element_const args 0) in
      let len = Val.get_i32 (Val.Vec.get_element_const args 1) in
      let b = Memory.get_data memory (Int32.to_int offset) (Int32.to_int len) in
      print_string (Bytes.to_string b);
      flush stdout;
      None

let () =
  let wasm =
    load_wasm_file
      "rust-wasm/target/wasm32-unknown-unknown/release/rust_integration.wasm"
  in

  let engine = Engine.new_ () in
  let store = Store.new_ engine in

  if not (Module.validate store wasm) then (
    print_endline "> Error validating module!";
    failwith "Invalid module!");

  let real_module = Module.new_ store wasm in

  let print_str_functype = Valkind.[ I32; I32 ] %-> Valkind.[] in
  let print_str_func = Func.new_ store print_str_functype print_str in

  let imports = Extern.Vec.of_list [ Extern.of_func print_str_func ] in
  match Instance.new_unsafe store real_module imports with
  | Error _ ->
      print_endline "> Error instanciating module!";
      failwith "Invalid module!"
  | Ok instance -> (
      let exports_raw = Extern.Vec.make_new () in
      Instance.exports instance exports_raw;

      let ( exported_funcs,
            (*exported_globals*) _,
            (*exported_tables*) _,
            exported_memories ) =
        Extern.Vec.split_kind exports_raw
      in
      let run_func, memory0 =
        match (exported_funcs, exported_memories) with
        | [ f ], [ m ] -> (f, m)
        | _ ->
            print_endline "> Error accessing exports (no function)!";
            failwith "Invalid instance!"
      in
      memory_ref := Some memory0;

      let args = Val.Vec.make_empty_null () in
      let results = Val.Vec.make_empty_null () in

      match Func.call run_func args results with
      | Some trap ->
          print_endline "> Error calling the function!";
          let msg = Message.make_new () in
          Trap.message trap msg;
          print_endline (Message.to_string msg);
          failwith "Invalid function!"
      | None -> ())
