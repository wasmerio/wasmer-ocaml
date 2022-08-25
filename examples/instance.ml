open Ctypes
open Wasmer
open Wasmer.Util

(*
This example loads a Wat script (embedded in the source code below),
and executes it.

The first function (mul_two_f) multiplies by two its input
after converting it from an i32 to an i64 using a callback to OCaml.

The second function (one_lsl_f) gives 2 to the power of its input,
given its input is between 0 and 63 (otherwise, it will return 0).
*)

let convert_i32_i64 store args rets =
  try
    let arg = Val.Vec.get_element_const args 0 in
    let ret = Val.of_i64 (Int64.of_int32 (Val.get_i32 arg)) in
    Val.Vec.set_element rets 0 ret;
    None
  with e ->
    Some
      (Trap.new_ store
         (Message.of_string ("Failed with exception: " ^ Printexc.to_string e)))

let () =
  let wasm =
    wasm_of_wat
      {|(module
      (import "" "convert_i32_i64" (func $do_conversion (param i32) (result i64)))
      (func $mul_two_f (export "mul_two")
       (param $value i32) (result i64) (local $loc i64)
        local.get $value
        call $do_conversion
        local.set $loc
        local.get $loc
        local.get $loc
        i64.add)
      (func $one_lsl (export "one_lsl")
       (param $value i32) (result i64) (local $counter i32) (local $acc i64)
        local.get $value
        i32.const 63
        i32.le_u
        if $ifLabel
          i64.const 1
          local.set $acc
          local.get $value
          local.set $counter
          loop $Loop
            block $ExitLoop
              local.get $counter
              i32.eqz
              br_if $ExitLoop
              local.get $counter
              i32.const 1
              i32.sub
              local.set $counter
              local.get $acc
              local.get $acc
              i64.add
              local.set $acc
              br $Loop
            end
          end
        else
          i64.const 0
          local.set $acc
        end
        local.get $acc))|}
  in

  let engine = Engine.new_ () in
  let store = Store.new_ engine in
  let module_ = Module.new_ store wasm in

  let functype = Valkind.[ I32 ] %-> Valkind.[ I64 ] in
  let func = Func.new_ store functype convert_i32_i64 in

  let imports = Extern.Vec.of_list [ Extern.of_func func ] in

  match Instance.new_ store module_ imports with
  | Error None ->
      print_endline "> Error instanciating the module! (Unknown error)";
      failwith "Invalid module!"
  | Error (Some trap) ->
      print_endline "> Error instanciating the module!";
      let msg = Message.make_new () in
      Trap.message trap msg;
      print_endline (Message.to_string msg);
      failwith "Invalid module!"
  | Ok instance -> (
      let exports = Extern.Vec.make_new () in
      Instance.exports instance exports;

      if Extern.Vec.get_size exports = 0 then (
        print_endline "> Error accessing exports!";
        failwith "Invalid module!");
      let mul_two_f =
        Extern.to_func (Extern.Vec.get_element_unsafe exports 0)
      in
      let one_lsl_f = Extern.to_func (Extern.Vec.get_element exports 1) in
      if Func.is_null mul_two_f then (
        print_endline "> Error getting mul_two!";
        failwith "Invalid module!");
      if Func.is_null one_lsl_f then (
        print_endline "> Error getting 1<<!";
        failwith "Invalid module!");

      let arg = Val.of_i32 3l in
      let args = Val.Vec.of_list [ arg ] in
      let results = Val.Vec.make_uninit 1 in

      match Func.call mul_two_f args results with
      | Some trap ->
          print_endline "> Error calling the function!";
          let msg = Message.make_new () in
          Trap.message trap msg;
          print_endline (Message.to_string msg);
          failwith "Invalid function!"
      | None -> (
          print_endline
            ("Result of `mul_two`: "
            ^ Int64.to_string
                (Val.get_i64 (Val.Vec.get_element_const results 0)));

          match Func.call one_lsl_f args results with
          | Some trap ->
              print_endline "> Error calling the function!";
              let msg = Message.make_new () in
              Trap.message trap msg;
              print_endline (Message.to_string msg);
              failwith "Invalid function!"
          | None ->
              print_endline
                ("Result of `one_lsl`: "
                ^ Int64.to_string
                    (Val.get_i64 (Val.Vec.get_element_const results 0)))))
