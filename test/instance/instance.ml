open Ctypes;;
open Wasmer_ocaml.WasmerBindings;;
open Wasmer_ocaml.Util;;

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
    (print_endline "> Error validating the module!"; failwith "Invalid module!")
  else begin
    print_endline "Compiling the module...";
    let module_ = Module.new_ store wasm in
    if is_null module_ then
      (print_endline "> Error compiling the module!"; failwith "Invalid module!")
    else begin
      Byte.Vec.delete wasm;
      
      print_endline "Creating imports...";
      let imports = Extern.Vec.make_empty_null () in
      
      print_endline "Instanciating the module...";
      let instance = Instance.new_ store module_ imports (from_voidp (ptr Trap.t) null) in
      if is_null instance then
        (print_endline "> Error instanciating the module!"; failwith "Invalid module!")
      else begin
        print_endline "Retrieving exports...";
        let exports = Extern.Vec.make () in
        Instance.exports instance exports;
        
        if (Unsigned.Size_t.to_int (!@ (exports |-> Extern.Vec.fsize)) = 0) then
          (print_endline "> Error accessing exports!"; failwith "Invalid module!")
        else begin
          let add_one_f = Extern.to_func (!@ !@ (exports |-> Extern.Vec.fdata)) in
          if is_null add_one_f then
            (print_endline "> Error instanciating the module!"; failwith "Invalid module!")
          else begin
            Module.delete module_;
            Instance.delete instance;
            
            print_endline "Calling the `add_one` function...";
            let arg = Val.of_i32 1l in
            let args = Val.Vec.of_list [arg] in
            let results = Val.Vec.make_uninit 1 in
            
            if not (is_null (Func.call add_one_f args results)) then
              (print_endline "> Error calling the function!"; failwith "Invalid function!")
            else begin
              print_endline ("Result of `add_one`: " ^
                (Int32.to_string
                 (!@
                  (!@ (results |-> Val.Vec.fdata) |-> Val.fof |-> Val.Anon0.fi32))));
              
              Extern.Vec.delete exports;
              Store.delete store;
              Engine.delete engine
            end
          end
        end
      end
    end
  end;;
