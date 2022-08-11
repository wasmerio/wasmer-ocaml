(*open Ctypes;;*)
open Wasmer_ocaml.WasmerBindings;;

let () =
ignore Ref.same;;(*
  print_endline "Initializing...";
  let eng = Engine.new_ () in
  let sto = Store.new_ eng in
  print_endline "Loading binary...";
  let wasm =
    let f = open_in_bin "hello.wasm" in
    try
      let flen = in_channel_length f in
      let b = Bytes.create flen in
      really_input f b 0 flen;
      let ret = ByteVec.of_bytes b in
      close_in f; ret
    with e ->
      close_in_noerr f;
      print_endline "> Failed to load file to memory!";
      raise e in
  
  print_endline "Validating module...";
  if not (Module.validate sto wasm) then
    (print_endline "> Error validating module!"; failwith "Invalid module!")
  else begin
    print_endline "Compiling module...";
    let real_module = Module.new_ sto wasm in
    if is_null real_module then
      (print_endline "> Error compiling module!"; failwith "Invalid module!")
    else begin
      Byte_vec.delete wasm;
      
      print_endline "Creating callback...";
      (* TODO *)
      print_endline "TODO"
    end
  end
*)
