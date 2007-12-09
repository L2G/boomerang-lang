module Source = Type.Convert_type(Bibtex.Source)
module THarmony = Source.Traverse(Harmony_tool)
module UHarmony = Source.Untraverse(Harmony_untool)
module TDebug = Source.Traverse(Debug_tool)

exception Failure of string

let handle_res s = function
    Pads.Ok p -> p 
  | Pads.Error -> raise (Failure s)

let print_source print_fn file_name = 
  let pads = handle_res "open handle" (Pads.open_handle ()) in 
  let _ = handle_res "open_out_file" (Pads.IO.open_out_file pads file_name) in 
  let _ = print_fn pads in 
(*  let _ = handle_res "close" (Pads.IO.close pads) in *)
  let _ = handle_res "close_handle" (Pads.close_handle pads) in 
    ()

let in_stream = 
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else "/dev/stdin"

let out_stream = 
  if Array.length Sys.argv > 2 then Sys.argv.(2)
  else "/dev/stdout"

let r,pd = Tester.parse_source Source.parse in_stream false
let _ = 
  Format.printf "PADS structure:@\n%!"; 
  TDebug.traverse r pd (TDebug.init ())
let t = try THarmony.traverse r pd (THarmony.init ()) with
    Error.Harmony_error(thk) -> 
      begin 
        thk (); 
        assert false
      end
let _ = 
  Format.printf "@\nHarmony Tree:@\n%!";
  Tree.format_t t
let r' = UHarmony.untraverse t
let _ = 
  Format.printf "@\nPADS structure:@\n%!"; 
  TDebug.traverse r' pd (TDebug.init ())
let _ = 
  Format.printf "@\nWriting output:%!";
  print_source (Source.print r pd) out_stream;
  Format.printf "@\nDone@\n%!"  

