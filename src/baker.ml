(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* baker.ml -                                      *)
(***************************************************)
(* $Id$ *)

let sprintf = Printf.sprintf
let srcs : (string, (bool * string)) Hashtbl.t = Hashtbl.create 101

let l2s = List.fold_left (fun acc x -> if acc = "" then x else Printf.sprintf "%s %s" acc x) "" 

let rec loop = function
    [] -> ()
  | f::rest -> 
      begin 
      match (Unix.lstat f).Unix.st_kind with
          Unix.S_REG -> 
            let is_src = Filename.check_suffix f ".src" in 
            let is_fcl = Filename.check_suffix f ".fcl" in 
              if (is_src or is_fcl) then
                begin
                  let fn = Filename.chop_extension (Filename.basename f) in              
                  let src_exists = try match Hashtbl.find srcs fn with (b,_) -> b with Not_found -> false in
                    if (is_src or not (src_exists)) then
                      (* don't overwrite a src with a fcl *)
                      begin
                        let fc = open_in f in
                        let len = in_channel_length fc in 
                        let buf = String.create len in
                          really_input fc buf 0 len;
                          close_in fc;
                          Hashtbl.replace srcs fn (is_src, (String.escaped buf))
                      end;
                end;
              loop rest
        | Unix.S_DIR -> 
            let rec dirloop dh fs = 
              try 
                let dc = Unix.readdir dh in
                let dn = sprintf "%s/%s" f dc in
                  dirloop dh (if dc = "." or dc =".." then fs else dn::fs)
              with End_of_file -> fs in 
            let dh = Unix.opendir f in
            let dcs = List.sort compare (dirloop dh []) in
            let _ = Unix.closedir dh in
              loop (rest @ dcs)
        | _ -> loop rest
    end

let _ = 
  let args = List.tl (Array.to_list Sys.argv) in
    loop args;
    Printf.printf "let items : (string,(bool*string)) Hashtbl.t = Hashtbl.create %d\n" (Hashtbl.length srcs);
    Hashtbl.iter (fun s (b,c) -> Printf.printf "let _ = Hashtbl.add items \"%s\" (%b,\"%s\")\n%!" s b c) srcs
