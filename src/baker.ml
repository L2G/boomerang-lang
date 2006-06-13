(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* baker.ml -                                      *)
(***************************************************)
(* $Id$ *)

let sprintf = Printf.sprintf
let srcs : (string, string) Hashtbl.t = Hashtbl.create 101

let l2s = List.fold_left (fun acc x -> if acc = "" then x else Printf.sprintf "%s %s" acc x) "" 

let rec loop = function
    [] -> ()
  | f::rest -> 
      begin 
      match (Unix.lstat f).Unix.st_kind with
          Unix.S_REG -> 
            if (Filename.check_suffix f ".fcl") then 
              begin
                let fc = open_in f in
                let len = in_channel_length fc in 
                let buf = String.create len in
                  really_input fc buf 0 len;
                  close_in fc;                 
                  Hashtbl.replace 
                    srcs 
                    (Filename.chop_extension (Filename.basename f))
                    (String.escaped buf)
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
    Printf.printf "let items : (string,string) Hashtbl.t = Hashtbl.create %d\n" (Hashtbl.length srcs);
    Hashtbl.iter (fun s c -> Printf.printf "let _ = Hashtbl.add items \"%s\" \"%s\"\n%!" s c) srcs

