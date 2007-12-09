open Unix

let usage = "mozillaPreProcess <src> [<dst>]"

let read_file fd str =
  ignore (lseek fd 0 SEEK_SET);
  let rec really_read fd buffer start length =
    if length <= 0 then () else
       match read fd buffer start length with
         0 -> print_endline "We were not able to read all the contents of the file !"; raise End_of_file
       | r -> really_read fd buffer (start + r) (length - r)
  in
  really_read fd str 0 (String.length str)

let myTidy fin fout =
  let fdin = openfile fin [O_RDONLY] 0o007 in
  let size = lseek fdin 0 SEEK_END in
  let buf = String.create size in
  let _ = (read_file fdin buf; close fdin) in
  let newbuf = Str.global_replace (Str.regexp  "<![^>]*>") "" buf in
  let fdout = openfile fout [O_WRONLY; O_CREAT] 0o644 in
  let newsize = String.length newbuf in
  (ignore(write fdout newbuf 0 newsize); ftruncate fdout newsize; close fdout)
    
let _ = match Array.length Sys.argv with
  3 -> handle_unix_error (myTidy Sys.argv.(1)) (Sys.argv.(2))
| 2 -> handle_unix_error (myTidy Sys.argv.(1)) (Sys.argv.(1))
|  _ -> print_endline (Printf.sprintf "Usage : %s" usage)
