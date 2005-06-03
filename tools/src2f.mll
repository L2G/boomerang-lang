(*
 * ML lex script to convert .src files to .pi files.
 *)

{
  type mode = SRC | TEX
  type lineMode = SRC | TEX | NONE

  let mode = ref TEX
  and lineMode = ref NONE
  and newLine = ref true

  let current = ref ""
  let old = ref ""
  let emit s = current := (!current ^ s)

  let suffix = ref "f" 
  let terminator = ref ";" 
  let created_files = ref ""
  let basename = ref ""
  let wholename = ref ""
  let count = ref 0
  let dump() =
     count := !count + 1;
     let outname = !basename (* ^ "." ^ (string_of_int !count) ^ "." ^ !suffix *) in
     created_files := (outname
                       ^ (if !created_files <> "" then " " else "")
                       ^ !created_files);
     let o = open_out outname in
     if !old <> "" then
       (output_string o ("DO printingoff" ^ !terminator ^ "\n");
        output_string o !old;
        output_string o ("DO printingon" ^ !terminator ^ "\n"));
     output_string o !current; close_out o;
     old := !old ^ !current;
     current := ""

  let pr s =
    if (!mode = SRC && !lineMode = NONE) || !lineMode = SRC then
      emit s
    else
      ()
}

rule lex = parse
  eof { }
| "\n" {
    emit "\n"; newLine := true; lineMode := NONE; lex lexbuf
  }
| "#{@}" {
    if !newLine then (newLine := false; mode := TEX)
    else (pr "#{@}"); lex lexbuf
  }
| "#{#}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{#}"); lex lexbuf
  }
| "#{*}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{*}"); lex lexbuf
  }
| "#@" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#@"); lex lexbuf
  }
| "#&" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#&"); lex lexbuf
  }
| "##" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "##"); lex lexbuf
  }
| "#<" ' '* '\n' {
    if !newLine then (dump(); emit "\n") else (pr "#<\n");
    newLine := true; lex lexbuf
  }
(* Nuke the next once things stabilize *)
| "%USECHECKER2" [' ']* "\n" {
    let s = Lexing.lexeme lexbuf in 
    if !newLine then
      (suffix := "ff"; terminator:=";"; emit "\n")
    else
      pr (Lexing.lexeme lexbuf);
    newLine := true; lex lexbuf
  }
| "###" {
    if !newLine then (newLine := false; lineMode := SRC; pr "#")
    else (pr "###"); lex lexbuf
  }
| "#*" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "#*"); lex lexbuf
  }
| _ {
    newLine := false; pr (Lexing.lexeme lexbuf); lex lexbuf
  }

{
  let () =
    if Array.length Sys.argv == 3 then
      (wholename := Array.get Sys.argv 1;
       basename := Array.get Sys.argv 2;
       emit "#line 1 \""; emit !wholename;
       emit "\"\n"; lex (Lexing.from_channel (open_in !wholename)))
    else
      (basename := "tex/f"; 
       lex (Lexing.from_channel stdin));
    if String.length(!current) > 0 then dump();
    print_string !created_files; print_string "\n";
    exit 0
}
