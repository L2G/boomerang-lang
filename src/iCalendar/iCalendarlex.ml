# 1 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
 
(*
   this lexer accepts both \r\n and \n end of line
   (* isn't that too restrictive ? - SL *)
   this lexer requires the version to be 2.0
*)
  
  open ICalendarparse
  open ICalendar_lextypes

(* To buffer string literals *)

let string_start_pos = ref 0;;
let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let store_string s =
  let l = String.length s in
  if !string_index + l > String.length (!string_buff) then begin
    let new_buff = String.create ((String.length (!string_buff) + l) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.blit s 0 (!string_buff) (!string_index) l;
  string_index := (!string_index) + l

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  (*
  print_endline s;
  flush stdout;
  *)
  s

# 52 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"
let __ocaml_lex_init_lexbuf lexbuf mem_size =
  let pos = lexbuf.Lexing.lex_curr_pos in
  lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ;
  lexbuf.Lexing.lex_start_pos <- pos ;
  lexbuf.Lexing.lex_last_pos <- pos ;
  lexbuf.Lexing.lex_last_action <- -1

let rec __ocaml_lex_next_char lexbuf =
  if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then begin
    if lexbuf.Lexing.lex_eof_reached then
      256
    else begin
      lexbuf.Lexing.refill_buff lexbuf ;
      __ocaml_lex_next_char lexbuf
    end
  end else begin
    let i = lexbuf.Lexing.lex_curr_pos in
    let c = lexbuf.Lexing.lex_buffer.[i] in
    lexbuf.Lexing.lex_curr_pos <- i+1 ;
    Char.code c
  end

let rec __ocaml_lex_state0 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 57 ;
  match __ocaml_lex_next_char lexbuf with
(* |eof *)
  |256 ->
    __ocaml_lex_state17 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state1 lexbuf
(* |'V' *)
  |86 ->
    __ocaml_lex_state2 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state3 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state4 lexbuf
(* |'G' *)
  |71 ->
    __ocaml_lex_state5 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state6 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state7 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state8 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state9 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state10 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state11 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state12 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state13 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state14 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state15 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state322 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state2 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state316 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state3 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state311 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state4 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state309 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state5 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state307 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state6 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state289 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state288 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state7 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state280 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state8 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state269 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state9 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state251 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state252 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state253 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state10 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state247 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state248 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state11 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state221 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state219 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state220 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state12 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state183 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state184 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state185 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state186 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state13 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state116 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state117 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state14 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state102 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state101 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state15 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state51 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state52 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state50 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state16 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state18 lexbuf
(* |'Z' *)
  |90 ->
    __ocaml_lex_state19 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state17 lexbuf = (* *)
  56

and __ocaml_lex_state18 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state41 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state42 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state19 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state20 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state21 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state22 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state23 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state20 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state40 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state21 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state38 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state22 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state27 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state23 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state24 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state24 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state25 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state25 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state26 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state26 lexbuf = (* *)
  55

and __ocaml_lex_state27 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state28 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state28 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state29 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state29 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state30 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state30 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state31 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state31 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state32 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state33 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state32 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state37 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state33 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state34 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state34 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state35 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state35 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state36 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state36 lexbuf = (* *)
  54

and __ocaml_lex_state37 lexbuf = (* *)
  53

and __ocaml_lex_state38 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state39 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state39 lexbuf = (* *)
  52

and __ocaml_lex_state40 lexbuf = (* *)
  51

and __ocaml_lex_state41 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state47 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state42 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state43 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state43 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state44 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state44 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state45 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state45 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state46 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state46 lexbuf = (* *)
  49

and __ocaml_lex_state47 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state48 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state48 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state49 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state49 lexbuf = (* *)
  30

and __ocaml_lex_state50 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state98 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state51 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state95 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state52 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state53 lexbuf
(* |'Q' *)
  |81 ->
    __ocaml_lex_state54 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state55 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state56 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state57 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state53 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state85 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state54 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state74 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state55 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state67 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state56 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state61 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state57 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state58 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state58 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state59 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state59 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state60 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state60 lexbuf = (* *)
  50

and __ocaml_lex_state61 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state62 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state62 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state63 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state63 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state64 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state64 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state65 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state65 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state66 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state66 lexbuf = (* *)
  45

and __ocaml_lex_state67 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state68 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state68 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state69 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state69 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state70 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state70 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state71 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state71 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state72 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state72 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state73 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state73 lexbuf = (* *)
  44

and __ocaml_lex_state74 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state75 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state75 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state76 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state76 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state77 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state77 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state78 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state78 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state79 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state79 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state80 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state80 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state81 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state81 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state82 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state82 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state83 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state83 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state84 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state84 lexbuf = (* *)
  43

and __ocaml_lex_state85 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state86 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state86 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state87 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state87 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state88 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state88 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state89 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state89 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state90 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state90 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state91 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state91 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state92 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state92 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state93 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state93 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state94 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state94 lexbuf = (* *)
  33

and __ocaml_lex_state95 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state96 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state96 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state97 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state97 lexbuf = (* *)
  47

and __ocaml_lex_state98 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state99 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state99 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state100 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state100 lexbuf = (* *)
  46

and __ocaml_lex_state101 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state107 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state102 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state103 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state103 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state104 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state104 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state105 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state105 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state106 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state106 lexbuf = (* *)
  48

and __ocaml_lex_state107 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state108 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state109 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state108 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state114 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state109 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state110 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state110 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state111 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state111 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state112 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state112 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state113 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state113 lexbuf = (* *)
  37

and __ocaml_lex_state114 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state115 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state115 lexbuf = (* *)
  36

and __ocaml_lex_state116 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state126 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state117 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state119 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state118 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state118 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state123 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state119 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state120 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state120 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state121 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state121 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state122 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state122 lexbuf = (* *)
  42

and __ocaml_lex_state123 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state124 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state124 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state125 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state125 lexbuf = (* *)
  41

and __ocaml_lex_state126 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |':' *)
  |58 ->
    __ocaml_lex_state127 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state127 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state128 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state129 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state130 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state128 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state149 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state150 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state151 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state152 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state129 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state140 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state130 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state131 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state131 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state132 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state132 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state133 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state133 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state134 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state134 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state135 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state135 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state136 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state136 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state137 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state137 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state138 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state139 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state138 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state139 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state139 lexbuf = (* *)
  11

and __ocaml_lex_state140 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state141 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state141 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state142 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state142 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state143 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state143 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state144 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state144 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state145 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state145 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state146 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state146 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state147 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state148 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state147 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state148 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state148 lexbuf = (* *)
  9

and __ocaml_lex_state149 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state174 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state150 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state168 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state151 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state162 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state152 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state153 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state153 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state154 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state154 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state155 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state155 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Z' *)
  |90 ->
    __ocaml_lex_state156 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state156 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state157 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state157 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state158 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state158 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state159 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state159 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state160 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state161 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state160 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state161 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state161 lexbuf = (* *)
  7

and __ocaml_lex_state162 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state163 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state163 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state164 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state164 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state165 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state165 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state166 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state167 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state166 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state167 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state167 lexbuf = (* *)
  5

and __ocaml_lex_state168 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state169 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state169 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state170 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state170 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state171 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state171 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state172 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state173 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state172 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state173 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state173 lexbuf = (* *)
  3

and __ocaml_lex_state174 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state175 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state175 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state176 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state176 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state177 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state177 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state178 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state178 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state179 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state179 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state180 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state180 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state181 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state181 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state182 lexbuf = (* *)
  1

and __ocaml_lex_state183 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state216 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state184 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state211 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state185 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state197 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state198 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state186 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state187 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state188 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state187 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state193 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state188 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state189 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state189 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state190 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state190 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state191 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state191 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state192 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state192 lexbuf = (* *)
  40

and __ocaml_lex_state193 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state194 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state194 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state195 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state195 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state196 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state196 lexbuf = (* *)
  39

and __ocaml_lex_state197 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state206 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state198 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state199 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state199 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state200 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state200 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state201 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state201 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state202 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state202 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state203 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state203 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state204 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state204 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state205 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state205 lexbuf = (* *)
  38

and __ocaml_lex_state206 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state207 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state207 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state208 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state208 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state209 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state209 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state210 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state210 lexbuf = (* *)
  14

and __ocaml_lex_state211 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state212 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state212 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state213 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state213 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state214 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state214 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state215 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state215 lexbuf = (* *)
  18

and __ocaml_lex_state216 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state217 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state217 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state218 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state218 lexbuf = (* *)
  17

and __ocaml_lex_state219 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state238 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state220 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state228 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state229 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state221 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state222 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state222 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state223 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state223 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state224 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state224 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state225 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state225 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state226 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state226 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state227 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state227 lexbuf = (* *)
  35

and __ocaml_lex_state228 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state232 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state229 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state230 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state230 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state231 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state231 lexbuf = (* *)
  34

and __ocaml_lex_state232 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state233 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state233 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state234 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state235 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state234 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state237 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state235 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state236 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state236 lexbuf = (* *)
  26

and __ocaml_lex_state237 lexbuf = (* *)
  20

and __ocaml_lex_state238 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state239 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state239 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state240 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state240 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state241 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state241 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state242 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state242 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state243 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state243 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state244 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state244 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state245 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state245 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state246 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state246 lexbuf = (* *)
  19

and __ocaml_lex_state247 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state250 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state248 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state249 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state249 lexbuf = (* *)
  32

and __ocaml_lex_state250 lexbuf = (* *)
  31

and __ocaml_lex_state251 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Q' *)
  |81 ->
    __ocaml_lex_state263 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state252 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state259 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state253 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state254 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state254 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state255 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state255 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state256 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state256 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state257 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state257 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state258 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state258 lexbuf = (* *)
  29

and __ocaml_lex_state259 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state260 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state260 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state261 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state261 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state262 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state262 lexbuf = (* *)
  28

and __ocaml_lex_state263 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state264 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state264 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state265 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state265 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state266 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state266 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state267 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state267 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state268 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state268 lexbuf = (* *)
  27

and __ocaml_lex_state269 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state270 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state271 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state270 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state277 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state271 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state272 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state272 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state273 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state273 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state274 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state274 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state275 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state275 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state276 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state276 lexbuf = (* *)
  25

and __ocaml_lex_state277 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state278 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state278 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state279 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state279 lexbuf = (* *)
  12

and __ocaml_lex_state280 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state281 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state281 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state282 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state282 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state283 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state283 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state284 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state284 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Z' *)
  |90 ->
    __ocaml_lex_state285 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state285 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state286 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state286 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state287 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state287 lexbuf = (* *)
  24

and __ocaml_lex_state288 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state296 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state289 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state290 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state290 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state291 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state291 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state292 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state292 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state293 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state293 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state294 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state294 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state295 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state295 lexbuf = (* *)
  23

and __ocaml_lex_state296 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state297 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state297 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state298 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state298 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state299 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state299 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state300 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state300 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state301 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state301 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state302 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state302 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state303 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state303 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state304 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state304 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state305 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state305 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state306 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state306 lexbuf = (* *)
  22

and __ocaml_lex_state307 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state308 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state308 lexbuf = (* *)
  21

and __ocaml_lex_state309 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state310 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state310 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 16 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state310 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state311 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state312 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state312 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state313 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state313 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state314 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state314 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state315 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state315 lexbuf = (* *)
  15

and __ocaml_lex_state316 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state317 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state317 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state318 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state318 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state319 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state319 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state320 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state320 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state321 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state321 lexbuf = (* *)
  13

and __ocaml_lex_state322 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state323 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state323 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state324 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state324 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state325 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state325 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |':' *)
  |58 ->
    __ocaml_lex_state326 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state326 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state327 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state328 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state329 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state327 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state348 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state349 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state350 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state351 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state328 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state339 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state329 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state330 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state330 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state331 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state331 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state332 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state332 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state333 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state333 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state334 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state334 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state335 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state335 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state336 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state336 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state337 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state338 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state337 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state338 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state338 lexbuf = (* *)
  10

and __ocaml_lex_state339 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state340 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state340 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state341 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state341 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state342 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state342 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state343 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state343 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state344 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state344 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state345 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state345 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state346 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state347 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state346 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state347 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state347 lexbuf = (* *)
  8

and __ocaml_lex_state348 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state373 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state349 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state367 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state350 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state361 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state351 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state352 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state352 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state353 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state353 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state354 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state354 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Z' *)
  |90 ->
    __ocaml_lex_state355 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state355 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state356 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state356 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state357 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state357 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state358 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state358 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state359 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state360 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state359 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state360 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state360 lexbuf = (* *)
  6

and __ocaml_lex_state361 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state362 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state362 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state363 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state363 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state364 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state364 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state365 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state366 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state365 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state366 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state366 lexbuf = (* *)
  4

and __ocaml_lex_state367 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state368 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state368 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state369 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state369 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state370 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state370 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state371 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state371 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state372 lexbuf = (* *)
  2

and __ocaml_lex_state373 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state374 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state374 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state375 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state375 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state376 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state376 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state377 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state377 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state378 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state378 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state379 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state379 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\013' *)
  |13 ->
    __ocaml_lex_state380 lexbuf
(* |'\n' *)
  |10 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state380 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state381 lexbuf = (* *)
  0

and __ocaml_lex_state382 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\t'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state382 lexbuf

and __ocaml_lex_state383 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |eof *)
  |256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |':' *)
  |58 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    __ocaml_lex_state182 lexbuf

and __ocaml_lex_state384 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |eof *)
  |256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\n' *)
  |10 ->
    __ocaml_lex_state381 lexbuf
(* |'\013' *)
  |13 ->
    __ocaml_lex_state385 lexbuf
  | _ ->
    __ocaml_lex_state182 lexbuf

and __ocaml_lex_state385 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\n' *)
  |10 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state386 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |eof *)
  |256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'"' *)
  |34 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    __ocaml_lex_state182 lexbuf

and __ocaml_lex_state387 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |';' *)
  |59 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state388 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |';' *)
  |59 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state389 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |eof *)
  |256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'2' *)
  |50 ->
    __ocaml_lex_state390 lexbuf
  | _ ->
    __ocaml_lex_state182 lexbuf

and __ocaml_lex_state390 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'.' *)
  |46 ->
    __ocaml_lex_state391 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state391 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0' *)
  |48 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state392 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'%'|'('|')'|','|'-'|'.'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|':'|'<'|'='|'>'|'?'|'@'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'\\'|'_'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|'~' *)
  |37|40|41|44|45|46|47|48|49|50|51|52|53|54|55|56|57|58|60|61|62|63|64|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|92|95|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122|126 ->
    __ocaml_lex_state392 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state393 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state394 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |';' *)
  |59 ->
    __ocaml_lex_state395 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state395 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'X' *)
  |88 ->
    __ocaml_lex_state396 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state396 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state397 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state397 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state398 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state398 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state398 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state399 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state400 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|','|':'|';'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|44|58|59|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'"' *)
  |34 ->
    __ocaml_lex_state402 lexbuf
  | _ ->
    __ocaml_lex_state401 lexbuf

and __ocaml_lex_state401 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|'"'|','|':'|';'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|34|44|58|59|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state401 lexbuf

and __ocaml_lex_state402 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'"' *)
  |34 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    __ocaml_lex_state402 lexbuf

and __ocaml_lex_state403 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state404 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state404 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state405 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|'"'|','|':'|';'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|34|44|58|59|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state405 lexbuf

and __ocaml_lex_state406 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state408 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state409 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state410 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state407 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state407 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state430 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state408 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state426 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state409 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state413 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state414 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state410 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state411 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state411 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'X' *)
  |88 ->
    __ocaml_lex_state412 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state412 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state413 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state420 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state414 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state415 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state415 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state416 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state416 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state417 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state417 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state418 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state418 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state419 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state419 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state420 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state421 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state421 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state422 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state422 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state423 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state423 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state424 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state424 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state425 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state425 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state426 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state427 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state427 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state428 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state428 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state429 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state429 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state430 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state431 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state431 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state432 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state432 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state433 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state433 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state434 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'9'|'A'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|57|65|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state436 lexbuf
(* |'8' *)
  |56 ->
    __ocaml_lex_state437 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state438 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state435 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state436 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state442 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state437 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state440 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state438 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state439 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state439 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state435 lexbuf
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state439 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state440 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state441 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state441 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state442 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state443 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state443 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state444 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state444 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'6' *)
  |54 ->
    __ocaml_lex_state445 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state445 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state435 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state446 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state447 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state448 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state447 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state447 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state448 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state447 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state449 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state449 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state447 lexbuf
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state449 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state450 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state451 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state452 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state453 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state454 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state455 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state456 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state451 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state502 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state452 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state491 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state453 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state471 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state472 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state454 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state463 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state455 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state460 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state456 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state457 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state457 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state458 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state458 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state459 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state459 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state460 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state461 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state461 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state462 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state462 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state463 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state464 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state464 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state465 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state465 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state466 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state466 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state467 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state467 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state468 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state468 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state469 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state469 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state470 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state470 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state471 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state485 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state472 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state473 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state474 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state473 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state480 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state474 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state475 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state475 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state476 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state476 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state477 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state477 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state478 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state478 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state479 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state479 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state480 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state481 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state481 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state482 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state482 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state483 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state483 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state484 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state484 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state485 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state486 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state486 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state487 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state487 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state488 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state488 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state489 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state489 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state490 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state490 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state491 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state492 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state492 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state493 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state493 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state494 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state494 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state495 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state495 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state496 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state496 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state497 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state497 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state498 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state498 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state499 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state499 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state500 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state500 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state501 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state501 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state502 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state503 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state503 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state504 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state504 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state505 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state505 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state506 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state506 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state507 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state507 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state508 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state508 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state509 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state510 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state511 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state510 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state516 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state511 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state512 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state512 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state513 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state513 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Q' *)
  |81 ->
    __ocaml_lex_state514 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state514 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state515 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state515 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state516 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state517 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state517 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state518 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state518 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state519 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state519 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state520 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state520 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state521 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state521 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state522 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state522 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state523 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state523 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state524 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state524 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state525 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state527 lexbuf
(* |'G' *)
  |71 ->
    __ocaml_lex_state528 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state529 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state530 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state531 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|72|74|75|76|77|78|79|80|81|83|84|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state526 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state527 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state549 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state528 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state546 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state529 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state538 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state539 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state530 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state533 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state531 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state532 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state532 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state532 lexbuf
(* |'/' *)
  |47 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state533 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'K' *)
  |75 ->
    __ocaml_lex_state534 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state534 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state535 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state535 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state536 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state536 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'W' *)
  |87 ->
    __ocaml_lex_state537 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state537 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state538 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state541 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state539 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state540 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state540 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state541 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state542 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state542 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state543 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state543 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state544 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state544 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state545 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state545 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state546 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state547 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state547 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state548 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state548 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state549 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state550 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state550 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state551 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state551 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state552 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state552 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state553 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state553 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state554 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state554 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state555 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state555 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state556 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state556 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state526 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state557 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state558 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state558 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state559 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state561 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state562 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state563 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state564 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state565 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|80|81|83|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state560 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state561 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state606 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state562 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state593 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state563 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state580 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state564 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state567 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state565 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state566 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state566 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state566 lexbuf
(* |'/' *)
  |47 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state567 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state568 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state568 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state569 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state569 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state570 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state570 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state571 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state571 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state572 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state572 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state573 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state573 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state574 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state574 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state575 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state575 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state576 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state576 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state577 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state577 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state578 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state578 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state579 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state579 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state580 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state581 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state581 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state582 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state582 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state583 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state583 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state584 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state584 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state585 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state585 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state586 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state586 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state587 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state587 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state588 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state588 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state589 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state589 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state590 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state590 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state591 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state591 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state592 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state592 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state593 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'Q' *)
  |81 ->
    __ocaml_lex_state594 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state594 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state595 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state595 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state596 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state596 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state597 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state597 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state598 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state598 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state599 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state599 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state600 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state600 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state601 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state601 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state602 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state602 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state603 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state603 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state604 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state604 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state605 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state605 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state606 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state607 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state607 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state608 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state608 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state560 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state609 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state612 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state613 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state614 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state615 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state616 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state617 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|69|70|71|72|74|75|76|77|79|80|81|82|83|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state611 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state610 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state611 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state659 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state612 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state653 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state613 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state646 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state614 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state634 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state615 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state627 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state616 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state619 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state617 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state618 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state618 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state618 lexbuf
(* |'/' *)
  |47 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state619 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state620 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state620 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state621 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state621 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state622 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state622 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state623 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state623 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state624 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state624 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state625 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state625 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state626 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state626 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state627 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state628 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state628 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state629 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state629 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state630 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state630 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state631 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state631 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state632 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state632 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state633 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state633 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state634 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state635 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state636 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state635 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state642 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state636 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state637 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state637 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state638 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state638 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state639 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state639 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state640 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state640 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state641 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state641 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state642 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state643 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state643 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state644 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state644 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state645 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state645 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state646 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state647 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state647 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state648 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state648 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state649 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state649 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state650 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state650 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state651 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state651 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state652 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state652 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state653 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state654 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state654 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state655 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state655 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state656 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state656 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state657 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state657 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state658 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state658 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state659 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state660 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state660 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state661 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state661 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state662 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state662 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state663 lexbuf
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state663 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state664 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state664 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state665 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state665 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state666 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state666 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state667 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state667 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state668 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state668 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state610 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state669 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|81|82|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state671 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state672 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state673 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state674 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state670 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state671 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state684 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state672 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state681 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state673 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state676 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state674 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state675 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state675 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state670 lexbuf
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state675 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state676 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state677 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state677 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state678 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state678 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state679 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state679 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state680 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state680 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state681 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state682 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state682 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state683 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state683 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state684 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state685 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state685 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state686 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state686 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state687 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state687 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state670 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state688 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state689 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state689 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state690 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state690 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state691 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state691 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state692 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state692 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state693 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state693 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state694 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state694 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state695 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state695 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state697 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state696 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state696 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state702 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state697 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state698 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state698 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state699 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state699 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state700 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state700 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state701 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state701 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state702 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state703 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state703 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state704 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state704 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state705 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state706 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state707 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state706 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state709 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state707 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state708 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state708 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state709 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state710 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state710 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state711 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state711 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state712 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 23 ;
  match __ocaml_lex_next_char lexbuf with
(* |';' *)
  |59 ->
    __ocaml_lex_state713 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state713 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state714 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state715 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state716 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state717 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state718 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state719 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state720 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state721 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state722 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state723 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state724 lexbuf
(* |'V' *)
  |86 ->
    __ocaml_lex_state725 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state726 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state714 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state839 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state715 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state833 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state834 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state716 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state813 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state814 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state717 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state806 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state718 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state800 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state719 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state793 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state720 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state787 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state721 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state780 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state722 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state744 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state745 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state746 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state747 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state723 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state737 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state724 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Z' *)
  |90 ->
    __ocaml_lex_state733 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state725 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state729 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state726 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state727 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state727 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state728 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state728 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 22 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state728 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state729 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state730 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state730 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state731 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state731 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state732 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state732 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state308 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state733 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state734 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state734 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state735 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state735 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state736 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state736 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 20 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state246 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state737 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state738 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state738 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state739 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state739 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state740 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state740 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'B' *)
  |66 ->
    __ocaml_lex_state741 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state741 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state742 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state742 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state743 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state743 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state215 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state744 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state777 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state745 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state761 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state746 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state750 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state747 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state748 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state748 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state749 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state749 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state218 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state750 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state751 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state751 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state752 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state752 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state753 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state754 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state753 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state759 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state754 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state755 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state755 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state756 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state756 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state757 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state757 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state758 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state758 lexbuf = (* *)
  16

and __ocaml_lex_state759 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state760 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state760 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state315 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state761 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state762 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state763 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state762 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state767 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state763 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state764 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state764 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state765 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state765 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state766 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state766 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state210 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state767 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state768 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state768 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state769 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state769 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state770 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state770 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state771 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state772 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state771 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state774 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state772 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state773 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state773 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state321 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state774 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state775 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state775 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state776 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state776 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state279 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state777 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state778 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state778 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state779 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state779 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state139 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state780 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state781 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state781 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state782 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state782 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state783 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state783 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state784 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state784 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state785 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state785 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state786 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state786 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state338 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state787 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state788 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state788 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'B' *)
  |66 ->
    __ocaml_lex_state789 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state789 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state790 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state790 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state791 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state791 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state792 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state792 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state148 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state793 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state794 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state794 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state795 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state795 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state796 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state796 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state797 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state797 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state798 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state798 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state799 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state799 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state347 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state800 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state801 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state801 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state802 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state802 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state803 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state803 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state804 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state804 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state805 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state805 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state161 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state806 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state807 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state807 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state808 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state808 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state809 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state809 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state810 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state810 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state811 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state811 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state812 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state812 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state360 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state813 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state817 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state814 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state815 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state815 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state816 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state816 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state167 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state817 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state818 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state818 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'G' *)
  |71 ->
    __ocaml_lex_state819 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state819 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state820 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state820 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state821 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state821 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state822 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state822 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state823 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state823 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state824 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state824 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state825 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state826 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state825 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state829 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state826 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state827 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state827 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state828 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state828 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state366 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state829 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state830 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state830 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state831 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state831 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state832 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state832 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state173 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state833 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state834 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state835 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state835 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state836 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state836 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state837 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state837 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state838 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state838 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state839 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state840 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state840 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state841 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state841 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state842 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state842 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state843 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state843 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state844 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state844 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state845 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state846 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state846 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state846 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state847 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state848 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state848 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state849 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 3 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state850 lexbuf
(* |'+' *)
  |43 ->
    __ocaml_lex_state851 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state852 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state850 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state854 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state851 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state853 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state852 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state853 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state854 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state855 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state856 lexbuf
(* |'+' *)
  |43 ->
    __ocaml_lex_state857 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state858 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state856 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state860 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state857 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state859 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state858 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state859 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state860 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state861 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state862 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state863 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state864 lexbuf
(* |'+' *)
  |43 ->
    __ocaml_lex_state865 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state866 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state864 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state870 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state865 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state868 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state866 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state867 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state867 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state868 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state869 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state869 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state870 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state871 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state871 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state872 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state873 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state874 lexbuf
(* |'W' *)
  |87 ->
    __ocaml_lex_state875 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state876 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state877 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state878 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state874 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state875 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state876 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H'|'U' *)
  |72|85 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state877 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state878 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A'|'U' *)
  |65|85 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state879 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state880 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 14 ;
  match __ocaml_lex_next_char lexbuf with
(* |';' *)
  |59 ->
    __ocaml_lex_state881 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state881 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state884 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state885 lexbuf
(* |'W' *)
  |87 ->
    __ocaml_lex_state886 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state887 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state882 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state883 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state882 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state948 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state883 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state944 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state884 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state937 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state885 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state893 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state886 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'K' *)
  |75 ->
    __ocaml_lex_state890 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state887 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state888 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state888 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state889 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state889 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 13 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state889 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state890 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state891 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state891 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state892 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state892 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state279 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state893 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state894 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state895 lexbuf
(* |'Y' *)
  |89 ->
    __ocaml_lex_state896 lexbuf
(* |'W' *)
  |87 ->
    __ocaml_lex_state897 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state898 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state899 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state894 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state934 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state895 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state932 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state896 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state926 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state897 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state921 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state898 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state909 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state910 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state899 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state900 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state900 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state901 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state902 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state901 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state906 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state902 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state903 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state903 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state904 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state904 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state905 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state905 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state139 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state906 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state907 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state907 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state908 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state908 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state173 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state909 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state917 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state910 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state911 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state911 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state912 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state912 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state913 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state913 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state914 lexbuf
(* |'=' *)
  |61 ->
    __ocaml_lex_state338 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state914 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state915 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state915 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state916 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state916 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state161 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state917 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state918 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state918 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state919 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state919 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state920 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state920 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state366 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state921 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state922 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state922 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'K' *)
  |75 ->
    __ocaml_lex_state923 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state923 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state924 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state924 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state925 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state925 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state148 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state926 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state927 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state927 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state928 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state928 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state929 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state929 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state930 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state930 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state931 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state931 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state347 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state932 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state933 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state933 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state360 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state934 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state935 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state935 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state936 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state936 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state167 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state937 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state938 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state938 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state939 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state939 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state940 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state940 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'V' *)
  |86 ->
    __ocaml_lex_state941 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state941 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state942 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state942 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state943 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state943 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state944 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state945 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state945 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state946 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state946 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state947 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state947 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state948 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state949 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state949 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state950 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state950 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state951 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state951 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state952 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state953 lexbuf
(* |'H' *)
  |72 ->
    __ocaml_lex_state954 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state955 lexbuf
(* |'W' *)
  |87 ->
    __ocaml_lex_state956 lexbuf
(* |'M' *)
  |77 ->
    __ocaml_lex_state957 lexbuf
(* |'Y' *)
  |89 ->
    __ocaml_lex_state958 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state953 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state985 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state954 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state981 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state955 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state978 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state956 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state974 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state957 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state963 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state964 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state958 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state959 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state959 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state960 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state960 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state961 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state961 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state962 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state962 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state963 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state969 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state964 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state965 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state965 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state966 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state966 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'H' *)
  |72 ->
    __ocaml_lex_state967 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state967 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state968 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state968 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state969 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state970 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state970 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state971 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state971 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state972 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state972 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state973 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state973 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state974 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state975 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state975 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'K' *)
  |75 ->
    __ocaml_lex_state976 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state976 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state977 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state977 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state978 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state979 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state979 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state980 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state980 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state981 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state982 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state982 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state983 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state983 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state984 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state984 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state985 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state986 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state986 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state987 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state987 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'N' *)
  |78 ->
    __ocaml_lex_state988 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state988 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state989 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state989 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'L' *)
  |76 ->
    __ocaml_lex_state990 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state990 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Y' *)
  |89 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state991 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state992 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state992 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state993 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state993 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state994 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state994 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'Q' *)
  |81 ->
    __ocaml_lex_state995 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state995 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state996 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 3 ;
  match __ocaml_lex_next_char lexbuf with
(* |'\000'|'\001'|'\002'|'\003'|'\004'|'\005'|'\006'|'\007'|'\008'|'\t'|'\n'|'\011'|'\012'|'\013'|'\014'|'\015'|'\016'|'\017'|'\018'|'\019'|'\020'|'\021'|'\022'|'\023'|'\024'|'\025'|'\026'|'\027'|'\028'|'\029'|'\030'|'\031'|','|';'|'\127'|'\249'|'\250'|'\251'|'\252'|'\253'|'\254'|'\255'|eof *)
  |0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|44|59|127|249|250|251|252|253|254|255|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\\' *)
  |92 ->
    __ocaml_lex_state997 lexbuf
  | _ ->
    __ocaml_lex_state372 lexbuf

and __ocaml_lex_state997 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |','|';'|'N'|'\\'|'n' *)
  |44|59|78|92|110 ->
    __ocaml_lex_state381 lexbuf
(* |'"' *)
  |34 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state998 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state999 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state999 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1000 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1000 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1001 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1001 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1002 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1003 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1003 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1004 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1004 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state1005 lexbuf
(* |'+'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |43|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1006 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1005 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1006 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1007 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1008 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1008 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1008 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1009 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'G' *)
  |71 ->
    __ocaml_lex_state1011 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1010 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1011 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state1012 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1012 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state1013 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1013 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'G' *)
  |71 ->
    __ocaml_lex_state1014 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1014 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state1015 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1015 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state1016 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1016 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state1017 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1017 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state1018 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1018 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1010 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1019 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state1021 lexbuf
(* |'C' *)
  |67 ->
    __ocaml_lex_state1022 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state1023 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1020 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1021 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state1035 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state1036 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1022 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state1025 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1023 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state1024 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1024 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state1020 lexbuf
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1024 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1025 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state1026 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1026 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'F' *)
  |70 ->
    __ocaml_lex_state1027 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1027 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state1028 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1028 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state1029 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1029 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state1030 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1030 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'N' *)
  |78 ->
    __ocaml_lex_state1031 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1031 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state1032 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1032 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state1033 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1033 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state1034 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1034 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1035 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'B' *)
  |66 ->
    __ocaml_lex_state1041 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1036 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state1037 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1037 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'V' *)
  |86 ->
    __ocaml_lex_state1038 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1038 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state1039 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1039 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'T' *)
  |84 ->
    __ocaml_lex_state1040 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1040 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1041 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state1042 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1042 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state1043 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1043 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1020 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1044 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1045 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1045 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1046 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1047 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1047 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1048 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'+' *)
  |43 ->
    __ocaml_lex_state381 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state182 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1049 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1050 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1050 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1051 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1051 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1052 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1052 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1053 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1054 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1055 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1056 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'Z' *)
  |90 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1057 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |',' *)
  |44 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1058 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'+'|'-' *)
  |43|45 ->
    __ocaml_lex_state1059 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1060 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1059 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1060 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1060 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1060 lexbuf
(* |'.' *)
  |46 ->
    __ocaml_lex_state1061 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1061 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1062 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1062 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1062 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1063 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state1064 lexbuf
(* |'+' *)
  |43 ->
    __ocaml_lex_state1065 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1066 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1064 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1068 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1065 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1067 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1066 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 2 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1066 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1067 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1067 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1068 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1068 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1069 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 5 ;
  match __ocaml_lex_next_char lexbuf with
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1070 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1070 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state173 lexbuf
(* |'W' *)
  |87 ->
    __ocaml_lex_state381 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state366 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state182 lexbuf
(* |'H' *)
  |72 ->
    __ocaml_lex_state372 lexbuf
(* |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' *)
  |48|49|50|51|52|53|54|55|56|57 ->
    __ocaml_lex_state1070 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1071 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 4 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state1072 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state1073 lexbuf
(* |'+' *)
  |43 ->
    __ocaml_lex_state1074 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1072 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state1076 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1073 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 3 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1074 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'P' *)
  |80 ->
    __ocaml_lex_state1075 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1075 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 3 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state372 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1076 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1077 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 1 ;
  match __ocaml_lex_next_char lexbuf with
(* |'.' *)
  |46 ->
    __ocaml_lex_state381 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1078 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state1080 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state1081 lexbuf
(* |'E' *)
  |69 ->
    __ocaml_lex_state1082 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state1083 lexbuf
(* |'X' *)
  |88 ->
    __ocaml_lex_state1084 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1079 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1080 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state1101 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1081 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state1096 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1082 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'M' *)
  |77 ->
    __ocaml_lex_state1093 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1083 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'R' *)
  |82 ->
    __ocaml_lex_state1086 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1084 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'-' *)
  |45 ->
    __ocaml_lex_state1085 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1085 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'/' *)
  |47 ->
    __ocaml_lex_state1079 lexbuf
(* |'-'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1085 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1086 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'O' *)
  |79 ->
    __ocaml_lex_state1087 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1087 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'C' *)
  |67 ->
    __ocaml_lex_state1088 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1088 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'E' *)
  |69 ->
    __ocaml_lex_state1089 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1089 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'D' *)
  |68 ->
    __ocaml_lex_state1090 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1090 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'U' *)
  |85 ->
    __ocaml_lex_state1091 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1091 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'R' *)
  |82 ->
    __ocaml_lex_state1092 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1092 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1093 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'A' *)
  |65 ->
    __ocaml_lex_state1094 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1094 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'I' *)
  |73 ->
    __ocaml_lex_state1095 lexbuf
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1095 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1096 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'S' *)
  |83 ->
    __ocaml_lex_state1097 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1097 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'P' *)
  |80 ->
    __ocaml_lex_state1098 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1098 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'L' *)
  |76 ->
    __ocaml_lex_state1099 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1099 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'A' *)
  |65 ->
    __ocaml_lex_state1100 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1100 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1101 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'D' *)
  |68 ->
    __ocaml_lex_state1102 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1102 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state1103 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state1103 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 0 ;
  match __ocaml_lex_next_char lexbuf with
(* |'-'|'/'|'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' *)
  |45|47|48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|71|72|73|74|75|76|77|78|79|80|81|82|83|84|85|86|87|88|89|90|97|98|99|100|101|102|103|104|105|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120|121|122 ->
    __ocaml_lex_state1079 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

let rec line lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 73 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BVCALENDAR )
# 10660 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 74 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EVCALENDAR )
# 10665 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 75 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BVEVENT )
# 10670 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 76 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EVEVENT )
# 10675 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 4 ->
# 77 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BVALARM )
# 10680 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 5 ->
# 78 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EVALARM )
# 10685 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 6 ->
# 79 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BVTIMEZONE )
# 10690 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 7 ->
# 80 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EVTIMEZONE )
# 10695 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 8 ->
# 81 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BSTANDARD )
# 10700 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 9 ->
# 82 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( ESTANDARD )
# 10705 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 10 ->
# 83 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( BDAYLIGHT )
# 10710 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 11 ->
# 84 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EDAYLIGHT )
# 10715 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 12 ->
# 85 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let pidval = text lexbuf in
                            crlf lexbuf;
                            PRODID (xplist, pidval) )
# 10724 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 13 ->
# 90 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            version2 lexbuf;
                            crlf lexbuf;
                            VERSION (xplist,"2.0") )
# 10733 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 14 ->
# 95 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let calval = calvalue lexbuf in
                            crlf lexbuf;
                            CALSCALE (xplist, calval) )
# 10742 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 15 ->
# 100 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let metval = iana_token lexbuf in
                            crlf lexbuf;
                            METHOD (xplist, metval) )
# 10751 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 16 ->
# 105 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let name = Lexing.lexeme lexbuf in
                            let params = all_param_list lexbuf in 
                            col lexbuf;
                            let txt = text lexbuf in
                            crlf lexbuf;
                            XPROP (name, params, txt) )
# 10761 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 17 ->
# 111 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let classval = classvalue lexbuf in
                            crlf lexbuf;
                            CLASS (xplist, classval) )
# 10770 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 18 ->
# 116 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let dt = date_time lexbuf in
                            crlf lexbuf;
                            CREATED (xplist, dt) )
# 10779 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 19 ->
# 121 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            DESCRIPTION (params, text) )
# 10788 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 20 ->
# 126 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let dval = dtp lexbuf in
                            crlf lexbuf;
                            DTSTART (params, dval) )
# 10797 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 21 ->
# 131 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let coord = geoval lexbuf in
                            crlf lexbuf;
                            GEO(xplist, coord) )
# 10806 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 22 ->
# 136 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let dt = date_time lexbuf in
                            crlf lexbuf;
                            LASTMODIFIED(xplist, dt) )
# 10815 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 23 ->
# 141 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            LOCATION (params, text) )
# 10824 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 24 ->
# 146 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let uri = uri lexbuf in
                            crlf lexbuf;
                            ORGANIZER (params, uri) )
# 10833 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 25 ->
# 151 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let pr = integer_val lexbuf in
                            crlf lexbuf;
                            PRIORITY (xplist, pr) )
# 10842 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 26 ->
# 156 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let allparams = all_param_list lexbuf in
                            col lexbuf;
                            let dt = dtp lexbuf in
                            crlf lexbuf;
                            DTSTAMP(allparams, dt) )
# 10851 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 27 ->
# 161 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let seq = integer_val lexbuf in
                            crlf lexbuf;
                            SEQUENCE (xplist, seq) )
# 10860 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 28 ->
# 166 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let status = status lexbuf in
                            crlf lexbuf;
                            STATUS (xplist, status) )
# 10869 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 29 ->
# 171 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let txt = text lexbuf in
                            crlf lexbuf;
                            SUMMARY (params, txt) )
# 10878 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 30 ->
# 176 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let transval = transvalue lexbuf in
                            crlf lexbuf;
                            TRANSP (xplist, transval) )
# 10887 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 31 ->
# 181 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let uid = text lexbuf in
                            crlf lexbuf;
                            UID (xplist, uid) )
# 10896 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 32 ->
# 186 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let url = uri lexbuf in
                            crlf lexbuf;
                            URL (xplist, url) )
# 10905 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 33 ->
# 191 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let rval = dtp lexbuf in
                            crlf lexbuf;
                            RECURRENCEID (params, rval) )
# 10914 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 34 ->
# 196 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let dval = dtp lexbuf in
                            crlf lexbuf;
                            DTEND (params, dval) )
# 10923 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 35 ->
# 201 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let duration = durval lexbuf in
                            crlf lexbuf;
                            DURATION (xplist, duration) )
# 10932 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 36 ->
# 206 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let aval =  
                              if List.exists (fun elt -> elt = Valuetypeparam ICalendar_syntax.Binary) params
                              then ICalendar_syntax.AttBinary (binary lexbuf)
                              else ICalendar_syntax.AttUri (uri lexbuf)
                            in
                            crlf lexbuf;
                            ATTACH (params, aval) )
# 10945 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 37 ->
# 215 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let aval = uri lexbuf in
                            crlf lexbuf;
                            ATTENDEE (params, aval) )
# 10954 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 38 ->
# 220 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            let tl = text_list lexbuf in
                            crlf lexbuf;
                            CATEGORIES (params, t :: tl) )
# 10964 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 39 ->
# 226 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            COMMENT (params, text) )
# 10973 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 40 ->
# 231 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            CONTACT (params, text) )
# 10982 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 41 ->
# 236 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            let vl = dtp_list lexbuf in
                            crlf lexbuf;
                            EXDATE (params, (v::vl)) )
# 10992 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 42 ->
# 242 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = recur lexbuf in
                            crlf lexbuf;
                            EXRULE (xpl, v) )
# 11001 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 43 ->
# 247 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let i = integer_val lexbuf in
                            let il = dot_integer_list lexbuf in
                            semi lexbuf;
                            let sd = text lexbuf in
                            let ed =
                            if is_semi lexbuf then
                              Some (text lexbuf)
                            else 
                              None
                            in
                            crlf lexbuf;
                            RSTATUS (params, (i::il), sd, ed) )
# 11019 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 44 ->
# 261 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let text = text lexbuf in
                            crlf lexbuf;
                            RELATED_TO (params, text) )
# 11028 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 45 ->
# 266 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            let tl = text_list lexbuf in
                            crlf lexbuf;
                            RESOURCES (params, t :: tl) )
# 11038 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 46 ->
# 272 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            let vl = dtp_list lexbuf in
                            crlf lexbuf;
                            RDATE (params, (v::vl)) )
# 11048 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 47 ->
# 278 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = recur lexbuf in
                            crlf lexbuf;
                            RRULE (xpl, v) )
# 11057 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 48 ->
# 283 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = actionval lexbuf in
                            crlf lexbuf;
                            ACTION (xpl, v) )
# 11066 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 49 ->
# 288 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let v = dtp lexbuf in
                            crlf lexbuf;
                            TRIGGER (params, v) )
# 11075 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 50 ->
# 293 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let v = integer_val lexbuf in
                            crlf lexbuf;
                            REPEAT (xpl, v) )
# 11084 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 51 ->
# 298 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xpl = xparam_list lexbuf in
                            col lexbuf;
                            let global = is_slash lexbuf in
                            let v = text lexbuf in
                            crlf lexbuf;
                            TZID (xpl, global, v) )
# 11094 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 52 ->
# 304 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let url = uri lexbuf in
                            crlf lexbuf;
                            TZURL (xplist, url) )
# 11103 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 53 ->
# 309 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let off = utcoffset lexbuf in
                            crlf lexbuf;
                            TZOFFSETTO (xplist, off) )
# 11112 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 54 ->
# 314 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let xplist = xparam_list lexbuf in
                            col lexbuf;
                            let off = utcoffset lexbuf in
                            crlf lexbuf;
                            TZOFFSETFROM (xplist, off) )
# 11121 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 55 ->
# 319 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let params = all_param_list lexbuf in
                            col lexbuf;
                            let t = text lexbuf in
                            crlf lexbuf;
                            TZNAME (params, t) )
# 11130 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 56 ->
# 324 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( EOF )
# 11135 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 57 ->
# 325 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          (  let s = aline lexbuf in failwith (s ^ " is not implemented yet") )
# 11140 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and aline lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state382 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 328 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                          ( Lexing.lexeme lexbuf )
# 11155 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and col lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state383 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 331 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( () )
# 11170 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 332 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( print_endline (Lexing.lexeme lexbuf);
                            failwith "expected a :" )
# 11176 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and crlf lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state384 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 336 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( () )
# 11191 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 337 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( print_endline (Lexing.lexeme lexbuf); failwith "expected eol" )
# 11196 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and quote lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state386 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 340 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( () )
# 11211 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 341 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( failwith "expected a \"" )
# 11216 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and is_semi lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state387 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 344 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( true )
# 11231 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 345 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( false )
# 11236 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and semi lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state388 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 348 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( () )
# 11251 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 349 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( failwith "expected a ';'" )
# 11256 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and version2 lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state389 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 352 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( () )
# 11271 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 353 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( failwith "version number different from 2.0" )
# 11276 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and uri lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state392 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 356 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( Lexing.lexeme lexbuf )
# 11291 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and text_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state393 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 359 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let t = text lexbuf in t :: (text_list lexbuf) )
# 11306 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 360 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( [] )
# 11311 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and text lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 363 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( reset_string_buffer ();
                            let string_start = Lexing.lexeme_start lexbuf in
                            string_start_pos := string_start;
                            intext lexbuf;
                            (*
                            lexbuf.Lexing.lex_start_pos <-
                                string_start - lexbuf.Lexing.lex_abs_pos;
                                *)
                            (*
                            let s = get_stored_string () in
                            print_endline s;
                            s
                            *)
                            get_stored_string ()
                          )
# 11340 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and binary lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 380 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( reset_string_buffer ();
                            let string_start = Lexing.lexeme_start lexbuf in
                            string_start_pos := string_start;
                            inbinary lexbuf;
                            get_stored_string ()
                          )
# 11360 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and xparam_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state394 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 388 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = (s', param_value_equal lexbuf) in r :: (xparam_list lexbuf) )
# 11377 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 391 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( [] )
# 11382 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and param_value_equal lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state399 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 394 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let pv = param_value lexbuf in
                            pv :: (param_value_list lexbuf) )
# 11398 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and param_value lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state400 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 398 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                    ( Lexing.lexeme lexbuf )
# 11413 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and param_value_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state403 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 401 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let pv = param_value lexbuf in
                            pv :: (param_value_list lexbuf) )
# 11429 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 403 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( [] )
# 11434 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and lang_rfc1766 lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state404 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 407 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( Lexing.lexeme lexbuf )
# 11449 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and paramtext lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state405 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 410 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( Lexing.lexeme lexbuf )
# 11464 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and valuetype lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state406 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 414 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( ICalendar_lextypes.valuetypeparam_from_string (Lexing.lexeme lexbuf) )
# 11479 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and encodingval lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state434 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 417 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                        ( ICalendar_lextypes.encoding_from_string (Lexing.lexeme lexbuf) )
# 11494 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and iana_or_x lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state446 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 420 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( Lexing.lexeme lexbuf )
# 11509 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and status lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state450 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 424 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                    ( ICalendar_lextypes.status_from_string (Lexing.lexeme lexbuf) )
# 11524 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and transvalue lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state509 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 427 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                ( ICalendar_lextypes.transvalue_from_string (Lexing.lexeme lexbuf) )
# 11539 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and cutype lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state525 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 431 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
  ( ICalendar_lextypes.cutype_from_string (Lexing.lexeme lexbuf) )
# 11554 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and quoted_uri_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state557 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 434 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( let r = uri lexbuf in quote lexbuf; r :: (quoted_uri_list lexbuf) )
# 11569 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 435 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( [] )
# 11574 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and role lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state559 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 439 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                        ( ICalendar_lextypes.roleparam_from_string (Lexing.lexeme lexbuf) )
# 11589 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and partstatparam lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state609 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 442 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                                                                                                                      ( ICalendar_lextypes.partstatparam_from_string (Lexing.lexeme lexbuf) )
# 11604 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and reltypeparam lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state669 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 446 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                      ( ICalendar_lextypes.reltypeparam_from_string (Lexing.lexeme lexbuf) )
# 11619 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and rangeparam lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state688 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 449 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                    ( ICalendar_lextypes.rangeparam_from_string (Lexing.lexeme lexbuf) )
# 11634 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and trigrelparam lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state705 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 452 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                  ( ICalendar_lextypes.trigrelparam_from_string (Lexing.lexeme lexbuf) )
# 11649 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and all_param_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state712 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 455 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let res = uri lexbuf in
                           quote lexbuf;
                           let r = Altrepparam res in r :: (all_param_list lexbuf) )
# 11666 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 458 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = CNparam (param_value lexbuf) in r :: (all_param_list lexbuf) )
# 11671 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 459 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Cutypeparam (cutype lexbuf) in r :: (all_param_list lexbuf) )
# 11676 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 460 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let u = uri lexbuf in
                           quote lexbuf;
                           let r = Delfromparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) )
# 11684 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 4 ->
# 464 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let u = uri lexbuf in
                           quote lexbuf;
                           let r = Deltoparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) )
# 11692 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 5 ->
# 468 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let uri = uri lexbuf in
                           quote lexbuf;
                           let r = Dirparam uri in r :: (all_param_list lexbuf) )
# 11699 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 6 ->
# 471 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Encparam (encodingval lexbuf) in r :: (all_param_list lexbuf) )
# 11704 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 7 ->
# 472 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Fmtparam (iana_or_x lexbuf) in r :: (all_param_list lexbuf) )
# 11709 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 8 ->
# 473 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Langparam (lang_rfc1766 lexbuf) in r :: (all_param_list lexbuf) )
# 11714 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 9 ->
# 474 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let u = uri lexbuf in
                           quote lexbuf;
                           let r = Memberparam (u :: (quoted_uri_list lexbuf)) in
                           r :: (all_param_list lexbuf) )
# 11722 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 10 ->
# 478 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Partstatparam (partstatparam lexbuf) in r :: (all_param_list lexbuf) )
# 11727 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 11 ->
# 479 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Rangeparam (rangeparam lexbuf) in r :: (all_param_list lexbuf) )
# 11732 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 12 ->
# 480 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( Trigrelparam ICalendar_syntax.RelStart :: (all_param_list lexbuf) )
# 11737 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 13 ->
# 481 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( Trigrelparam ICalendar_syntax.RelEnd :: (all_param_list lexbuf) )
# 11742 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 14 ->
# 482 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Reltypeparam (reltypeparam lexbuf) in r :: (all_param_list lexbuf) )
# 11747 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 15 ->
# 483 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( (Rsvpparam true) :: (all_param_list lexbuf) )
# 11752 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 16 ->
# 484 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( (Rsvpparam false) :: (all_param_list lexbuf) )
# 11757 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 17 ->
# 485 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let r = Roleparam (role lexbuf) in r :: (all_param_list lexbuf) )
# 11762 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 18 ->
# 486 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let uri = uri lexbuf in
                           quote lexbuf;
                           let r = Sentbyparam uri in r :: (all_param_list lexbuf) )
# 11769 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 19 ->
# 489 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let pt = paramtext lexbuf in 
                           let r = Tzidparam(true, pt) in r :: (all_param_list lexbuf) )
# 11775 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 20 ->
# 491 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let pt = paramtext lexbuf in 
                           let r = Tzidparam(false, pt) in r :: (all_param_list lexbuf) )
# 11781 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 21 ->
# 493 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let vt = valuetype lexbuf in
                           (Valuetypeparam vt) :: (all_param_list lexbuf) )
# 11787 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 22 ->
# 495 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = Xparam (s', param_value_equal lexbuf) in r :: (all_param_list lexbuf) )
# 11794 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 23 ->
# 498 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 11799 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and digits lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state845 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 501 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( int_of_string (Lexing.lexeme lexbuf) )
# 11814 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onetwod lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state847 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 504 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( int_of_string (Lexing.lexeme lexbuf) )
# 11829 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onetwosignedopt lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state849 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 507 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( Some (int_of_string (Lexing.lexeme lexbuf))  )
# 11844 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 508 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              Some (int_of_string s')  )
# 11851 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 511 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( Some (int_of_string (Lexing.lexeme lexbuf))  )
# 11856 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 512 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( None )
# 11861 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onetwosigned lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state855 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 515 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            (int_of_string (Lexing.lexeme lexbuf)  )
# 11876 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 516 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  )
# 11883 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 519 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( int_of_string (Lexing.lexeme lexbuf)  )
# 11888 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onetwodlist lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state861 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 522 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwod lexbuf in v :: (onetwodlist lexbuf) )
# 11903 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 523 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 11908 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onetwosignedlist lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state862 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 526 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwosigned lexbuf in v :: (onetwosignedlist lexbuf) )
# 11923 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 527 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 11928 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onethreesigned lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state863 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 530 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                                (int_of_string (Lexing.lexeme lexbuf)  )
# 11943 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 531 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                                ( let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  )
# 11950 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 534 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                                ( int_of_string (Lexing.lexeme lexbuf)  )
# 11955 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and onethreesignedlist lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state872 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 537 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onethreesigned lexbuf in v :: (onethreesignedlist lexbuf) )
# 11970 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 538 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 11975 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and weekday lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state873 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 541 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                                    ( ICalendar_lextypes.weekday_from_string
                                                        (Lexing.lexeme lexbuf) )
# 11991 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and weekdaynum lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 545 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let n = onetwosignedopt lexbuf in
                           n, weekday lexbuf )
# 12007 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and wdaylist lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state879 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 549 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let w = weekdaynum lexbuf in
                           w :: (wdaylist lexbuf) )
# 12023 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 551 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 12028 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and recurstuff lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state880 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 554 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let d = dtp lexbuf in (RecDate d) :: (recurstuff lexbuf) )
# 12043 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 555 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let c = digits lexbuf in (RecCount c) :: (recurstuff lexbuf) )
# 12048 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 556 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let c = digits lexbuf in (RecInterval c) :: (recurstuff lexbuf) )
# 12053 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 557 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwod lexbuf in 
                           let l = onetwodlist lexbuf in (RecBySecond (v :: l)) :: (recurstuff lexbuf) )
# 12059 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 4 ->
# 559 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByMinute (v :: l)) :: (recurstuff lexbuf) )
# 12065 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 5 ->
# 561 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByHour (v :: l)) :: (recurstuff lexbuf) )
# 12071 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 6 ->
# 563 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = weekdaynum lexbuf in
                           let l = wdaylist lexbuf in (RecByDay (v :: l)) :: (recurstuff lexbuf) )
# 12077 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 7 ->
# 565 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwosigned lexbuf in
                           let l = onetwosignedlist lexbuf in 
                           (RecByMonthDay (v :: l)) :: (recurstuff lexbuf) )
# 12084 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 8 ->
# 568 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                          ( let v = onethreesigned lexbuf in
                           let l = onethreesignedlist lexbuf in 
                           (RecByYearDay (v :: l)) :: (recurstuff lexbuf) )
# 12091 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 9 ->
# 571 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwosigned lexbuf in
                           let l = onetwosignedlist lexbuf in 
                           (RecByWeekNo (v :: l)) :: (recurstuff lexbuf) )
# 12098 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 10 ->
# 574 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onetwod lexbuf in
                           let l = onetwodlist lexbuf in (RecByMonth (v :: l)) :: (recurstuff lexbuf) )
# 12104 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 11 ->
# 576 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let v = onethreesigned lexbuf in
                           let l = onethreesignedlist lexbuf in 
                           (RecBySetPos (v :: l)) :: (recurstuff lexbuf) )
# 12111 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 12 ->
# 579 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let w = weekday lexbuf in (RecWkStart w) :: (recurstuff lexbuf) )
# 12116 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 13 ->
# 580 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let s = Lexing.lexeme lexbuf in
                            let s' = String.sub s 1 ((String.length s) - 1) in
                            let r = RecX (s', text lexbuf) in r :: (recurstuff lexbuf) )
# 12123 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 14 ->
# 583 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( [] )
# 12128 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and freq lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state952 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 587 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( ICalendar_lextypes.freq_from_string (Lexing.lexeme lexbuf) )
# 12143 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and recur lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state991 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 590 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                         ( let freq = freq lexbuf in
                           freq, (recurstuff lexbuf) )
# 12159 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and intext lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state996 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 595 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
    ( store_string_char('\\');
      store_string_char(Lexing.lexeme_char lexbuf 1);
      intext lexbuf )
# 12176 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 600 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
    ( store_string_char(Lexing.lexeme_char lexbuf 1);
      intext lexbuf )
# 12182 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 603 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
    ( store_string_char(Lexing.lexeme_char lexbuf 0);
      intext lexbuf )
# 12188 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 605 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
     ( () )
# 12193 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and inbinary lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state998 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 608 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                               ( store_string (Lexing.lexeme lexbuf);
                                 inbinary lexbuf )
# 12209 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 610 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                               ( inendbinary lexbuf )
# 12214 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and inendbinary lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1002 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 613 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                               ( store_string (Lexing.lexeme lexbuf) )
# 12229 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 614 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                               ( store_string (Lexing.lexeme lexbuf) )
# 12234 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 615 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                               ( )
# 12239 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and iana_token lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1007 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 618 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( Lexing.lexeme lexbuf )
# 12254 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and calvalue lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1009 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 621 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( ICalendar_lextypes.calvalue_from_string (Lexing.lexeme lexbuf) )
# 12269 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and classvalue lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1019 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 625 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( ICalendar_lextypes.classvalue_from_string (Lexing.lexeme lexbuf) )
# 12284 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and date_time lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 628 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let d = date lexbuf in
                              single_T lexbuf;
                              let t = time lexbuf in
                              { ICalendar_syntax.date = d;
                                ICalendar_syntax.time = t}
                              )
# 12304 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and two_digits lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1044 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 636 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( Lexing.lexeme lexbuf )
# 12319 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and opt_two_digits lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1046 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 639 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( Some (Lexing.lexeme lexbuf) )
# 12334 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 640 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( None )
# 12339 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and get_sign lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1048 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 643 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( true )
# 12354 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 644 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( false )
# 12359 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 645 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( failwith "Expected a '+' or a '-'" )
# 12364 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and utcoffset lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 648 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let pos = get_sign lexbuf in
                              let h = int_of_string (two_digits lexbuf) in
                              let m = int_of_string (two_digits lexbuf) in
                              let s =
                                match (opt_two_digits lexbuf) with
                                | Some t -> int_of_string t
                                | None -> 0
                              in
                              { ICalendar_syntax.positive = pos;
                                ICalendar_syntax.off_h = h;
                                ICalendar_syntax.off_m = m;
                                ICalendar_syntax.off_s = s; }
                            )
# 12391 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and date lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 663 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( match opt_date lexbuf with
                              | Some d -> d
                              | None -> failwith "was expecting a date = 8 digits" )
# 12408 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and opt_date lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1049 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 668 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let year = int_of_string (Lexing.lexeme lexbuf) in
                              let month = int_of_string (two_digits lexbuf) in
                              let day = int_of_string (two_digits lexbuf) in
                              Some (ICalendar_lextypes.mk_date (year, month, day)) )
# 12426 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 672 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( None )
# 12431 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and is_slash lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1053 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 675 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( true )
# 12446 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 676 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( false )
# 12451 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and single_T lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1054 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 680 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( () )
# 12466 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 681 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( failwith "was expecting a T" )
# 12471 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and is_T lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1055 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 684 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( true )
# 12486 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 685 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( false )
# 12491 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and time lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 688 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let hour = int_of_string (two_digits lexbuf) in
                               let minute = int_of_string (two_digits lexbuf) in
                               let second = int_of_string (two_digits lexbuf) in
                               let t = (hour, minute, second) in
                               let res = 
                                 if (single_Z lexbuf) then (t, true)
                                 else (t, false) 
                               in
                               ICalendar_lextypes.mk_time res
                             )
# 12515 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and single_Z lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1056 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 700 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                       ( true )
# 12530 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 701 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                                       ( false )
# 12535 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and dtp lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 704 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( match opt_date lexbuf with
                               | Some d ->
                                   if (is_T lexbuf) then
                                     let dts = {ICalendar_syntax.date = d;
                                                ICalendar_syntax.time = (time lexbuf)}
                                     in
                                     if (is_slash lexbuf) then
                                       ICalendar_syntax.PeriodVal (
                                         match (opt_durval lexbuf) with
                                         | Some d -> ICalendar_syntax.PeriodStart (dts, d)
                                         | None -> ICalendar_syntax.PeriodExplicit (dts, date_time lexbuf)
                                       )
                                     else
                                       ICalendar_syntax.DateTimeVal dts
                                   else
                                     ICalendar_syntax.DateVal d
                               | None -> ICalendar_syntax.DurationVal (durval lexbuf) )
# 12566 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and dtp_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1057 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 723 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let v = dtp lexbuf in v :: (dtp_list lexbuf) )
# 12581 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 724 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( [] )
# 12586 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and float_val lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1058 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 727 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( float_of_string (Lexing.lexeme lexbuf) )
# 12601 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and integer_val lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1063 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 730 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( int_of_string (Lexing.lexeme lexbuf)  )
# 12616 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 731 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( let s = Lexing.lexeme lexbuf in
                              let s' = String.sub s 1 ((String.length s) - 1) in
                              int_of_string s'  )
# 12623 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 734 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                            ( int_of_string (Lexing.lexeme lexbuf)  )
# 12628 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and geoval lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 737 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let f1 = float_val lexbuf in
                               semi lexbuf;
                               (f1, float_val lexbuf) )
# 12645 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and duropt lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1069 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 742 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let s = Lexing.lexeme lexbuf in
                               Week (int_of_string (String.sub s 0 ((String.length s) - 1))) )
# 12661 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 744 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let s = Lexing.lexeme lexbuf in
                               if is_T lexbuf then
                                 DayT (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) 
                               else
                                 Day (int_of_string (String.sub s 0 ((String.length s) - 1))) 
                               )
# 12671 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 750 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let s = Lexing.lexeme lexbuf in
                               Hour (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) )
# 12677 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 752 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let s = Lexing.lexeme lexbuf in
                               Minute (int_of_string (String.sub s 0 ((String.length s) - 1)), duropt lexbuf) )
# 12683 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 4 ->
# 754 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let s = Lexing.lexeme lexbuf in
                               Second (int_of_string (String.sub s 0 ((String.length s) - 1))) )
# 12689 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 5 ->
# 756 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( DurEnd )
# 12694 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and opt_durval lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1071 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 759 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( Some (neg_dur (convert_duration_time (duropt lexbuf))) )
# 12709 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 760 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( Some (neg_dur (convert_duration_week_date (duropt lexbuf))) )
# 12714 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 2 ->
# 761 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( Some (convert_duration_time (duropt lexbuf)) )
# 12719 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 3 ->
# 762 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( Some (convert_duration_week_date (duropt lexbuf)) )
# 12724 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 4 ->
# 763 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( None )
# 12729 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and durval lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state381 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 766 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( match (opt_durval lexbuf) with
                               | Some d -> d
                               | None -> failwith "expected a duration" )
# 12746 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and dot_integer_list lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1077 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 771 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( let i = integer_val lexbuf in
                               i :: (dot_integer_list lexbuf) )
# 12762 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | 1 ->
# 773 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( [] )
# 12767 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


and actionval lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 0; 
  let __ocaml_lex_result = __ocaml_lex_state1078 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
# 777 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.mll"
                             ( ICalendar_lextypes.actionval_from_string (Lexing.lexeme lexbuf) )
# 12782 "/Users/bcpierce/current/harmony/src/iCalendar/iCalendarlex.ml"

  | _ -> raise (Failure "lexing: empty token")


;;

