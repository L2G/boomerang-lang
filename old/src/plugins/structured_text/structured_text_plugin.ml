open Library
open Syntax

(*

(**************)
(* PREDICATES *)
(**************)

(* ALLCAPS *)
(*   : true of strings in all caps *)
(* allcaps - native interface *)
let allcaps s =
  (String.uppercase s = s) && (String.lowercase s <> s)

(* allcaps - library interface *)
let allcaps_lib = P allcaps
		
let _ = register ("allcaps",T (Predicate), allcaps_lib)

(**********)
(* LENSES *)
(**********)

(* STRUCTURED_TEXT *)
(* structures_text - library interface *)
let structured_text_lib = 
  Compiler.compile_focal "
    let filter p d = fork p id (const {} d)
    let focus n d = filter {n} d;hoist n
    let hd d = focus *h {*t=d}
    let tl d = focus *t {*h=d}
    let prune n d = fork {n} (const {} {n=d}) id
    let rename m n= xfork {m} {n} (hoist m;plunge n) id
    let grename m n = acond (hasChild m) (hasChild n) (rename m n) id
    let hoist_hd p = hoist_nonunique *h p;hoist_nonunique *t (neg p)

    let map_head l = fork {*h} (map l) id
    let map_tail l = fork {*t} (map l) id
    let mapp p l = fork p (map l) id
    let promote k1 k2 = fork {k1} (map (fork {k2} id (plunge *temp*))) id;
      hoist_nonunique k1 {*temp* k2};
      rename *temp* k1

    let act p = 
     acond isEmptyList isEmptyList
      id
       (acond (child *t (child *h (child *h p))) (child *h (child *t (minus all isEmptyList)))
         (filter {*h *t} [];
            rename *h *x;
            hoist_nonunique *t {*h *t *};
            xfork {*x *h} {*h} (rename *h *t;rename *x *h;add * {};plunge *h) id )
         (map_head (plunge *h;add * {};add *t [])))

    let structured_text p = 
     let rec aux = wmap < *t -> aux > ; act p in
     aux

    do structured_text"

(* structured_text - native interface *)
let structured_text p = 
  let r = structured_text_lib in
  let r = (funOfArg r) (P p) in
  lensOfArg r

let structured_text_unit_tests = 
    [
    ]

let _ = register_and_test
	  ("structured_text", T(Arrow(Predicate,Lens)), structured_text_lib)
	  structured_text_unit_tests

*)



let allcapsheader v =
  let s = V.get_value v in
  (String.uppercase s = s) && (String.lowercase s <> s)

(* allcaps - library interface *)
let allcaps_lib = Sc allcapsheader
let _ = register ("allcaps",T (Schema), allcaps_lib)

let notallcapsheader v =
  let s = V.get_value v in
  not ((String.uppercase s = s) && (String.lowercase s <> s))

let notallcaps_lib = Sc notallcapsheader
let _ = register ("notallcaps",T (Schema), notallcaps_lib)

let structured_text_arg = Compiler.compile_focal
"let map_head l = fork {*h} (map l) id
 let act p = 
 acond isEmptyList isEmptyList
  id
   (acond (child *t (child *h (child *h p))) (child *h (child *t (minus all isEmptyList)))
     (filter {*h *t} [];
	rename *h *x;
	hoist_nonunique *t {*h *t *};
	xfork {*x *h} {*h} (rename *h *t;rename *x *h;add * {};plunge *h) id )
     (map_head (plunge *h;add * {};add *t [])))
 let structured_text p = 
  let rec aux = wmap < *t -> aux > ; act (not p) in
  aux
 do structured_text"

let structured_text s =
  let r = structured_text_arg in
  let r = (funOfArg r) (Sc s) in
  lensOfArg r

let reader f =
  let lines = Misc.split_nonescape '\n' (Misc.read f) in
  Some(V.from_desc (V.L (Safelist.map (fun l -> V.Val l) lines)))

let writer vo f =
  match vo with
    None ->
      V.error_msg [`String "structured_text_plugin: cannot write missing file"]
  | Some v ->
      Misc.write f
        (String.concat "\n" (Safelist.map V.get_value (V.list_from_structure v)))

let from_string s =
  let lines = Misc.split_nonescape '\n' s in
  Some(V.from_desc (V.L (Safelist.map (fun l -> V.Val l) lines)))

let to_string v =
  (String.concat "\n" (Safelist.map V.get_value (V.list_from_structure v)))
    
let _ =
  Surveyor.register_encoding "asciiwithheaders" 
    {
      Surveyor.description = "Ascii text with caps-only header lines";
      Surveyor.encoding_test =
        (fun filename copt -> Filename.check_suffix filename ".txt");
      Surveyor.reader = reader;
      Surveyor.writer = writer;
      Surveyor.from_string = from_string;
      Surveyor.to_string = to_string;
      Surveyor.base_type = ["lines"; "structured"];
    };
  let bogus = Types.string2abstract_type "" in
  Optometrist.register_lens 
    ["lines"; "structured"] ["structured"]
    Schemas.structured
    (structured_text allcapsheader)

(* ************************************************************************** *)
(* Testing stuff...
let p v =
  let s = V.get_value v in
  s = String.uppercase s

let nhp = tracepoint "nhp" [nhflatten p]

let _ =
try 
let a0 = nhp.get V.empty_list in

let a1 = nhp.get (V.from_desc (V.L [V.Val "foo"; V.Val "bar"; V.Val "FOO"])) in

let a2 = nhp.get (V.from_desc (V.L [V.Val "BAR"; V.Val "foo"; V.Val "bar"; V.Val "FOO"; V.Val "BAR"])) in

let c1 = nhp.put V.empty_list a1 in
let c'1 = nhp.create a1 in

let c2 = nhp.put V.empty_list a2 in
let c'2 = nhp.create a2 in

V.format_msg ([
  `String "tests of nhflatten:";
  `Break;
  `String "nh / 0 :";
  `View a0;
  `String "nh / [foo; bar; FOO] :";
  `View a1;
  `String "putting back the result in 0     :";
  `View c1;
  `String "and creation (should be the same :";
  `View c'1;
  `String "nh / [BAR; foo; bar; FOO; BAR] :";
  `View a2;
  `String "putting back the result in 0     :";
  `View c2;
  `String "and creation (should be the same :";
  `View c'2;
  ] @
  (try
    let a3 = nhp.get (V.from_desc (V.L [V.L [V.Val "blah"]])) in
    [
      `String "***ERROR***: this should have failed: nh / [ [blah] ] :";
      `View a3]
  with
  | V.Error l ->
      ([
        `String "Good, when testing nh / [ [blah] ], we got the error: "]
        @l)
  | V.Illformed (s, tl) ->
      ([
        `String "Good, when testing nh / [ [blah] ], we got the error: ";
        `String s
      ]
      @ (List.map (fun v -> `View v)) tl)
  )
  );
  flush stdout
with
  | V.Error l ->
      Format.printf "@,";
      print_string (Misc.color "-----------\nFATAL ERROR\n-----------"
                    Misc.Red ~bold:true);
      Format.printf "@,";
      V.format_msg l;
      flush stdout;
      flush stderr;
      exit 3
  | e ->
      (flush stdout;
      flush stderr;
      raise e)
*)
