(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* syntax.ml - Focal abstract syntax                            *)
(****************************************************************)
(* $Id$ *)

let ( @ ) = Safelist.append  (* redefine @ to use stack-safe append *)
let sprintf = Printf.sprintf (* import selectively *)

(* identifiers *)
type i = Info.t
type id = i * string
type qid = id list * id

let get_qualifiers (is,_) = is

let mk_id i s = (i,s)
let mk_qid is i = (is,i)

(* functions on identifiers *)
(* equality: ignore parsing Info.t *)
let id_compare (_,x1) (_,x2) = compare x1 x2
let id_equal i1 i2 = (id_compare i1 i2 = 0)
let qid_compare (qs1,x1) (qs2,x2) = 
  let rec ids_compare xs1 xs2 = match xs1,xs2 with
      [],[] -> 0
    | _,[]  -> 1
    | [],_  -> -1
    | (x1::t1),(x2::t2) -> 
	let hd_compare = id_compare x1 x2 in
	  if (hd_compare <> 0) then hd_compare 
	  else ids_compare t1 t2
  in
    ids_compare (qs1@[x1]) (qs2@[x2])
let qid_equal q1 q2 = (qid_compare q1 q2 = 0)
	
let qid_prefix q1 q2 = 
  let (is1,i1) = q1 in
  let (is2,i2) = q2 in 
  let il1 = is1 @ [i1] in
  let il2 = is2 @ [i2] in
    ((Safelist.length il1) <= (Safelist.length il2)) 
    && (Safelist.for_all 
	  (fun (i1,i2) -> id_equal i1 i2)
	  (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))
	
let string_of_id (_,i) = i
let string_of_qid (qs,i) = 
  Printf.sprintf "%s%s"
    (Safelist.fold_left 
       (fun acc qi -> Printf.sprintf "%s%s." acc (string_of_id qi)) 
       ""
       qs)    
    (string_of_id i)

let qid_hash q = Hashtbl.hash (string_of_qid q)

(* data structures over Qids *)
module QidMapplus = Mapplus.Make(
  struct
    type t = qid
    let compare = qid_compare
    let to_string = string_of_qid 
  end)

module QidMap = QidMapplus.Map
module QidSet = QidMapplus.KeySet

(* utility functions *)
let qid_of_id id = [],id
let id_of_string i s = (i,s)
let dot (qs1,x1) (qs2,x2) = (qs1@(x1::qs2),x2)

(* constants *)
let native_prelude i = Safelist.map (mk_id i) ["Native"; "Prelude"]
let mk_pre_qid i s = mk_qid (native_prelude i) (mk_id i s)
let compose2_qid i = mk_pre_qid i "compose2"
let get_qid i = mk_pre_qid i "get"
let create_qid i = mk_pre_qid i "create"
let put_qid i = mk_pre_qid i "put"
let sync_qid i = mk_pre_qid i "_sync" (* can't write "sync" because it's a token :( *)
let cons_qid i = mk_pre_qid i "Cons"
let nil_qid i = mk_pre_qid i "Nil"
let any_qid i = mk_pre_qid i "Any"
let type_of_tree_qid i = mk_pre_qid i "type_of_tree"

(* abstract syntax *)

(* sorts *)
type sort = 
    SName 
  | SLens  
  | SSchema   
  | STree    
  | SArrow of sort * sort
      
(* parameters *)
type param = PDef of i * id * sort

(* expressions *)
type exp = 
    EApp of i  * exp * exp
  | EAssert of i * exp 
  | ECheckLens of i * exp * exp * exp
  | EAtom of i * exp * exp 
  | ECat of i * exp list 
  | ECons of i * exp * exp 
  | ESpineCons of i * exp * exp 
  | EFun of i * param list * sort option * exp 
  | ELet of i * binding list * exp
  | EName of i * id
  | ENil of i
  | EProtect of i * exp 
  | ESchema of i * schema_binding list * exp 
  | EUnion of i * exp list
  | EVar of i * qid * bool
  | EWild of i * exp list * int * bool * exp
  | EInter of i * exp list
  | EMinus of i * exp * exp

(* bindings *)
and binding = BDef of i * id * param list * sort * exp

and schema_binding = SDef of i * id * exp 

(* declarations *)
type test_result =
    Result of exp
  | ErrorResult
  | PrintResult

type decl = 
    DLet of i * binding list 
  | DMod of i * id * decl list 
  | DSchema of i * schema_binding list
  | DTest of i * exp * test_result
       
(* modules *)
type modl = MDef of i * id * qid list * decl list
  
(* accessor functions *)
let name_of_id (_,x) = x
let id_of_binding (BDef(_,x,_,_,_)) = x

(* read off info fields *)
let info_of_list (e2i:'a -> Info.t) (i:Info.t) (l: 'a list) : Info.t = Safelist.fold_left (fun i ei -> Info.merge_inc (e2i ei) i) i l
let info_of_id = function (i,_) -> i
let info_of_qid = function (_,id) -> info_of_id id
let info_of_exp = function
    EApp(i,_,_)    -> i
  | EAssert(i,_)   -> i
  | ECheckLens(i,_,_,_) -> i
  | EAtom(i,_,_)   -> i
  | ECat(i,_)      -> i
  | ECons(i,_,_)   -> i
  | ESpineCons(i,_,_)   -> i
  | EFun(i,_,_,_)  -> i
  | ELet(i,_,_)    -> i
  | EName(i,_)     -> i
  | ENil(i)        -> i
  | EProtect(i,_)  -> i
  | ESchema(i,_,_) -> i 
  | EUnion(i,_t)   -> i
  | EVar(i,_,_)    -> i
  | EWild(i,_,_,_,_) -> i
  | EMinus(i,_,_)    -> i
  | EInter(i,_)      -> i

let info_of_binding (BDef(i,_,_,_,_)) = i
let info_of_bindings bs = 
  match bs with 
      [] -> Info.M "NONE"
    | h::t -> info_of_list info_of_binding (info_of_binding h) t

let info_of_schema_binding (SDef(i,_,_)) = i
let info_of_schema_bindings ss = 
  match ss with 
      [] -> Info.M "NONE"
    | h::t -> info_of_list info_of_schema_binding (info_of_schema_binding h) t

(* read off pieces of parameters *)
let id_of_param = function PDef(_,x,_) -> x
let sort_of_param = function PDef(_,_,s) -> s

(* ---------------- pretty printing --------------- *)
(* sorts *)
let format_sort s0 = 
  let rec format_sort_aux parens = function
      SName         -> Format.printf "name"
    | SLens         -> Format.printf "lens"
    | SSchema       -> Format.printf "schema"
    | STree         -> Format.printf "tree"
    | SArrow(s1,s2) -> 
        if parens then Format.printf "(";
        format_sort_aux true s1; 
        Format.printf "@ ->@ "; 
        format_sort_aux false s2; 
        if parens then Format.printf ")"
  in 
    Format.printf "@[<2>";
    format_sort_aux false s0;
    Format.printf "@]"
          
(* params *)
let format_param (PDef(_,i,s)) = 
  Format.printf "@[%s:" (string_of_id i); 
  format_sort s; 
  Format.printf "@]"

type format_mode = { app : bool; cat : bool }

(* expressions *)
let rec format_exp_aux mode e0 = match e0 with 
    EApp(_,e1,e2)   -> 
      let mode = { mode with cat = false } in   
        Format.printf "@[<2>"; 
        if mode.app then Format.printf "(";
        (* some special formatting for infix operators, etc. *)
        begin match e1 with 
            EApp(_,EVar(_,q,_),e12)
              when string_of_qid q = "Native.Prelude.compose2" ->
                format_exp_aux { mode with app = false } e12;
                Format.printf ";@ ";
                format_exp_aux { mode with app = false } e2
          | EVar(_,q,_) when string_of_qid q = "Native.Prelude.get" ->
              format_exp_aux { mode with app = true} e2;
              Format.printf "@ /"                
          | EVar(_,q,_) when string_of_qid q = "Native.Prelude.put" -> 
              format_exp_aux { mode with app = true } e2;
              Format.printf "@ \\"
          | EApp(_,EVar(_,q,_),e12) 
              when string_of_qid q = "Native.Prelude.create" -> 
              format_exp_aux { mode with app = true } e12;
                Format.printf "@ \\@ ";
                format_exp_aux { mode with app = true } e2;
                Format.printf "@ missing"                
          | _ -> 
              format_exp_aux { mode with app = false } e1; 
              Format.printf "@ "; 
              format_exp_aux { mode with app = true } e2
        end;
        if mode.app then Format.printf ")";
        Format.printf "@]"
  | EAssert(_,e) ->       
      let mode = { mode with cat = false } in
        Format.printf "@[<2>assert@ "; format_exp_aux mode e; Format.printf "@]"
      
  | ECheckLens(_,e1,e2,e3) ->       
      let mode = { mode with cat = false } in
        Format.printf "@[<2>check@ ("; 
        format_exp_aux mode e1; 
        Format.printf ") : ";
        format_exp_aux mode e2;
        Format.printf "@ <->@ ";
        format_exp_aux mode e3;
        Format.printf "@]"      
                                                  
  | EAtom(_,n,e)    -> 
      let imode = { mode with cat = false } in
        Format.printf "@[<2>"; 
        if not mode.cat then Format.printf "{";
        format_exp_aux imode n; 
        Format.printf "=@,"; 
        format_exp_aux imode e; 
        if not mode.cat then Format.printf "}";
        Format.printf "@]"
  | ECat(_,es)      -> 
      Format.printf "{@[<1>"; 
      Misc.format_list 
        ",@ "
        (format_exp_aux { mode with cat = true })
        es; 
      Format.printf "@]}"
  | ECons(_,e1,e2)  -> 
      let mode = { mode with cat = false } in 
        (* if we have a spine of cons cells, ending in [], print
           it using list notation. Otherwise, use :: *)
      let rec get_list_elts acc = function
          ENil(_) -> Some (Safelist.rev acc)
        | ECons(_,e,e') -> get_list_elts (e::acc) e'
        | _            -> None in
        Format.printf "@["; 
        begin
          match get_list_elts [] e0 with 
              None -> 
                Format.printf "(";
                format_exp_aux mode e1; 
                Format.printf "::@,"; 
                format_exp_aux mode e2;
                Format.printf ")";
            | Some el -> 
                Format.printf "["; 
                Misc.format_list ",@ " (format_exp_aux mode) el;
                Format.printf "]"
        end;
        Format.printf "@]"
  | ESpineCons(_,e1,e2)  -> 
      let mode = { mode with cat = false } in 
        (* if we have a spine of spined cons cells, ending in [], print
           it using list notation. Otherwise, use :|: *)
      let rec get_list_elts acc = function
          ENil(_) -> Some (Safelist.rev acc)
        | ECons(_,e,e') -> get_list_elts (e::acc) e'
        | _            -> None in
        Format.printf "@["; 
        begin
          match get_list_elts [] e0 with 
              None -> 
                Format.printf "(";
                format_exp_aux mode e1; 
                Format.printf ":|:@,"; 
                format_exp_aux mode e2;
                Format.printf ")";
            | Some el -> 
                Format.printf "[|"; 
                Misc.format_list ",@ " (format_exp_aux mode) el;
                Format.printf "|]"
        end;
        Format.printf "@]"
  | EFun(_,ps,so,e) -> 
      let mode = { mode with cat = false } in   
      Format.printf "@[<2>fun@ ";  
      Misc.format_list 
        "@ "
        format_param 
        ps;
      (match so with 
           None -> ()
         | Some s -> Format.printf " : "; format_sort s);
      Format.printf "@ ->@ "; 
      format_exp_aux mode e;
      Format.printf "@]"
  | ELet(_,bs,e) ->
      let mode = { mode with cat = false } in 
      Format.printf "@[<2>";
      format_bindings bs;
      Format.printf "@ in@ ";
      format_exp_aux mode e;
      Format.printf "@]"
  | EName(_,n) -> 
      Format.printf "@[%s@]" (Misc.whack (string_of_id n))
  | ENil(_) -> 
      Format.printf "[]"
  | EProtect(_,e) -> 
      let mode = { mode with cat = false } in 
        Format.printf "@[protect@ "; format_exp_aux mode e; Format.printf "@]"
  | ESchema(_,ss,e) -> 
      let mode = { mode with cat = false } in 
        Format.printf "@[<2>";
        format_schema_bindings ss;
        Format.printf "@ in@ ";
        format_exp_aux mode e;
        Format.printf "@]"
  | EUnion(_,es) -> 
      let mode = { mode with cat = false } in 
        Format.printf "@[<2>(";
        Misc.format_list "@ |@ " (format_exp_aux mode) es;
        Format.printf ")@]"
  | EInter(_,es) -> 
      let mode = { mode with cat = false } in 
        Format.printf "@[<2>(";
        Misc.format_list "@ &@ " (format_exp_aux mode) es;
        Format.printf ")@]"
  | EMinus(_,e1,e2) -> 
      let mode = { mode with cat = false } in 
        Format.printf "@[<2>(";
        format_exp_aux mode e1;
        Format.printf "@ -@ ";
        format_exp_aux mode e2;
        Format.printf ")@]"
  | EVar(_,x,_) -> 
      Format.printf "@[%s@]" (string_of_qid x)
  | EWild(_,f,l,u,e)  ->
      let rec format_n_bangs n = match n with 
          0 -> ()
        | n -> Format.printf "!"; format_n_bangs (n-1) in                
        Format.printf "@[";
        if not mode.cat then Format.printf "{";
        let imode = { mode with cat = false } in 
          (match l,u with 
               0,true -> Format.printf "*"
             | n,true -> format_n_bangs n; Format.printf "*"
             | n,false -> format_n_bangs n);
          if f <> [] then 
          (Format.printf "\\(@[";
           Misc.format_list ",@ " (format_exp_aux imode) f;
           Format.printf "@])");
        Format.printf "=@,";
        format_exp_aux imode e;
        if not mode.cat then Format.printf "}";
        Format.printf "@]" 
          
and format_exp e = format_exp_aux { app = false; cat = false } e

and format_binding (BDef(_,x,ps,s,e)) = 
  Format.printf "@[<2>%s" (string_of_id x);
  if ps <> [] then Format.printf "@ "; 
  Misc.format_list "@ " (fun pi -> Format.printf "("; format_param pi; Format.printf ")") ps;
  Format.printf "@ :@ ";
  format_sort s;
  Format.printf "@ =@ ";
  format_exp e;
  Format.printf "@]"

and format_bindings bs = 
  Format.printf "@[let "; 
  Misc.format_list "@\nand " format_binding bs;
  Format.printf "@]"

and format_schema_binding(SDef(_,x,e)) = 
  Format.printf "@[<2>%s =@ " (string_of_id x);
  format_exp e;
  Format.printf "@]"

and format_schema_bindings ss = 
  Format.printf "@[schema "; 
  Misc.format_list "@\nand " format_schema_binding ss;
  Format.printf "@]"

and format_decl = function
  | DLet(i,bs) -> format_bindings bs
  | DMod(_,i,ds) -> 
      Format.printf "@[module %s =@\n  @[" (string_of_id i);
      Misc.format_list "@\n" format_decl ds;
      Format.printf "@]end@]"
  | DSchema(i,ss) -> format_schema_bindings ss
  | DTest(_,e,tr) -> 
      Format.printf "@[<2>test@ ";
      Format.printf "@[";
      format_exp e;
      Format.printf "@ =@ ";
      (match tr with 
           ErrorResult -> Format.printf "error"
         | PrintResult -> Format.printf "?"
         | Result e2   -> format_exp e2);
      Format.printf "@]@]"

let id_of_modl (MDef(_,m,_,_)) = m
let info_of_module (MDef(i,_,_,_)) = i

let format_module (MDef(_,i,qs,ds)) = 
  Format.printf "@[module %s =@\n  @[" (string_of_id i);
  if qs <> [] then 
    Misc.format_list 
      "@\n" 
      (fun qi -> Format.printf "open %s" (string_of_qid qi)) 
      qs;
  Misc.format_list "@\n" format_decl ds;
  Format.print_newline ();
  Format.printf "@\n@]@]"
