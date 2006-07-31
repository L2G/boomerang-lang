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

(* constants / helpers *)
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

(* --------------- fresh variables generation ----------------- *)
let fresh_map : ((int Name.Map.t) ref) = ref Name.Map.empty
let fresh x = 
  let n = Name.Map.safe_find x (!fresh_map) 0 in 
    fresh_map := Name.Map.add x (n + 1) (!fresh_map);
    let res = sprintf "_%s_%d" x n in res

(* ---------------- abstract syntax ---------------------- *)

(* sorts *)
type sort = 
    SName 
  | SLens
  | SCheckedLens of (qid * lensarrow * qid) 
  | SSchema
  | SPred
  | SFD
  | SView    
  | SMap 
  | SArrow of sort * sort

and lensarrow = Bij | Vwb | Wb

(* parameters *)
and param = PDef of i * id * sort

(* expressions *)
and exp = 
    EApp of i  * exp * exp
  | EAssert of i * exp 
  | ECheckLens of i * exp * lensarrow * exp * exp
  | EAtomCats of i * exp list * exp 
  | EAtomAlts of i * exp list * exp 
  | ECat of i * exp list 
  | ECons of i * exp * exp 
  | EDB of Info.t * Db.t
  | EDBPred of Info.t * Db.Relation.Pred.t
  | EDBFD of Info.t * Dbschema.Relschema.Fd.Set.t
  | EDBSchema of Info.t * Dbschema.t
  | EFun of i * param list * sort option * exp 
  | ELet of i * binding list * exp
  | EMap of i * (exp * exp) list
  | EName of i * id
  | ENil of i
  | EProtect of i * exp * sort option
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

(* constant constructors *)
let mk_compose2_exp i e1 e2 = EApp(i, EApp(i, EVar(i, compose2_qid i,false),e1),e2)
let mk_get_exp i l c = EApp(i, EApp(i, EVar(i, get_qid i,false), l), c)
let mk_put_exp i l a co = match co with 
    None -> EApp(i, EApp(i, EVar(i, create_qid i, false), l), a)
  | Some c -> EApp(i, EApp(i, EApp(i, EVar(i, put_qid i,false), l), a), c)
let mk_sync_exp i lo la lb t v = EApp(i,EApp(i,EApp(i,EApp(i,EApp(i,EVar(i,sync_qid i,false),lo),la),lb),t),v)
let mk_empty_tree i = ECat(i,[])
let mk_any_exp i = EVar(i,any_qid i,false)

(* accessor functions *)
let name_of_id (_,x) = x
let id_of_binding (BDef(_,x,_,_,_)) = x

(* read off info fields *)
let info_of_list (e2i:'a -> Info.t) (i:Info.t) (l: 'a list) : Info.t = Safelist.fold_left (fun i ei -> Info.merge_inc (e2i ei) i) i l
let info_of_id = function (i,_) -> i
let info_of_qid = function (_,id) -> info_of_id id
let info_of_exp = function
    EApp(i,_,_) 
  | EAssert(i,_) 
  | ECheckLens(i,_,_,_,_) 
  | EAtomCats(i,_,_)
  | EAtomAlts(i,_,_)
  | ECat(i,_)  
  | ECons(i,_,_)
  | EDB(i,_)
  | EDBPred(i,_)
  | EDBFD(i,_)
  | EDBSchema(i,_)
  | EFun(i,_,_,_)     
  | ELet(i,_,_)       
  | EMap(i,_)
  | EName(i,_)        
  | ENil(i)           
  | EProtect(i,_,_)   
  | ESchema(i,_,_)    
  | EUnion(i,_)  
  | EVar(i,_,_)   
  | EWild(i,_,_,_,_)
  | EMinus(i,_,_)   
  | EInter(i,_) -> i    

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

(* infix arrow sort constructor *)
let ( ^> ) s1 s2 = SArrow (s1,s2)

(* ---------------- pretty printing --------------- *)
type format_mode = { app : bool; cat : bool }

(* sorts *)
let format_lensarrow la = 
  Util.format "<<%s>" 
    (match la with Bij -> "~" | Vwb -> "=" | Wb -> "-")

let rec format_sort s0 = 
  let rec format_sort_aux parens = function
      SName         -> Util.format "name"
    | SLens         -> Util.format "lens"
    | SCheckedLens(q1,la,q2) -> 
        Util.format "%s" (string_of_qid q1);
        format_lensarrow la;
        Util.format "%s" (string_of_qid q2)
    | SSchema       -> Util.format "schema"
    | SPred         -> Util.format "pred"
    | SFD           -> Util.format "fds"
    | SView         -> Util.format "view"
    | SMap          -> Util.format "fmap"
    | SArrow(s1,s2) -> 
        if parens then Util.format "(";
        format_sort_aux true s1; 
        Util.format "@ ->@ "; 
        format_sort_aux false s2; 
        if parens then Util.format ")"
  in 
    Util.format "@[<2>";
    format_sort_aux false s0;
    Util.format "@]"

(* params *)
and format_param (PDef(_,i,s)) = 
  Util.format "@[%s:" (string_of_id i); 
  format_sort s; 
  Util.format "@]"

(* expressions *)
and format_exp_aux mode e0 = match e0 with 
    EApp(_,e1,e2)   -> 
      let mode = { mode with cat = false } in   
        Util.format "@[<2>"; 
        if mode.app then Util.format "(";
        (* some special formatting for infix operators, etc. *)
        begin match e1 with 
            EApp(_,EVar(_,q,_),e12)
              when string_of_qid q = "Native.Prelude.compose2" ->
                format_exp_aux { mode with app = false } e12;
                Util.format ";@ ";
                format_exp_aux { mode with app = false } e2
          | EVar(_,q,_) when string_of_qid q = "Native.Prelude.get" ->
              format_exp_aux { mode with app = true} e2;
              Util.format "@ /"                
          | EVar(_,q,_) when string_of_qid q = "Native.Prelude.put" -> 
              format_exp_aux { mode with app = true } e2;
              Util.format "@ \\"
          | EApp(_,EVar(_,q,_),e12) 
              when string_of_qid q = "Native.Prelude.create" -> 
              format_exp_aux { mode with app = true } e12;
                Util.format "@ \\@ ";
                format_exp_aux { mode with app = true } e2;
                Util.format "@ missing"                
          | _ -> 
              format_exp_aux { mode with app = false } e1; 
              Util.format "@ "; 
              format_exp_aux { mode with app = true } e2
        end;
        if mode.app then Util.format ")";
        Util.format "@]"
  | EAssert(_,e) ->       
      let mode = { mode with cat = false } in
        Util.format "@[<2>assert@ "; format_exp_aux mode e; Util.format "@]"
  | ECheckLens(_,e1,la,e2,e3) ->       
      let mode = { mode with cat = false } in
        Util.format "@[<2>check@ ("; 
        format_exp_aux mode e3; 
        Util.format ") : ";
        format_exp_aux mode e1;
        format_lensarrow la;
        format_exp_aux mode e2;
        Util.format "@]"      
  | EAtomCats(_,ns,e)    -> 
      let imode = { mode with cat = false } in
        Util.format "@[<2>"; 
        if not mode.cat then Util.format "{";
        (match ns with 
            [n] -> format_exp_aux imode n 
          | _  -> 
              Util.format "("; 
              Misc.format_list "," (format_exp_aux imode) ns; 
              Util.format ")");
        Util.format "=@,"; 
        format_exp_aux imode e; 
        if not mode.cat then Util.format "}";
        Util.format "@]"
  | EAtomAlts(_,ns,e)    -> 
      let imode = { mode with cat = false } in
        Util.format "@[<2>"; 
        if not mode.cat then Util.format "{";
        (match ns with 
            [n] -> format_exp_aux imode n 
          | _  -> 
              Util.format "("; 
              Misc.format_list "|" (format_exp_aux imode) ns; 
              Util.format ")");
        Util.format "=@,"; 
        format_exp_aux imode e; 
        if not mode.cat then Util.format "}";
        Util.format "@]"
  | ECat(_,es)      -> 
      Util.format "{@[<1>"; 
      Misc.format_list 
        ",@ "
        (format_exp_aux { mode with cat = true })
        es; 
      Util.format "@]}"
  | ECons(_,e1,e2)  -> 
      let mode = { mode with cat = false } in 
        (* if we have a spine of cons cells, ending in [], print
           it using list notation. Otherwise, use :: *)
      let rec get_list_elts acc = function
          ENil(_) -> Some (Safelist.rev acc)
        | ECons(_,e,e') -> get_list_elts (e::acc) e'
        | _            -> None in
        Util.format "@["; 
        begin
          match get_list_elts [] e0 with 
              None -> 
                Util.format "(";
                format_exp_aux mode e1; 
                Util.format "::@,"; 
                format_exp_aux mode e2;
                Util.format ")";
            | Some el -> 
                Util.format "["; 
                Misc.format_list ",@ " (format_exp_aux mode) el;
                Util.format "]"
        end;
        Util.format "@]"
  | EDB(_,db)      -> 
      Db.format_t db
  | EDBPred(_,pred)      -> 
      Util.format "(where ";
      Db.Relation.Pred.format_t pred;
      Util.format ")"
  | EDBFD(_,fds) ->
      Util.format "(with ";
      Dbschema.Relschema.Fd.Set.format_t fds;
      Util.format ")"
  | EDBSchema(_,dbs)      -> 
      Dbschema.format_t dbs
  | EFun(_,ps,so,e) -> 
      let mode = { mode with cat = false } in   
      Util.format "@[<2>fun@ ";  
      Misc.format_list 
        "@ "
        format_param 
        ps;
      (match so with 
           None -> ()
         | Some s -> Util.format " : "; format_sort s);
      Util.format "@ ->@ "; 
      format_exp_aux mode e;
      Util.format "@]"
  | ELet(_,bs,e) ->
      let mode = { mode with cat = false } in 
      Util.format "@[<2>";
      format_bindings bs;
      Util.format "@ in@ ";
      format_exp_aux mode e;
      Util.format "@]"
  | EMap(_,ms) -> 
      let mode = { mode with cat = false } in   
      Util.format "{@[<2>";
      Misc.format_list 
        ",@, " 
        (fun (e1,e2) -> format_exp_aux mode e1; Util.format " ->@ "; format_exp_aux mode e2) 
        ms;
      Util.format "@]}"
  | EName(_,n) -> 
      Util.format "@[%s@]" (Misc.whack (string_of_id n))
  | ENil(_) -> 
      Util.format "[]"
  | EProtect(_,e,_) -> 
      let mode = { mode with cat = false } in 
        Util.format "@[protect@ "; format_exp_aux mode e; Util.format "@]"
  | ESchema(_,ss,e) -> 
      let mode = { mode with cat = false } in 
        Util.format "@[<2>";
        format_schema_bindings ss;
        Util.format "@ in@ ";
        format_exp_aux mode e;
        Util.format "@]"
  | EUnion(_,es) -> 
      let mode = { mode with cat = false } in 
        Util.format "@[<2>(";
        Misc.format_list "@ |@ " (format_exp_aux mode) es;
        Util.format ")@]"
  | EInter(_,es) -> 
      let mode = { mode with cat = false } in 
        Util.format "@[<2>(";
        Misc.format_list "@ &@ " (format_exp_aux mode) es;
        Util.format ")@]"
  | EMinus(_,e1,e2) -> 
      let mode = { mode with cat = false } in 
        Util.format "@[<2>(";
        format_exp_aux mode e1;
        Util.format "@ -@ ";
        format_exp_aux mode e2;
        Util.format ")@]"
  | EVar(_,x,_) -> 
      Util.format "@[%s@]" (string_of_qid x)
  | EWild(_,f,l,u,e)  ->
      let rec format_n_bangs n = match n with 
          0 -> ()
        | n -> Util.format "!"; format_n_bangs (n-1) in                
        Util.format "@[";
        if not mode.cat then Util.format "{";
        let imode = { mode with cat = false } in 
          (match l,u with 
               0,true -> Util.format "*"
             | n,true -> format_n_bangs n; Util.format "*"
             | n,false -> format_n_bangs n);
          if f <> [] then 
          (Util.format "\\(@[";
           Misc.format_list ",@ " (format_exp_aux imode) f;
           Util.format "@])");
        Util.format "=@,";
        format_exp_aux imode e;
        if not mode.cat then Util.format "}";
        Util.format "@]" 

and format_exp e = format_exp_aux { app = false; cat = false } e

and format_binding (BDef(_,x,ps,s,e)) = 
  Util.format "@[<2>%s" (string_of_id x);
  if ps <> [] then Util.format "@ "; 
  Misc.format_list "@ " (fun pi -> Util.format "("; format_param pi; Util.format ")") ps;
  Util.format "@ :@ ";
  format_sort s;
  Util.format "@ =@ ";
  format_exp e;
  Util.format "@]"

and format_bindings bs = 
  Util.format "@[let "; 
  Misc.format_list "@\nand " format_binding bs;
  Util.format "@]"

and format_schema_binding(SDef(_,x,e)) = 
  Util.format "@[<2>%s =@ " (string_of_id x);
  format_exp e;
  Util.format "@]"

and format_schema_bindings ss = 
  Util.format "@[schema "; 
  Misc.format_list "@\nand " format_schema_binding ss;
  Util.format "@]"

and format_decl = function
  | DLet(i,bs) -> format_bindings bs
  | DMod(_,i,ds) -> 
      Util.format "@[module %s =@\n  @[" (string_of_id i);
      Misc.format_list "@\n" format_decl ds;
      Util.format "@]end@]"
  | DSchema(i,ss) -> format_schema_bindings ss
  | DTest(_,e,tr) -> 
      Util.format "@[<2>test@ ";
      Util.format "@[";
      format_exp e;
      Util.format "@ =@ ";
      (match tr with 
           ErrorResult -> Util.format "error"
         | PrintResult -> Util.format "?"
         | Result e2   -> format_exp e2);
      Util.format "@]@]"

let id_of_modl (MDef(_,m,_,_)) = m
let info_of_module (MDef(i,_,_,_)) = i

let format_module (MDef(_,i,qs,ds)) = 
  Util.format "@[module %s =@\n  @[" (string_of_id i);
  if qs <> [] then 
    Misc.format_list 
      "@\n" 
      (fun qi -> Util.format "open %s" (string_of_qid qi)) 
      qs;
  Misc.format_list "@\n" format_decl ds;
  Util.format "@\n";
  Util.format "@\n@]@]"
