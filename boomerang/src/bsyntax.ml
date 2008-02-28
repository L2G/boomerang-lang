(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  *)
(*                                                                             *)
(* This library is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU Lesser General Public                  *)
(* License as published by the Free Software Foundation; either                *)
(* version 2.1 of the License, or (at your option) any later version.          *)
(*                                                                             *)
(* This library is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           *)
(* Lesser General Public License for more details.                             *)
(*******************************************************************************)
(* /boomerang/src/bsyntax.ml                                                   *)
(* Boomerang abstract syntax                                                   *)
(* $Id$                                                                        *)
(*******************************************************************************)

(* imports *)
let ( @ ) = Safelist.append 
let sprintf = Printf.sprintf
let msg = Util.format

(* parsing info *)
type i = Info.t

(* identifiers and qualified identifiers *)
type id = i * string
type qid = id list * id

let mk_native_prelude_qid x = 
  let i = Info.M (sprintf "%s built-in" x) in 
  ([(i,"Native");(i,"Prelude")],(i,x))

let mk_prelude_qid x = 
  let i = Info.M (sprintf "%s built-in" x) in 
  ([(i,"Prelude")],(i,x))

let info_of_id (i,_) = i
let string_of_id (_,x) = x
let id_compare (_,x1) (_,x2) = compare x1 x2
let id_equal (i1:id) (i2:id) = (id_compare i1 i2 = 0)
let qid_of_id (x:id) = [],x
let id_dot x1 (qs2,x2) = (x1::qs2,x2)
let splice_id_dot x1 (qs2,x2) = (qs2@[x1],x2)
let qid_compare (qs1,x1) (qs2,x2) = 
  let rec ids_compare xs1 xs2 = match xs1,xs2 with
    | [],[] -> 0
    | _,[]  -> 1
    | [],_  -> -1
    | (x1::t1),(x2::t2) -> 
        let hd_compare = id_compare x1 x2 in
          if (hd_compare <> 0) then hd_compare 
          else ids_compare t1 t2
  in
    ids_compare (qs1@[x1]) (qs2@[x2])
let qid_equal q1 q2 = (qid_compare q1 q2 = 0)
let id_prefix q1 il2 = 
  let (is1,i1) = q1 in 
  let il1 = is1 @ [i1] in
    ((Safelist.length il1) <= (Safelist.length il2)) 
    && (Safelist.for_all 
          (fun (i1,i2) -> id_equal i1 i2)
          (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))

let qid_qualifiers (qs,_) = qs

let info_of_qid = function
  | [],(i,_) -> i
  | (i1,_)::_,(i2,_) -> Info.merge_inc i1 i2

let string_of_qid (qs,i) = 
  Printf.sprintf "%s%s"
    (Safelist.fold_left 
       (fun acc qi -> Printf.sprintf "%s%s." acc (string_of_id qi)) 
       ""
       qs)    
    (string_of_id i)

(* sorts, parameters, expressions *)
type sort = 
    | SUnit                            (* unit *)
    | SString                          (* strings *)
    | SRegexp                          (* regular expressions *)
    | SLens                            (* lenses *)
    | SCanonizer                       (* canonizers *)
    | SFunction of sort * sort         (* funtions *)
    | SData of sort list * qid         (* data types *)
    | SProduct of sort * sort          (* products *)
    | SVar of svar                     (* sort variables *)
    | SRawVar of id                    (* parsed sort variables *)
and sbnd = Fre | Bnd of sort | Con of scon
and scon = Str | Reg | Lns 
and svar = sbnd ref * int

module SVSet = Set.Make
(struct
  type t = svar 
  let compare (_,x) (_,y) = Pervasives.compare x y
 end)                          

type scheme = SVSet.t * sort

let scheme_of_sort s = (SVSet.empty,s)

type param = Param of i * id * sort

type binding = Bind of i * pat * sort option * exp

and exp = 
    (* lambda calculus *)
    | EApp of i  * exp * exp
    | EVar of i * qid
    | EFun of i * param * sort option * exp 
    | ELet of i * binding * exp

    (* with products, units, sums *)
    | EUnit of i
    | EPair of i * exp * exp
    | ECase of i * exp * (pat * exp) list
        
    (* placeholders: hold dictionary parameters in type inference *)
    | EOver of i * osym 

    (* constants *)
    | EString of i * Bstring.t
    | ECSet of i * bool * (Bstring.sym * Bstring.sym) list 

and osym = 
  | ODot 
  | OBar 
  | OTilde
  | OIter

and pat = 
  | PWld of i
  | PUnt of i
  | PVar of i * id 
  | PVnt of i * id * pat option
  | PPar of i * pat * pat

(* declarations *)
type test_result =
    | TestValue of exp
    | TestError
    | TestShow
    | TestSort of sort option
    | TestLensType of (exp option * exp option)

type decl = 
    | DLet of i * binding  
    | DType of i * sort list * id * (id * sort option) list 
    | DMod of i * id * decl list 
    | DTest of i * exp * test_result
        
(* modules *)
type modl = Mod of i * id * id list * decl list

let (^>) s1 s2 = SFunction(s1,s2)

(* functions on sorts *)
(* fresh variables *)
let gensym_count = ref 0 
let fresh_svar con = 
  let n = !gensym_count in 
  incr gensym_count;
  (ref con,n)

let fresh_sort con = SVar(fresh_svar con)

let raw_error i = 
  Berror.run_error i 
    (fun () -> msg "unexpected raw sort variable")

let svs_of_svl l = 
  Safelist.fold_left 
    (fun s svi -> SVSet.add svi s) 
    SVSet.empty l

let svs_of_sl i l = 
  Safelist.fold_left 
    (fun s si -> match si with 
       | SVar(svi) -> SVSet.add svi s
       | _ -> Berror.run_error i (fun () -> msg "expected sort variable"))
    SVSet.empty l                

let svs_sl_of_svl l = 
  Safelist.fold_left 
    (fun (s,l) svi -> (SVSet.add svi s, SVar svi::l)) 
    (SVSet.empty,[]) l
    
let sl_of_svs s = 
  SVSet.fold (fun svi l -> SVar svi::l) 
    s [] 

let svl_al_of_rl i rl = 
  Safelist.fold_left 
    (fun (svl,al) si -> 
       match si with 
         | SRawVar(x) -> begin 
             try (Safelist.assoc x al::svl,al)
             with Not_found -> 
               let s_fresh = fresh_sort Fre in 
               (s_fresh::svl, (x,s_fresh)::al)
           end
         | _ -> Berror.run_error i (fun () -> msg "expected sort variable"))
    ([],[]) rl 

let subst al eq s0 = 
  let rec go s = match s with
    | SVar _ | SRawVar _ -> 
        begin 
          try snd (Safelist.find (fun (x,_) -> eq s x) al) 
          with Not_found -> s
        end
    | SUnit | SString | SRegexp | SLens | SCanonizer -> s 
    | SFunction(s1,s2) -> SFunction(go s1, go s2)
    | SProduct(s1,s2)  -> SProduct(go s1, go s2)
    | SData(sl,x1)     -> SData(Safelist.map go sl,x1) in 
  go s0 

let rec fix_sort al s = match s with
  | SVar _ | SUnit | SString | SRegexp | SLens | SCanonizer -> (al,s)
  | SFunction(s1,s2) -> 
      let al1,s1' = fix_sort al s1 in 
      let al2,s2' = fix_sort al s2 in 
      (al2,SFunction(s1',s2'))
    | SProduct(s1,s2)  -> 
        let al1,s1' = fix_sort al s1 in 
        let al2,s2' = fix_sort al s2 in 
        (al2,SProduct(s1',s2'))
    | SData(sl,x1) -> 
        let al',sl' = 
          Safelist.fold_left 
            (fun (al,sl) si -> 
               let al',si' = fix_sort al si in 
                 (al',si'::sl))
            (al,[]) sl in 
        (al',SData(sl',x1))
    | SRawVar(x) -> 
        try (al,snd (Safelist.find (fun (y,_) -> id_equal x y) al))
        with Not_found -> 
          let s_fresh = fresh_sort Fre in 
          ((x,s_fresh)::al,s_fresh) 

(* sv_equal: true iff the two variables are equal *)
let sv_equal (_,x) (_,y) = x=y

(* zonking: removes "chains" of variable references producing during unification. *)
let rec zonk i s0 = match s0 with 
  | SUnit | SString | SRegexp | SLens | SCanonizer -> s0
  | SFunction(s1,s2) -> SFunction(zonk i s1,zonk i s2)
  | SData(sl,x) -> SData(Safelist.map (zonk i) sl,x)
  | SProduct(s1,s2) -> SProduct(zonk i s1,zonk i s2)
  | SVar(sor,_) -> begin match !sor with 
      | Bnd s -> 
          let s' = zonk i s in 
          sor := Bnd s';
          s'
      | _ -> s0     
    end
  | SRawVar _ -> raw_error i      

(* free sort variables *)
let free_svs i s0 = 
  let rec go acc = function
    | SVar(sv1) -> 
        let (sor1,x1) = sv1 in 
        begin match !sor1 with 
          | Bnd s1 -> go acc s1
          | _    -> SVSet.add sv1 acc 
        end
    | SUnit | SString | SRegexp | SLens | SCanonizer -> acc
    | SFunction(s1,s2) -> go (go acc s1) s2
    | SProduct(s1,s2)  -> go (go acc s1) s2
    | SData(sl,s1)     -> Safelist.fold_left go acc sl 
    | SRawVar _        -> raw_error i in 
  go SVSet.empty s0

let generalize i env_svs s0 = 
  let s = zonk i s0 in 
  let fsvs = SVSet.diff (free_svs i s) env_svs in 
  (fsvs,s)

let instantiate_cases i (svs,s0) cl = 
  let al = 
    SVSet.fold 
      (fun (cr,x) l -> (x,SVar (fresh_svar !cr))::l)
      svs [] in 
  let eq = function
    | SVar(_,x) -> (=) x
    | _ -> (fun _ -> false) in 
  let go = subst al eq in 
  (go s0, Safelist.map (fun (x,so) -> (x,Misc.map_option go so)) cl)

let instantiate i scm = 
  let res,_ = instantiate_cases i scm [] in
  res

and merge_con c1 c2 = match c1,c2 with
  | Str,Str -> Str
  | Reg,Reg | Reg,Str | Str,Reg -> Reg
  | _ -> Lns

(* info accessors *)

let info_of_exp = function    
  | EApp (i,_,_)     -> i
  | EVar (i,_)       -> i
  | EFun (i,_,_,_)   -> i
  | ELet (i,_,_)     -> i
  | EUnit(i)         -> i
  | EPair(i,_,_)     -> i
  | ECase(i,_,_)     -> i
  | EString (i,_)    -> i
  | ECSet (i,_,_)    -> i
  | EOver (i,_)      -> i 
      
let info_of_pat = function
  | PWld(i)     -> i
  | PUnt(i)     -> i
  | PVar(i,_)   -> i
  | PVnt(i,_,_) -> i
  | PPar(i,_,_) -> i


let info_of_module = function
  | Mod(i,_,_,_) -> i

let id_of_module = function
  | Mod(_,x,_,_) -> x

let sort_of_param = function
  | Param(_,_,s) -> s

let id_of_param = function
  | Param(_,x,_) -> x

let name_ctxt : (int * ((int * string) list)) ref = ref (0,[])
let reset_name_ctxt () = 
  name_ctxt := (0,[])

let format_svar_tag x = 
  let i,assoc = !name_ctxt in 
  let x_str = 
    try Safelist.assoc x assoc
    with Not_found -> 
      let chr = Char.chr (97 + i mod 26) in 
      let sub = i / 26 in 
      let x_str = 
        if sub=0 then Printf.sprintf "'%c" chr
        else Printf.sprintf "'%c_%d" chr sub in 
        name_ctxt := (succ i,(x,x_str)::assoc);
        x_str in     
    Util.format "%s" x_str
      (* Util.format "x%d" x *)

(* TODO only use parens when necessary *)
let rec format_sort = function
  | SUnit -> Util.format "unit"
      
  | SString -> Util.format "string"
      
  | SRegexp -> Util.format "regexp"
      
  | SLens -> Util.format "lens"
      
  | SCanonizer -> Util.format "canonizer"
      
  | SFunction(s1, s2) ->
      Util.format "@[(";
      format_sort s1;
      Util.format "@ ->@ ";
      format_sort s2;
      Util.format ")@]"
        
  | SProduct(s1,s2) -> 
      Util.format "@[<2>";
      format_sort s1;
      Util.format "@ *@ ";
      format_sort s2;
      Util.format "@]"
  | SData([],x1) -> Util.format "@[%s@]" (string_of_qid x1)
  | SData([s],x1) -> 
      msg "@[";
      format_sort s;
      msg "@ ";
      msg "%s@]" (string_of_qid x1)
  | SData(ms,x1) ->         
      Util.format "@[[@[<2>";
      Misc.format_list ",@ " (format_sort) ms;      
      Util.format "@]]@ %s@]" (string_of_qid x1)
  | SVar(sv) -> format_svar false sv
  | SRawVar(x) -> Util.format "~%s" (string_of_id x)

and format_svar print_cons (cr,x) = match !cr with
  | Bnd s -> format_sort s
      (* msg "@[[%d:=%t]@]" x (fun _ -> format_sort s)*)
  | Fre   -> 
      format_svar_tag x
  | Con c -> 
      format_cons c; 
      msg " ";
      format_svar_tag x;
 
and format_cons = function
  | Str -> Util.format "Str"
  | Reg -> Util.format "Reg"
  | Lns -> Util.format "Lns"

let format_scheme (svs,s) = 
  Util.format "@[<2>";
  let con_fsvs = 
    SVSet.fold 
      (fun ((cr,_) as svi) l -> match !cr with | Con _ -> svi::l | _ -> l)
      svs [] in 
  if Safelist.length con_fsvs <> 0 then
    (Misc.format_list ", " (format_svar true) con_fsvs;
     Util.format " => ");
  format_sort s;
  Util.format "@]"

let rec format_pat = function
  | PWld _ -> Util.format "_"
  | PUnt _ -> Util.format "()"
  | PVar(_,x) -> Util.format "%s" (string_of_id x)
  | PPar(_,p1,p2) -> 
      Util.format "@[<2>(";
      format_pat p1;
      Util.format ",@,";
      format_pat p2;
      Util.format ")@]";
  | PVnt(_,l,None) -> Util.format "%s" (string_of_id l)
  | PVnt(_,l,Some p1) ->  
      Util.format "@[<2>(%s@ " (string_of_id l);
      format_pat p1;
      Util.format ")@]"

and format_param = function
  | Param(_,x,s) ->
      Util.format "@[(%s:" (string_of_id x);
      format_sort s;
      Util.format ")@]"

and format_binding (Bind (_, x, s, e)) =
  Util.format "@[<2>";
  format_pat x;
  (match s with
     | None -> ()
     | Some s -> Util.format "@ :@ "; format_sort s);     
  Util.format "@ =@ ";
  format_exp e;
  Util.format "@]"

and format_exp = function
  | EApp (_, e1, e2) ->
	Util.format "@[<2>(";
	format_exp e1;
	Util.format "@ ";
	format_exp e2;
	Util.format ")@]"

    | EVar (_, qid) ->
	Util.format "@[%s@]" (string_of_qid qid)

    | EFun (_, p, s, e) ->
	Util.format "@[<2>(fun@ ";
	format_param p;
	(match s with
	   | None -> ()
           | Some s -> Util.format "@ :@ "; format_sort s);
	Util.format "@ ->@ ";
	format_exp e;
	Util.format ")@]";

    | ELet (_, b, e) ->
	Util.format "@[<2>let@ ";
	format_binding b;
	Util.format "@ in@ ";
	format_exp e;
	Util.format "@]";

    | EUnit _ -> Util.format "()"

    | EPair(_,e1,e2) -> 
        Util.format "@[<2>(";
        format_exp e1;
        Util.format ",";
        format_exp e2;
        Util.format ")@]"

    | ECase(_,e1,pl) -> 
        Util.format "@[<2>match@ ";
        format_exp e1;
        Util.format "@ with@ ";
        Misc.format_list "@ |@ "
          (fun (p,e) -> 
             Util.format "@[<2>@ ";
             format_pat p;
             Util.format "@ ->@ ";
             format_exp e;
             Util.format "@]")
          pl;
        Util.format "@]"

    | EString (_, s) ->
	Util.format "@[\"%s\"@]" (Bstring.escaped (Bstring.string_of_t s))

    | ECSet (_, negated, ranges) ->
	Util.format "@[[";
	(if negated
	 then Util.format "^"
	 else ());
	Misc.format_list ""
	  (fun (first, last) ->
	     if Bstring.compare_sym first last = 0
	     then Util.format "%s" (Bstring.escaped_repr first)
	     else Util.format "%s-%s" 
	       (Bstring.escaped_repr first)
	       (Bstring.escaped_repr last))
	  ranges;
	  Util.format "]@]"

    | EOver(_,s) -> format_sym s

and format_sym = function
  | ODot -> Util.format "concat"
  | OBar -> Util.format "union"
  | OTilde -> Util.format "swap"
  | OIter -> Util.format "iter"

and format_test_result tr =
  match tr with
    | TestValue e -> 
        Util.format "= @[<2>" ; 
        format_exp e;
        Util.format "@]";
    | TestError -> Util.format "= @[<2>error@]"
    | TestShow -> Util.format "= @[<2>?@]"
    | TestSort(None) -> Util.format ": @[<2>?@]"
    | TestSort(Some s) -> 
        Util.format ": @[<2>";
        format_sort s;
        Util.format "@]"
    | TestLensType(e1o,e2o) -> 
        let format_eo = function
          | None -> Util.format "?"
          | Some e1 -> format_exp e1 in 
        Util.format ": @[<2>";
        format_eo e1o;
        Util.format " <-> ";
        format_eo e2o;
        Util.format "@]"

and format_decl d =
  match d with
      DLet (_, b) ->
	Util.format "@[let@ ";
	format_binding b;
	Util.format "@]"

    | DType(_,svl,x,cl) ->  
        Util.format "@[type@ ";
        (match svl with 
          | [] -> ()
          | [sv] -> format_sort sv
          | _ -> 
              msg "(";
              Misc.format_list ",@ " format_sort svl;
              msg ")");
        msg "%s@ =@ " (string_of_id x);
        Misc.format_list " | "
          (fun (l,s) -> match s with
             | None -> Util.format "%s" (string_of_id l)
             | Some s -> 
                 Util.format "(%s@ " (string_of_id l);
                 format_sort s;
                 Util.format ")")
          cl;
        Util.format "@]"        

    | DMod (i, id, ds) ->
	format_module (Mod (i, id, [], ds))

    | DTest (_, e, tr) ->
	Util.format"@[<2>test@ @[";
	format_exp e;
	Util.format "@ =@ ";
	format_test_result tr;
	Util.format "@]@]"
and format_module (Mod (_, id, qs, ds)) =
  Util.format "@[module %s =@\n  @[" (string_of_id id);
  if qs <> [] then 
    Misc.format_list "@\n" 
      (fun x -> Util.format "open %s" (string_of_id x))
      qs;
  Misc.format_list "@\n" format_decl ds;
  Util.format "@\n@]@\n@]"

(* string_of_sort : s -> string
 *
 * [string_of_sort s] produces a string representing [s] 
 *)
let string_of_sort s = Util.format_to_string (fun () -> format_sort s)
let string_of_scheme s = Util.format_to_string (fun () -> format_scheme s)
let string_of_param p = Util.format_to_string (fun () -> format_param p)
let string_of_pat p = Util.format_to_string (fun () -> format_pat p)

(* instance: assumes sort has been zonk'd *)
let rec instance s c = match s,c with
  | SString,Str | SRegexp,Str | SLens,Str | SCanonizer,Str -> Some s
  | SString,Reg -> Some SRegexp
  | SRegexp,Reg | SLens,Reg | SCanonizer,Reg -> Some s
  | SString,Lns | SRegexp,Lns -> Some SLens 
  | SLens,Lns | SCanonizer,Lns -> Some s
  | _ -> None

type ('a,'b,'c) three = Fst of 'a | Snd of 'b | Thd of 'c

let rec get_con_sort = function 
  | SVar(sor,x) -> begin match !sor with 
      | Fre   -> Fst x 
      | Bnd s -> get_con_sort s
      | Con c -> Snd c
    end
  | s -> Thd s
      
let occurs_check i sv1 s2 = 
  (* occurrs check *)
  if SVSet.mem sv1 (free_svs i s2) then
    begin 
      reset_name_ctxt ();
      Berror.sort_error i 
        (fun () -> 
           msg "occurs check failed during unification of %t and %t."
             (fun _ -> format_svar true sv1)
             (fun _ -> format_sort s2))
    end

let rec set_con (br,x) c = match !br with 
  | Fre           -> br := Con c
  | Bnd (SVar sv) -> set_con sv c
  | Con _         -> br := Con c
  | _             -> assert false

let rec set_sort (br,x) s = match !br with
  | Bnd (SVar sv) -> set_sort sv s
  | _             -> br := Bnd s
  
let rec unify i s1 s2 = 
 (* msg "@[UNIFY@\n";
    msg "  S1="; format_sort s1;
    msg "@\n  S2="; format_sort s2;
    msg "@\n@]%!"; *)
  let res = match s1,s2 with

  (* equal types *)
  | SUnit,SUnit           -> true
  | SString,SString       -> true
  | SRegexp,SRegexp       -> true
  | SLens,SLens           -> true
  | SCanonizer,SCanonizer -> true

  (* products *)
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      (unify i s11 s21) && (unify i s12 s22)
      
  | SFunction(s11,s12),SFunction(s21,s22) -> 
      (unify i s21 s11) && (unify i s12 s22)

  (* data types *)
  | SData(ss1,x1),SData(ss2,x2) -> 
      let ok,ss12 = 
        try true, Safelist.combine ss1 ss2 
        with Invalid_argument _ -> (false,[]) in 
      Safelist.fold_left
        (fun b (s1,s2) -> b && unify i s1 s2)
        (ok && qid_equal x1 x2)
        ss12
      
  (* type variables *)
  | SVar sv1,_ -> unifyVar i false sv1 s2
  | _,SVar sv2 -> unifyVar i true sv2 s1
  | _        -> false in
    (* msg "@[UNIFY RES=%b@\n" res;
       msg "  S1="; format_sort s1;
       msg "@\n  S2="; format_sort s2;
       msg "@\n@]"; *)
  res

and unifyVar i flip sv1 s2 = 
  (* msg "UNIFY_VAR: ";
  format_svar true sv1;
  msg " ~ ";
  format_sort s2;
  msg "@\n"; *)
  let (sor1,x1) = sv1 in 
  match !sor1,s2 with    
    | Bnd s1,_ -> 
        if flip then unify i s2 s1 
        else unify i s1 s2 
    | Fre,SVar(sor2,_) -> 
        begin
          match get_con_sort s2 with 
            | Fst x2 -> 
                if x1 = x2 then ()
                else sor1 := Bnd s2
            | Snd c  -> 
                sor2 := Con c;
                sor1 := Bnd s2
            | Thd s' -> 
                occurs_check i sv1 s2;
                sor2 := Bnd s';
                sor1 := Bnd s2
        end;
        true
        
    | Fre,_ -> 
        occurs_check i sv1 s2;
        sor1 := Bnd s2; 
        true

     | Con c1,SVar((sor2,x2) as sv2) -> 
         begin match get_con_sort s2 with 
             | Fst x2 -> 
                 if x1=x2 then ()
                 else 
                   (set_con sv2 c1;
                    sor1 := Bnd s2);
                 true

             | Snd c  -> 
                 set_con sv2 c;
                 sor1 := Bnd s2;
                 true
             | Thd s ->                 
                 begin match instance s2 c1 with
                   | None -> false
                   | Some s' -> 
                       occurs_check i sv1 s2;
                       set_sort sv2 s';
                       sor1 := Bnd s2;
                       true
                end
         end        
  | Con c1,s2 -> 
      begin 
        match instance s2 c1 with
        | None -> false
        | Some s' -> 
            occurs_check i sv1 s2;
            sor1 := Bnd s';
            true
      end


        
