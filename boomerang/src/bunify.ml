open Bsyntax
open Bprint

let msg = Util.format

(* fresh variables *)
let gensym_count = ref 0 
let fresh_svar con = 
  let n = !gensym_count in 
  incr gensym_count;
  (n,ref con)
let fresh_sort con = SVar(fresh_svar con)

(* raw_error: unexpected a raw sort variable---these should have been
   eliminated using fix_sort. *)
let raw_error i x = 
  Berror.run_error i 
    (fun () -> msg "unexpected raw sort variable %s" (Id.string_of_t x))

(* svl_of_sl: convert a sort list to an svar list *)
let svl_of_sl i l = 
  Safelist.rev
    (Safelist.fold_left 
       (fun l si -> match si with 
          | SVar(svi) -> svi::l
          | _ -> Berror.run_error i 
              (fun () -> msg "expected sort variable"))
       [] l)

(* svs_of_sl: convert a sort list to an svar list *)
let svs_of_sl i l = 
  Safelist.fold_left 
    (fun svs si -> match si with 
       | SVar(svi) -> SVSet.add svi svs
       | _ -> Berror.run_error i 
           (fun () -> msg "expected sort variable"))
    SVSet.empty l

(* svs_of_svl: convert a svl to an SVSet.t *)
let svs_of_svl l = 
  Safelist.fold_left 
    (fun s svi -> SVSet.add svi s)
    SVSet.empty l 

(* sl_of_svs: convert an SVSet.t to an svar list *)
let sl_of_svs s = 
  Safelist.rev 
    (SVSet.fold (fun svi l -> SVar svi::l) s [])

(* substitute a sort *)
let rec subst_sort al eq s0 = 
  let go s = subst_sort al eq s in
    match s0 with
      | SVar _ | SRawVar _ -> 
          begin 
            try snd (Safelist.find (fun (x,_) -> eq s0 x) al) 
            with Not_found -> s0
          end
      | SUnit | SString | SInteger | SRegexp | SLens | SCanonizer -> s0
      | SFunction(x1,s1,s2) -> SFunction(x1, go s1, go s2)
      | SProduct(s1,s2)  -> SProduct(go s1, go s2)
      | SData(sl,x1)     -> SData(Safelist.map go sl,x1) 
      | SRefine(x1,s1,e1) -> SRefine(x1,go s1,subst_exp al eq e1)

and subst_exp al eq e0 = assert false 
(*
  let go e = subst_exp al eq e in
  let go_s s = subst_sort al eq s in
  let go_so so = Misc.map_option go_s so in
  let go_p p = subst_pat al eq p in
    match e0 with
      | EUnit | EString _ | ECSet _ -> e0

      | EApp(i,e1,e2,s0) -> EApp(i,go e1,go e2,go_so s0)
          
      | EVar(i,x0,so0) -> EVar(i,x0,go_so so0)

      | EFun(i0,Param(i1,x0,s0),so1,e1,so2) ->
          EFun(i0,Param(i1,x0,go_s s0),go_so so1,go e1,go_so so2)

      | ELet(i0,Bind(i1,p0,so0,e1),e2,so1) ->
          ELet(i0,Bind(i1,go_p p0,go_so so0,go e1),go e2,go_so so1)

      | EPair(i0,e1,e2,so0) -> EPair(i0,go e1,go e2,go_so so0)

      | ECase(i,e1,pel0,so0) ->
          ECase(i,go e1,Safelist.map (fun (p, e) -> (go_p p,go e)) pel0,go_so so0)
*)
and subst_pat al eq p0 = p0

(* zonking: removes "chains" of variable references producing during
   unification. *)
let rec zonk i s0 = match s0 with 
  | SUnit | SString | SInteger | SRegexp | SLens | SCanonizer -> s0
  | SFunction(x0,s1,s2) -> SFunction(x0,zonk i s1,zonk i s2)
  | SData(sl,x) -> SData(Safelist.map (zonk i) sl,x)
  | SProduct(s1,s2) -> SProduct(zonk i s1,zonk i s2)
  | SVar(_,sor) -> begin match !sor with 
      | Bnd s -> 
          let s' = zonk i s in 
            sor := Bnd s';
            s'
      | _ -> s0     
    end
  | SRawVar x -> raw_error i x
  | SRefine(x0,s0,e0) -> assert false

let generalize i env_svs s0 = 
  let s = zonk i s0 in 
  let fsvs = SVSet.diff (free_svs i s) env_svs in 
    (fsvs,s)

let instantiate_cases i (qx,(svl,cl)) = 
  let s0 = SData(Safelist.map (fun svi -> SVar(svi)) svl,qx) in
  let al = 
    SVSet.fold 
      (fun (x,cr) l -> (x,SVar (fresh_svar !cr))::l)
      (svs_of_svl svl) [] in 
  let eq = function
    | SVar(x,_) -> (=) x
    | _ -> (fun _ -> false) in 
  let go = subst_sort al eq in 
    (go s0, Safelist.map (fun (x,so) -> (x,Misc.map_option go so)) cl)

let instantiate i (svs,s0) = 
  let al = 
    SVSet.fold 
      (fun (x,cr) l -> (x,SVar (fresh_svar !cr))::l) 
      svs [] in 
  let eq = function
    | SVar(x,_) -> (=) x
    | _ -> (fun _ -> false) in 
  let go = subst_sort al eq in 
    go s0

(* instance: assumes sort has been zonk'd *)
let rec instance s c = match s with 
  | SUnit -> BSSet.mem Unt c
  | SString -> BSSet.mem Str c
  | SRegexp -> BSSet.mem Reg c
  | SLens -> BSSet.mem Lns c
  | SCanonizer -> BSSet.mem Can c
  | _ -> false

type ('a,'b,'c) three = Fst of 'a | Snd of 'b | Thd of 'c

let rec get_con_sort = function 
  | SVar(x,sor) -> begin match !sor with 
      | Fre   -> Fst x 
      | Bnd s -> get_con_sort s
      | Con c -> Snd (x,c)
    end
  | s -> Thd s
      
let occurs_check i sv1 s2 = 
  (* occurrs check *)
  if SVSet.mem sv1 (free_svs i s2) then
    begin 
      Bprint.reset_name_ctxt ();
      Berror.sort_error i 
        (fun () -> 
           msg "occurs check failed during unification of %t and %t."
             (fun _ -> format_svar true sv1)
             (fun _ -> format_sort s2))
    end

let rec add_con (x,br) c = match !br with 
  | Fre           -> br := Con c
  | Bnd (SVar sv) -> add_con sv c
  | Con c1        -> br := Con (BSSet.union c1 c)
  | _             -> assert false

let rec set_sort (x,br) s = match !br with
  | Bnd (SVar sv) -> set_sort sv s
  | _             -> br := Bnd s
  
let rec unify i s1 s2 = 
(*  msg "@[UNIFY@\n"; *)
(*  msg "  S1="; format_sort s1; *)
(*  msg "@\n  S2="; format_sort s2; *)
(*  msg "@\n@]%!"; *)
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
      
  | SFunction(x1,s11,s12),SFunction(x2,s21,s22) -> 
      (* TODO must ids be equal? *)
      Id.equal x1 x2 && (unify i s21 s11) && (unify i s12 s22)

  (* data types *)
  | SData(ss1,q1),SData(ss2,q2) -> 
      let ok,ss12 = 
        try true, Safelist.combine ss1 ss2 
        with Invalid_argument _ -> (false,[]) in 
      Safelist.fold_left
        (fun b (s1,s2) -> b && unify i s1 s2)
        (ok && (Qid.equal q1 q2))
        ss12
      
  (* type variables *)
  | SVar sv1,_ -> unifyVar i false sv1 s2
  | _,SVar sv2 -> unifyVar i true sv2 s1
  | _        -> false in
(*    msg "@[UNIFY RES=%b@\n" res; *)
(*    msg "  S1="; format_sort s1; *)
(*    msg "@\n  S2="; format_sort s2; *)
(*    msg "@\n@]"; *)
  res

and unifyVar i flip sv1 s2 = 
(*   msg "UNIFY_VAR: "; *)
(*   format_svar true sv1; *)
(*   msg " ~ "; *)
(*   format_sort s2; *)
(*   msg "@\n";  *)
  let (x1,sor1) = sv1 in 
  match !sor1,s2 with    
    | Bnd s1,_ -> 
        if flip then unify i s2 s1 
        else unify i s1 s2 
    | Fre,SVar(_,sor2) -> 
        begin
          match get_con_sort s2 with 
            | Fst x2 -> 
                if x1 = x2 then ()
                else sor1 := Bnd s2
            | Snd (x2,c)  -> 
                if x1 = x2 then ()
                else 
                  (sor2 := Con c;
                   sor1 := Bnd s2)
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
                   (add_con sv2 c1;
                    sor1 := Bnd s2);
                 true

             | Snd (x2,c) -> 
                 if x1=x2 then () 
                 else
                   (add_con sv2 c1;
                    sor1 := Bnd s2);
                 true
             | Thd s' ->            
                 if instance s2 c1 then
                   (occurs_check i sv1 s2;
                    set_sort sv2 s';
                    sor1 := Bnd s2;
                    true)
                 else false
         end        
  | Con c1,s2 -> 
      if instance s2 c1 then
        (occurs_check i sv1 s2;
         sor1 := Bnd s2;
         true)
      else false

        
