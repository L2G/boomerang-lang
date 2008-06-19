open Bsyntax
open Bident
open Bprint 

let msg = Util.format

(* These are big and ugly, but they're simple... and substitution is
tricky enough that I think it's better to have them written out
explicitly and (I hope) carefully. *)

(* Safelist.assoc generalized over an arbitrary equality function *)
let rec gen_assoc eq x = function
  | [] -> raise Not_found
  | (y,s)::rest -> if eq x y then s else gen_assoc eq x rest

(* FREE SORT VARIABLES *)
let rec free_svars_pat acc = function
  | PVar(_,_,Some s) -> free_svars_sort acc s
  | PVar(_,_,None)   -> acc
  | PVnt(_,_,Some p) -> free_svars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> free_svars_pat (free_svars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and free_svars_sort acc = function
  | SVar a -> 
      Id.Set.add a acc 
  | SFunction(x,s1,s2) -> 
      let acc1 = free_svars_sort acc s1 in 
      let acc2 = free_svars_sort acc1 s2 in 
      acc2
  | SProduct(s1,s2) -> 
      let acc1 = free_svars_sort acc s1 in 
      let acc2 = free_svars_sort acc1 s2 in 
      acc2
  | SData(sl,qx)       -> 
      Safelist.fold_left free_svars_sort acc sl
  | SRefine(x,s1,e2) -> 
      let acc1 = free_svars_sort acc s1 in 
      let acc2 = free_svars_exp acc1 e2 in 
      acc2
  | SForall(a,s1) -> 
      let acc1 = free_svars_sort acc s1 in 
      Id.Set.remove a acc1
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer ->
      acc
and free_svars_exp acc = function
  | EApp(_,e1,e2) -> 
      let acc1 = free_svars_exp acc e1 in 
      let acc2 = free_svars_exp acc1 e2 in 
      acc2
  | EOver(_,_,el) -> 
      Safelist.fold_left free_svars_exp acc el
  | EFun(_,Param(_,_,s1),so2,e3) -> 
      let acc1 = free_svars_sort acc s1 in 
      let acc2 = match so2 with None -> acc1 | Some s2 -> free_svars_sort acc1 s2 in 
      let acc3 = free_svars_exp acc2 e3 in 
      acc3
  | ELet(_,Bind(_,p1,so2,e3),e4) ->
      let acc1 = free_svars_pat acc p1 in
      let acc2 = match so2 with None -> acc1 | Some s2 -> free_svars_sort acc1 s2 in 
      let acc3 = free_svars_exp acc2 e3 in 
      let acc4 = free_svars_exp acc3 e4 in 
      acc4
  | ETyFun(_,a,e1) -> 
      let acc1 = free_svars_exp acc e1 in 
      Id.Set.remove a acc1
  | ETyApp(_,e1,s2) -> 
      let acc1 = free_svars_exp acc e1 in 
      let acc2 = free_svars_sort acc1 s2 in 
      acc2
  | ECast(_,f1,t2,_,e3) ->
      let acc1 = free_svars_sort acc f1 in 
      let acc2 = free_svars_sort acc1 t2 in 
      let acc3 = free_svars_exp acc2 e3 in 
      acc3 
  | EPair(_,e1,e2) -> 
      let acc1 = free_svars_exp acc e1 in 
      let acc2 = free_svars_exp acc1 e2 in 
      acc2
  | ECase(_,e1,cl2,s3) -> 
      let acc1 = free_svars_exp acc e1 in 
      let acc2 = Safelist.fold_left 
        (fun acci (pi,ei) -> 
           let acci1 = free_svars_pat acci pi in 
           let acci2 = free_svars_exp acci1 ei in 
           acci2)
        acc1 cl2 in 
      let acc3 = free_svars_sort acc2 s3 in 
      acc3
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ | EVar _ -> 
      acc

(* SORT SUBSTITUTION *)
let free_svars_in_subst subst = 
  Safelist.fold_left 
    (fun acc (x,s) -> free_svars_sort (Id.Set.add x acc) s)
    Id.Set.empty subst

let rec fresh_svar subst a = 
  let clashes = free_svars_in_subst subst in 
  let rec aux a = 
    if Id.Set.mem a clashes then aux (Id.prime a)
    else a in 
  aux a 

(* TODO: optimize all of these so we don't cons up new ASTs unless
   they've actually changed!*)
let rec subst_svars_pat subst p0 = match p0 with
  | PVar(i,x,Some s) ->       
      let new_s = subst_svars_sort subst s in 
      PVar(i,x,Some new_s)
  | PVar(i,x,None) -> 
      p0
  | PVnt(i,qx,Some p) -> 
      let new_p = subst_svars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> 
      p0
  | PPar(i,p1,p2) -> 
      let new_p1 = subst_svars_pat subst p1 in  
      let new_p2 = subst_svars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> p0
and subst_svars_sort subst s0 = match s0 with 
  | SVar a -> (try gen_assoc Id.equal a subst with Not_found -> s0)
  | SFunction(x,s1,s2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      SFunction(x,new_s1,new_s2)
  | SProduct(s1,s2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      SProduct(new_s1,new_s2)
  | SData(sl,qx) -> 
      let new_sl = Safelist.map (subst_svars_sort subst) sl in 
      SData(new_sl,qx)
  | SRefine(x,s1,e2) -> 
      let new_s1 = subst_svars_sort subst s1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      SRefine(x,new_s1,new_e2)
  | SForall(a,s1) -> 
      let fresh_a = fresh_svar subst a in 
      let safe_s1 = if fresh_a = a then s1 else subst_svars_sort [(a,SVar fresh_a)] s1 in 
      let new_s1 = subst_svars_sort subst safe_s1 in
      SForall(fresh_a,new_s1)
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer -> 
      s0
and subst_svars_exp subst e0 = match e0 with 
  | EApp(i,e1,e2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      EApp(i,new_e1,new_e2) 
  | EOver(i,o,el) -> 
      let new_el = Safelist.map (subst_svars_exp subst) el in 
      EOver(i,o,new_el)
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let new_s2 = subst_svars_sort subst s2 in 
      let new_so3 = Misc.map_option (subst_svars_sort subst) so3 in 
      let new_e4 = subst_svars_exp subst e4 in 
      EFun(i,Param(ip,x1,new_s2),new_so3,new_e4) 
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let new_p1 = subst_svars_pat subst p1 in 
      let new_so2 = Misc.map_option (subst_svars_sort subst) so2 in 
      let new_e3 = subst_svars_exp subst e3 in 
      let new_e4 = subst_svars_exp subst e4 in 
      ELet(i,Bind(ib,new_p1,new_so2,new_e3),new_e4)
  | ETyFun(i,a,e1) -> 
      let fresh_a = fresh_svar subst a in 
      let safe_e1 = if fresh_a = fresh_a then e1 else subst_svars_exp [(a,SVar fresh_a)] e1 in 
      let new_e1 = subst_svars_exp subst safe_e1 in
      ETyFun(i,fresh_a,new_e1) 
  | ETyApp(i,e1,s2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_s2 = subst_svars_sort subst s2 in 
      ETyApp(i,new_e1,new_s2) 
  | ECast(i,f1,t2,b,e3) ->
      let new_f1 = subst_svars_sort subst f1 in 
      let new_t2 = subst_svars_sort subst t2 in 
      let new_e3 = subst_svars_exp subst e3 in 
      ECast(i,new_f1,new_t2,b,new_e3)
  | EPair(i,e1,e2) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_e2 = subst_svars_exp subst e2 in 
      EPair(i,new_e1,new_e2)
  | ECase(i,e1,cl,s3) -> 
      let new_e1 = subst_svars_exp subst e1 in 
      let new_cl = 
        Safelist.map 
          (fun (pi,ei) -> 
             let new_pi = subst_svars_pat subst pi in 
             let new_ei = subst_svars_exp subst ei in 
             (new_pi,new_ei))
          cl in 
      let new_s3 = subst_svars_sort subst s3 in 
      ECase(i,new_e1,new_cl,new_s3)
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ | EVar _ -> e0

(* FREE EXPRESSION VARIABLES *)
let qvs_of_is s = Id.Set.fold (fun xi acc -> Qid.Set.add (Qid.t_of_id xi) acc) s Qid.Set.empty 

let rec free_evars_pat acc = function
  | PVar(_,_,Some s) -> free_evars_sort acc s
  | PVar(_,_,None)   -> acc
  | PVnt(_,_,Some p) -> free_evars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> free_evars_pat (free_evars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and bound_evars_pat acc = function
  | PVar(_,x,_)      -> Id.Set.add x acc
  | PVnt(_,_,Some p) -> bound_evars_pat acc p
  | PVnt(_,_,None)   -> acc
  | PPar(_,p1,p2)    -> bound_evars_pat (bound_evars_pat acc p1) p2
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> acc
and free_evars_sort acc = function
  | SFunction(x,s1,s2) ->       
      let acc2 = 
        Qid.Set.remove (Qid.t_of_id x) 
          (free_evars_sort acc s2) in 
      let acc1 = free_evars_sort acc2 s1 in
      acc1
      
  | SProduct(s1,s2) -> 
      let acc1 = free_evars_sort acc s1 in 
      let acc2 = free_evars_sort acc1 s2 in 
      acc2 
  | SData(sl,_) -> 
      Safelist.fold_left free_evars_sort acc sl
  | SRefine(x,s1,e2) ->
      let acc2 = Qid.Set.remove (Qid.t_of_id x) (free_evars_exp acc e2) in 
      let acc1 = free_evars_sort acc2 s1 in 
      acc1 
  | SForall(_,s1) -> 
      free_evars_sort acc s1
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer | SVar _ -> 
      acc
and free_evars_exp acc = function
  | EVar(_,q) -> 
      Qid.Set.add q acc
  | EApp(_,e1,e2) -> 
      let acc1 = free_evars_exp acc e1 in 
      let acc2 = free_evars_exp acc1 e2 in
      acc2
  | EOver(_,_,el) -> 
      Safelist.fold_left free_evars_exp acc el
  | EFun(_,Param(_,x1,s2),so3,e4) -> 
      let acc3 = match so3 with None -> acc | Some s3 -> free_evars_sort acc s3 in 
      let acc4 = Qid.Set.remove (Qid.t_of_id x1) (free_evars_exp acc3 e4) in 
      let acc2 = free_evars_sort acc4 s2 in 
      acc2
  | ELet(_,Bind(_,p1,so2,e3),e4) ->
      let acc2 = match so2 with None -> acc | Some s2 -> free_evars_sort acc s2 in 
      let acc3 = free_evars_exp acc2 e3 in 
      let acc3_minus_bvars = Qid.Set.diff acc3 (qvs_of_is (bound_evars_pat Id.Set.empty p1)) in
      let acc1 = free_evars_pat acc3_minus_bvars p1 in
      let acc4 = free_evars_exp acc1 e4 in
      acc4
  | ETyFun(_,_,e1) -> 
      free_evars_exp acc e1
  | ETyApp(i,e1,s2) -> 
      let acc1 = free_evars_exp acc e1 in 
      let acc2 = free_evars_sort acc1 s2 in 
      acc2
  | ECast(_,f1,t2,_,e3) ->
      let acc1 = free_evars_sort acc f1 in 
      let acc2 = free_evars_sort acc1 t2 in 
      let acc3 = free_evars_exp acc2 e3 in 
      acc3 
  | EPair(_,e1,e2) -> 
      let acc1 = free_evars_exp acc e1 in 
      let acc2 = free_evars_exp acc1 e2 in 
      acc2
  | ECase(i,e1,cl,s3) -> 
      let accl = 
      Safelist.fold_left 
        (fun accj (pi,ei) -> 
           let acci = free_evars_exp accj ei in            
           let acci_minus_bvars = Qid.Set.diff acci (qvs_of_is (bound_evars_pat Id.Set.empty pi)) in
           free_evars_pat acci_minus_bvars pi)
        acc cl in 
      let acc1 = free_evars_exp accl e1 in 
      let acc3 = free_evars_sort acc1 s3 in 
      acc3
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _ -> 
      acc

(* EXPRESSION SUBSTITUTION *)
let free_evars_in_subst subst = 
  Safelist.fold_left 
    (fun acc (x,e) -> free_evars_exp (Qid.Set.add x acc) e)
    Qid.Set.empty subst

let fresh_evar subst a = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a =     
    if Qid.Set.mem a clashes then aux (Qid.prime a)
    else a in 
  aux a 

let fresh_evar_id subst a = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a =     
    if Qid.Set.mem (Qid.t_of_id a) clashes then aux (Id.prime a)
    else a in 
  aux a 

let fresh_evar_ids subst xs = 
  let clashes = free_evars_in_subst subst in 
  let rec aux a = 
    if Qid.Set.mem (Qid.t_of_id a) clashes then aux (Id.prime a)
    else a in 
  Id.Set.fold (fun xi acc -> (xi,aux xi)::acc) xs [] 

let rec rename_evars_pat subst p0 = match p0 with 
  | PVar(i,x,so) ->  
      (try 
         let new_x = gen_assoc Id.equal x subst in 
         PVar(i,new_x,so) 
       with Not_found -> p0)
  | PVnt(i,qx,Some p) -> 
      let new_p = rename_evars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> 
      p0
  | PPar(i,p1,p2) -> 
      let new_p1 = rename_evars_pat subst p1 in  
      let new_p2 = rename_evars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> 
      p0

let rec subst_evars_pat subst p0 = match p0 with
  | PVar(i,x,Some s) ->       
      let new_s = subst_evars_sort subst s in 
      PVar(i,x,Some new_s)
  | PVar(i,x,None) -> 
      p0
  | PVnt(i,qx,Some p) -> 
      let new_p = subst_evars_pat subst p in       
      PVnt(i,qx,Some new_p)
  | PVnt(i,qx,None) -> p0
  | PPar(i,p1,p2) -> 
      let new_p1 = subst_evars_pat subst p1 in  
      let new_p2 = subst_evars_pat subst p2 in  
      PPar(i,new_p1,new_p2) 
  | PWld _ | PUnt _ | PBol _ | PInt _ | PStr _ -> 
      p0
 
(* msg "@[SUBST: {"; *)
(* Misc.format_list ",@ " (fun (x,e) -> msg "@[%s |-> %s@]" (Qid.string_of_t x) (string_of_exp e)) subst; *)
(* msg "} IN %s@]@\n@[X: %s < %b > FRESH_X: %s@]@\n" (string_of_sort s0) (Id.string_of_t x) (x = fresh_x) (Id.string_of_t fresh_x); *)

and subst_evars_sort subst s0 = match s0 with 
  | SFunction(x,s1,s2) ->      
      let new_s1 = subst_evars_sort subst s1 in       
      let fresh_x = fresh_evar_id subst x in 
      let safe_s2 = 
        if x = fresh_x then s2 
        else 
          let qx = Qid.t_of_id x in
          let fresh_qx = Qid.t_of_id fresh_x in 
          let subst_x = [(qx,EVar(Id.info_of_t fresh_x,fresh_qx))] in 
          subst_evars_sort subst_x s2 in       
      let new_s2 = subst_evars_sort subst safe_s2 in       
      SFunction(fresh_x,new_s1,new_s2)
  | SProduct(s1,s2) -> 
      let new_s1 = subst_evars_sort subst s1 in 
      let new_s2 = subst_evars_sort subst s2 in 
      SProduct(new_s1,new_s2)
  | SData(sl,qx) -> 
      let new_sl = Safelist.map (subst_evars_sort subst) sl in 
      SData(new_sl,qx)
  | SRefine(x,s1,e2) -> 
      let new_s1 = subst_evars_sort subst s1 in
      let fresh_x = fresh_evar_id subst x in 
      let safe_e2 = 
        if fresh_x = x then e2 
        else 
          let i = Id.info_of_t x in 
          let qx = Qid.t_of_id x in 
          let fresh_qx = Qid.t_of_id fresh_x in 
          let subst_x = [(qx,EVar(i,fresh_qx))] in 
          subst_evars_exp subst_x e2 in                       
      let new_e2 = subst_evars_exp subst safe_e2 in 
      SRefine(fresh_x,new_s1,new_e2)
  | SForall(a,s1) -> 
      let new_s1 = subst_evars_sort subst s1 in 
      SForall(a,new_s1)
  | SUnit | SBool | SInteger | SChar | SString | SRegexp | SLens | SCanonizer | SVar _ -> 
      s0
and subst_evars_exp subst e0 = match e0 with 
  | EVar(_,x) -> 
      (try gen_assoc Qid.equal x subst with Not_found -> e0)
  | EApp(i,e1,e2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_e2 = subst_evars_exp subst e2 in 
      EApp(i,new_e1,new_e2) 
  | EOver(i,o,el) -> 
      let new_el = Safelist.map (subst_evars_exp subst) el in 
      EOver(i,o,new_el)
  | EFun(i,Param(ip,x1,s2),so3,e4) -> 
      let new_s2 = subst_evars_sort subst s2 in 
      let fresh_x1 = fresh_evar_id subst x1 in 
      let safe_so3,safe_e4 = 
        if fresh_x1 = x1 then (so3,e4)
        else
          let i = Id.info_of_t x1 in 
          let qx1 = Qid.t_of_id x1 in 
          let fresh_qx1 = Qid.t_of_id fresh_x1 in 
          let subst_x1 = [(qx1,EVar(i,fresh_qx1))] in 
          (Misc.map_option (subst_evars_sort subst_x1) so3,
           subst_evars_exp subst_x1 e4) in 
      let new_so3 = Misc.map_option (subst_evars_sort subst) safe_so3 in 
      let new_e4 = subst_evars_exp subst safe_e4 in 
      EFun(i,Param(ip,fresh_x1,new_s2),new_so3,new_e4) 
  | ELet(i,Bind(ib,p1,so2,e3),e4) ->
      let p1_vars = bound_evars_pat Id.Set.empty p1 in 
      let p1_fresh_vars = fresh_evar_ids subst p1_vars in 
      let safe_p1,safe_so2,safe_e3,safe_e4 = 
        if Safelist.for_all (fun (xi,fresh_xi) -> xi = fresh_xi) p1_fresh_vars then 
          (p1,so2,e3,e4)
        else
          let subst_pvars = 
            Safelist.map 
              (fun (xi,fresh_xi) -> 
                 (Qid.t_of_id xi, EVar(i,Qid.t_of_id fresh_xi))) 
              p1_fresh_vars in
          (rename_evars_pat p1_fresh_vars p1,
           Misc.map_option (subst_evars_sort subst_pvars) so2,
           subst_evars_exp subst_pvars e3,
           subst_evars_exp subst_pvars e4) in 
      let new_p1 = subst_evars_pat subst safe_p1 in 
      let new_so2 = Misc.map_option (subst_evars_sort subst) safe_so2 in 
      let new_e3 = subst_evars_exp subst safe_e3 in 
      let new_e4 = subst_evars_exp subst safe_e4 in 
      ELet(i,Bind(ib,new_p1,new_so2,new_e3),new_e4)
  | ETyFun(i,a,e1) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      ETyFun(i,a,new_e1)
  | ETyApp(i,e1,s2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_s2 = subst_evars_sort subst s2 in 
      ETyApp(i,new_e1,new_s2) 
  | ECast(i,f1,t2,b,e3) ->
      let new_f1 = subst_evars_sort subst f1 in 
      let new_t2 = subst_evars_sort subst t2 in 
      let new_e3 = subst_evars_exp subst e3 in 
      ECast(i,new_f1,new_t2,b,new_e3)
  | EPair(i,e1,e2) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_e2 = subst_evars_exp subst e2 in 
      EPair(i,new_e1,new_e2)
  | ECase(i,e1,cl,s3) -> 
      let new_e1 = subst_evars_exp subst e1 in 
      let new_cl = 
        Safelist.map 
          (fun (pi,ei) -> 
             let pi_vars = bound_evars_pat Id.Set.empty pi in 
             let pi_fresh_vars = fresh_evar_ids subst pi_vars in 
             let safe_pi,safe_ei = 
               if Safelist.for_all (fun (xi,fresh_xi) -> xi = fresh_xi) pi_fresh_vars then 
               (pi,ei)
               else
                 let subst_pvars = 
                   Safelist.map 
                     (fun (xi,fresh_xi) -> 
                        (Qid.t_of_id xi, EVar(i,Qid.t_of_id fresh_xi))) 
                     pi_fresh_vars in
                 (rename_evars_pat pi_fresh_vars pi,
                  subst_evars_exp subst_pvars ei) in
             let new_pi = subst_evars_pat subst safe_pi in 
             let new_ei = subst_evars_exp subst safe_ei in 
             (new_pi,new_ei))
          cl in 
      let new_s3 = subst_evars_sort subst s3 in 
      ECase(i,new_e1,new_cl,new_s3)
  | EUnit _ | EBoolean _ | EInteger _ | EChar _ | EString _ | ECSet _  -> 
      e0

let subst_sort subst s0 = subst_svars_sort subst s0

let subst_exp_in_sort subst s0 = subst_evars_sort subst s0

let free_sort_vars s0 = free_svars_sort Id.Set.empty s0

let free_exp_vars_in_sort s0 = free_evars_sort Qid.Set.empty s0

let rec erase_sort = function
  | SFunction(x,s1,s2) ->       
      SFunction(x,erase_sort s1,erase_sort s2)
  | SProduct(s1,s2) -> 
      SProduct(erase_sort s1, erase_sort s2)
  | SData(sl,qx) ->
      SData(Safelist.map erase_sort sl,qx)
  | SRefine(x,s1,e1) -> 
      erase_sort s1
  | SForall(x,s1) -> 
      SForall(x,erase_sort s1)
  | SUnit | SBool | SInteger | SChar | SString 
  | SRegexp | SLens | SCanonizer | SVar _ as s0 -> 
      s0

let rec expose_sort = function
  | SRefine(x,s1,e1) -> expose_sort s1
  | s0 -> s0


let rec_eq eq = 
  let rec aux acc l1 l2 = 
  acc && (match l1,l2 with 
            | [],[] -> true
            | h1::t1,h2::t2 -> aux (eq h1 h2) t1 t2
            | _ -> false) in 
  aux 

let rec syneq_sort s1 s2 = match s1,s2 with
  | SVar a,SVar b -> Id.equal a b 
  | SFunction(x,s11,s12),SFunction(y,s21,s22) -> 
      Id.equal x y && syneq_sort s11 s21 && syneq_sort s21 s22 
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      syneq_sort s11 s21 && syneq_sort s21 s22
  | SData(sl1,qx), SData(sl2,qy) -> 
      rec_eq syneq_sort (Qid.equal qx qy) sl1 sl2 
  | SRefine(x,s1,e1),SRefine(y,s2,e2) -> 
      Id.equal x y && syneq_sort s1 s2 && syneq_exp e1 e2 
  | SForall(a,s1),SForall(b,s2) -> 
      Id.equal a b && syneq_sort s1 s2
  | SUnit,SUnit
  | SBool,SBool  
  | SInteger,SInteger 
  | SChar,SChar 
  | SString,SString 
  | SRegexp,SRegexp 
  | SLens,SLens 
  | SCanonizer,SCanonizer -> true
  | _ -> false
and syneq_pat p1 p2 = match p1,p2 with
  | PVar(_,x,Some s1),PVar(_,y,Some s2) -> 
      Id.equal x y && syneq_sort s1 s2
  | PVar(_,x,None),PVar(_,y,None) -> 
      Id.equal x y
  | PVnt(_,x,Some p1),PVnt(_,y,Some p2) -> 
      Qid.equal x y && syneq_pat p1 p2
  | PVnt(_,x,None),PVnt(_,y,None) -> 
      Qid.equal x y 
  | PPar(_,p11,p12),PPar(_,p21,p22) -> 
      syneq_pat p11 p21 && syneq_pat p12 p22
  | PWld _,PWld _   -> true
  | PUnt _,PUnt _   -> true
  | PBol(_,b1),PBol(_,b2) -> b1 = b2
  | PInt(_,n1),PInt(_,n2) -> n1=n2
  | PStr(_,s1),PStr(_,s2) -> s1=s2
  | _ -> false
and syneq_exp e1 e2 = match e1,e2 with
  | EVar(_,x),EVar(_,y) -> 
      Qid.equal x y 
  | EApp(_,e11,e12),EApp(_,e21,e22) -> 
      syneq_exp e11 e21 && syneq_exp e12 e22
  | EOver(_,o1,el1),EOver(_,o2,el2) -> 
      rec_eq syneq_exp (o1=o2) el1 el2  
  | EFun(_,Param(_,x,s1),so1,e1),EFun(_,Param(_,y,s2),so2,e2) -> 
         Id.equal x y 
      && syneq_sort s1 s2 
      && (match so1,so2 with 
            | None,None -> true 
            | Some s11,Some s12 -> syneq_sort s11 s12 
            | _ -> false)
      && syneq_exp e1 e2
  | ELet(_,Bind(_,p1,so1,e11),e12),ELet(_,Bind(_,p2,so2,e21),e22) ->
         syneq_pat p1 p2 
      && (match so1,so2 with 
            | None, None -> true
            | Some s11,Some s12 -> syneq_sort s11 s12
            | _ -> false) 
      && syneq_exp e11 e21
      && syneq_exp e12 e22
  | ETyFun(_,a,e1),ETyFun(_,b,e2) -> 
      Id.equal a b && syneq_exp e1 e2
  | ETyApp(i,e1,s1),ETyApp(_,e2,s2) -> 
      syneq_exp e1 e2 && syneq_sort s1 s2
  | ECast(_,f1,t1,_,e1),ECast(_,f2,t2,_,e2) ->
        syneq_sort f1 f2 
     && syneq_sort t1 t2
     && syneq_exp e1 e2
  | EPair(_,e11,e12),EPair(_,e21,e22) -> 
         syneq_exp e11 e21
      && syneq_exp e12 e22
  | ECase(_,e1,cl1,s1),ECase(_,e2,cl2,s2) -> 
      rec_eq (fun (pi,ei) (pj,ej) -> syneq_pat pi pj && syneq_exp ei ej)
        (syneq_exp e1 e2 && syneq_sort s1 s2) cl1 cl1
  | EUnit _,EUnit _         -> true
  | EBoolean(_,b1),EBoolean(_,b2) -> b1 = b2
  | EInteger(_,n1),EInteger(_,n2) -> n1 = n2
  | EChar(_,c1),EChar(_,c2)       -> c1 = c2
  | EString(_,s1),EString(_,s2)   -> s1 = s2
  | ECSet(_,b1,cl1),ECSet(_,b2,cl2) -> 
      rec_eq 
        (fun (s11,s12) (s21,s22) -> s11 = s12 && s21 = s22)
        (b1=b2) cl1 cl2
  | _ -> false

let fresh_counter = ref 0
let gensym i e = 
  let xs = free_evars_exp Qid.Set.empty e in 
  let rec aux () = 
    let n = 
      incr fresh_counter;
      !fresh_counter in 
    let s = Printf.sprintf "_%c%s" 
      (Char.chr ((n mod 26) + 96))
      (if n > 26 then Printf.sprintf "_%d" (n / 26) else "") in          
    let x = Id.mk i s in 
    if Qid.Set.mem (Qid.t_of_id x) xs then aux () 
    else x in
  aux ()

