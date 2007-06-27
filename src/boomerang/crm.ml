module RS = Rstring

module M = Map.Make (struct 
  type t = (RS.sym * RS.sym)
  let (<=) = RS.leq
  let (<) = RS.l
  let compare ((a,b):(RS.sym * RS.sym)) (c, d) : int =
    if ((a <= c) && (c <= b)) || ((a <= d) && ((d <= b) || (c <= a))) then
      0
    else if b < c then - 1 else 1
end)
open Char    

module Make(SS : 
  sig 
    include Set.S 
    val format_elt : elt -> unit
  end) = struct
  type t = SS.t M.t

  let find = M.find 
  let safe_find k m d  = try M.find k m with Not_found -> d
  let fold = M.fold
  let empty = M.empty
  let is_empty = M.is_empty

  let find_elt e m = find (e, e) m
  let safe_find_elt e m d = safe_find (e, e) m d

  let map = M.map
  let iter = M.iter

(*  let format_map fold format_key format_val map = 
    Format.printf "{@[";
    ignore 
      (fold (fun k v not_fst -> 
        if not_fst then Format.printf "@\n"; 
        format_key k;
        Format.printf "->";
        format_val v; 
        true) map false);
    Format.printf "@]}"
      
  let format_set fold format_sym set = 
    Format.printf "{@[";
    ignore 
      (fold (fun e not_fst -> 
        if not_fst then Format.printf ","; 
        format_sym e; 
        true) set false);
    Format.printf "@]}"
      
  let format t = format_map M.fold 
    (fun (a,b) -> Util.format "[%s-%s]" (repr a) (repr b))
    (format_set SS.fold SS.format_sym)
    t
*)

  
  let intersect keep (a,b) ns m f = 
    M.fold 
      (fun (c,d) s (rl, macc) -> 
	let _,rl,macc = Safelist.fold_left 
	  (fun (changed,rl,macc) (ci,di) -> 
	      if (di < c) || (d < ci) then
		let m1 = if not changed && keep then M.add (c, d) s macc else macc in
		  (changed,(ci,di)::rl, m1) 
	      else
		(match (RS.compare_sym c ci), (RS.compare_sym d di) with
		   | 0,0 ->
		       (true,rl,M.add (c,d) (f ns s) macc)
		   | 0,1 ->
		       let m1 = if keep then M.add (RS.succ di,d) s macc else macc in
		       let m1 = M.add (c,di) (f ns s) m1 in
			 (true,rl,m1)
		   | 0,-1 -> 
		       let m1 = M.add (c,d) (f ns s) macc in
			 (true,(RS.succ d,di)::rl, m1)
		   | 1,0 -> 
		       let m1 = M.add (c,d) (f ns s) macc in
			 (true,(ci,RS.pred c)::rl, m1)
		   | 1,1 -> 
		       let m1 = M.add (c,di) (f ns s) macc in
		       let m2 = if keep then M.add (RS.succ di,d) s m1 else m1 in
			 (true,(ci,RS.pred c)::rl, m2)
		   | 1,-1 -> 
		        let m1 = M.add (c,d) (f ns s) macc in
                  	  (true,(ci,RS.pred c)::(RS.succ d,di)::rl, m1)
		   | -1,0 -> 
		       let m1 = M.add (ci,di) (f ns s) macc in
		       let m2 = if keep then M.add (c,RS.pred ci) s m1 else m1 in
			 (true,rl,m2)
		   | -1,1 -> 
		       let m1 = M.add (ci,di) (f ns s) macc in
		       let m2 = if keep then M.add (c,RS.pred ci) s m1 else m1 in
		       let m3 = if keep then M.add (RS.succ di,d) s m2 else m2 in
			 (true,rl,m3)
		   | -1,-1 -> 
		       let m1 = M.add (ci,d) (f ns s) macc in
		       let m2 = if keep then M.add (c,RS.pred ci) s m1 else m1 in
		       	 (true,(RS.succ d,di)::rl,m2)
		   | _,_ -> assert false))
	   (false,[],macc) rl in 
          rl,macc)
      m ([(a,b)],if keep then m else M.empty)

      
  let add (a,b) ns m =
    let rl,m' = intersect true (a,b) ns m SS.union in
      Safelist.fold_left (fun macc (ci,di) -> M.add (ci,di) ns macc) m' rl
	
  let fill_holes ns m = 
    let rl,m' = intersect true (RS.char_code_min, RS.char_code_max) ns m (fun _ y -> y) in
      Safelist.fold_left (fun macc (ci,di) -> M.add (ci,di) ns macc) m' rl

  let rem (a,b) m = 
    snd (intersect true (a,b) SS.empty m (fun _ _ -> SS.empty))

  let add_elt i = add (i, i)

  let rem_elt i = rem (i, i)

  let union m1 m2 = 
    M.fold 
      (fun r1 s1 m -> add r1 s1 m)
      m1 m2
    
  let product f m1 m2 =
    M.fold 
      (fun (a,b) ss acc -> 
	 let _,m' = intersect false (a,b) ss m2 f
	 in union m' acc
      ) 
      m1 M.empty

  let isect_range (a,b) (c,d) = 
    let e = max a c in 
    let f = min b d in 
      if e > f then None
      else Some(e,f)

  let single_trans i q = 
    M.add (i, i) (SS.singleton q) M.empty

end
