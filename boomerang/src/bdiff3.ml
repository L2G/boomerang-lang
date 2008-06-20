module type DIFF3ARGS = sig 
  type elt
  val eqv : elt -> elt -> bool
  val format : elt -> unit
end

module type DIFF3RES = sig
  type elt
  type seq = elt list
  
  type chunk = 
    | Stable of elt * elt * elt
    | AChange of seq * seq * seq
    | BChange of seq * seq * seq
    | Conflict of seq * seq * seq
  val parse : seq -> seq -> seq -> chunk list
end

(*******************************************************************)

let debug = Trace.debug "bdiff3"

module Make(A: DIFF3ARGS) = struct

type elt = A.elt

type seq = elt list
  
type chunk = 
  | Stable of elt * elt * elt
  | AChange of seq * seq * seq
  | BChange of seq * seq * seq
  | Conflict of seq * seq * seq
             
type lcs_type = Top | Diag | Left 

let parse o a b =
  let len_a = Safelist.length a in 
  let len_b = Safelist.length b in
  let len_o = Safelist.length o in
  let arr_a = Array.of_list a in
  let arr_b = Array.of_list b in
  let arr_o = Array.of_list o in 
  let make_comp arr_1 arr_2 = 
    (* set comp_oa [i,j] to true iff arr_o[i]=arr_o[j]... *)
    let comp = Array.make_matrix (Array.length arr_1) (Array.length arr_2) true in 
      Array.iteri 
        (fun i x -> 
           comp.(i) <- 
             ( Array.map (fun elt_a -> A.eqv elt_a x ) arr_2 ))
        arr_1;
      comp in
  let comp_oa = make_comp arr_o arr_a in
  let comp_ob = make_comp arr_o arr_b in
  let make_match_list comp l_o l_a =  
    let lcs_oa = Array.make_matrix (l_o+1) (l_a+1) 0 in 
    let lcs_aux = Array.make_matrix (l_o+1) (l_a+1) Diag in 
      Array.iteri 
        (fun i x -> 
           if (i=0) then ()  
           else Array.iteri 
             (fun j y ->
                if (j=0) then () 
                else if comp.(i-1).(j-1) then 
                  let _ = lcs_aux.(i).(j) <- Diag in 
                    lcs_oa.(i).(j) <- lcs_oa.(i-1).(j-1) + 1 
                else if (lcs_oa.(i-1).(j) > lcs_oa.(i).(j-1)) then  
                  let _ = lcs_aux.(i).(j) <- Top in
                    lcs_oa.(i).(j) <- lcs_oa.(i-1).(j) 
                else
                  let _ = lcs_aux.(i).(j) <- Left in
                    lcs_oa.(i).(j) <- lcs_oa.(i).(j-1))
             lcs_oa.(i)) 
        lcs_oa;
      let rec find_matches i j l = 
        if (i=0) or (j=0) then l
        else match lcs_aux.(i).(j) with  
            Diag -> find_matches (i-1) (j-1)  ((i-1,j-1) :: l)  
          | Top -> find_matches  (i-1) j  l    
          | _ ->  find_matches  i (j-1)  l in
        find_matches l_o l_a  [] in
  let same_lines_oa = make_match_list comp_oa len_o len_a in 
  let same_lines_ob = make_match_list comp_ob len_o len_b in
  let rec common_lines lines_oa lines_ob l =
    match lines_oa with 
        [] -> l
      | (lo,la)::tl -> 
          match lines_ob with 
              [] -> l 
            | (lo',lb)::tl' ->
                if lo = lo' then common_lines tl tl' ((lo,la,lb)::l)
                else if lo > lo' then common_lines lines_oa tl'  l 
                else common_lines tl lines_ob  l in
  let same_lines_oab =
    Safelist.append (common_lines same_lines_oa same_lines_ob []) [(-1,-1,-1)] in 

  (************************************************ *)  
  let _,chunks = 
    Safelist.fold_left
      (fun ((eo,ea,eb),chunks) (so,sa,sb) ->         
         let rec find_diff io ia jo ja comp = 
           if (io=jo) then
             (ia <> ja) (* file o has no more lines *)
           else if (ia=ja) then  
             true (* file a has no more lines *)
           else 
             (not comp.(io).(ia)) || (find_diff (io+1) (ia+1) jo ja comp) in
         let is_same_oa = not (find_diff (so+1) (sa+1) eo ea comp_oa) in
         let is_same_ob = not (find_diff (so+1) (sb+1) eo eb comp_ob) in 

         let get_lines sl el arr =
           let len = el - sl in 
           if (len > 0) then 
             (* BCP: Next line is hideous... *)
             Array.to_list (Array.init len (fun i -> arr.(sl+i)))
           else [] in
           
         let onew = get_lines (so+1) eo arr_o in 
         let anew = get_lines (sa+1) ea arr_a in 
         let bnew = get_lines (sb+1) eb arr_b in  

         (* so and eo are the matching line numbers - so the differing lines are
            so+1, so+2 ... eo-1.  When finally adding the lines to reconciled
            version, we need to add the common line too *)

         let common =
           if (sb = -1) then []
           else
             let li = arr_b.(sb) in
             [Stable(li,li,li)] in

         if is_same_oa && is_same_ob then
           (* a and b are equal to o, so just output a stable chunk. *)              
           (assert(onew = [] && anew = [] && bnew = []);
           ((so,sa,sb),common@chunks))
         else if is_same_oa then
           (* a is equal to o (and b is not). *)
           ((so,sa,sb),common@(BChange(onew,anew,bnew)::chunks))
         else if is_same_ob then 
           (* b is equal to o (and a is not). *)
           ((so,sa,sb),common@(AChange(onew,anew,bnew)::chunks))
         else
           (* neither a nor b equal to o (they are possibly the same) *)
           ((so,sa,sb),common@(Conflict(onew,anew,bnew)::chunks)))
      ((len_o,len_a,len_b),[]) same_lines_oab in
    chunks

end (* functor Make *)


