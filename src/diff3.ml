module type DIFF3ARGS = sig
  type elt
  val eqv : elt -> elt -> bool
  val tostring : elt -> string
  val format : elt -> unit
end

let debug = Trace.debug "diff3"

module Make(A: DIFF3ARGS) = struct

open Printf

let print_elt e = eprintf "%s" (A.tostring e)

type lcs_type = Top | Diag | Left 

let print_list l =
  eprintf "[";
  let rec loop l = match l with
    [] -> ()
  | [e] -> eprintf "%s" (A.tostring e)
  | e::es -> eprintf "%s" (A.tostring e); eprintf ", "; loop es
  in loop l;
  eprintf "]"

let format_list l =
  Format.printf "[";
  let rec loop l = match l with
    [] -> ()
  | [e] -> A.format e
  | e::es -> A.format e; Format.printf ",@ "; loop es
  in loop l;
  Format.printf "]"

let sync (o,a,b) =
  debug (fun () ->
    eprintf "Inputs:\n";
    eprintf "          o = "; print_list o; eprintf "\n"; 
    eprintf "          a = "; print_list a; eprintf "\n"; 
    eprintf "          b = "; print_list b; eprintf "\n"); 
  let len_a = List.length a in 
  let len_b = List.length b in
  let len_o = List.length o in
  let arr_a = Array.of_list a in
  let arr_b = Array.of_list b in
  let arr_o = Array.of_list o in 
  (* initialize comp_oa [i,j] = true iff arr_o[i]=arr_o[j]: *)
  let make_comp arr_1 arr_2 = 
    let comp = Array.make_matrix (Array.length arr_1) (Array.length arr_2) true in 
    Array.iteri 
      (fun i x -> 
         comp.(i) <- 
           ( Array.map (fun elt_a -> A.eqv elt_a x ) arr_2 ) 
      ) arr_1;
    comp in
  let comp_oa = make_comp arr_o arr_a in
  let comp_ob = make_comp arr_o arr_b in
  (* let comp_ab = make_comp arr_a arr_b in  *)
  let make_match_list comp l_o l_a =  
    let lcs_oa = Array.make_matrix (l_o+1) (l_a+1) 0 in 
    let lcs_aux = Array.make_matrix (l_o+1) (l_a+1) Diag in 
    let _ = Array.iteri 
      (fun i x -> 
         if (i=0) 
         then ()  
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
           lcs_oa.(i))  (* end of fun i *) 
      lcs_oa in
    let rec find_matches i j l = 
      if (i=0) or (j=0) then l
      else match lcs_aux.(i).(j) with  
              Diag -> find_matches (i-1) (j-1)  ((i-1,j-1) :: l)  
            | Top -> find_matches  (i-1) j  l    
            | _ ->  find_matches  i (j-1)  l
    in
      find_matches l_o l_a  []  
  in
  let same_lines_oa = make_match_list comp_oa len_o len_a in 
  let same_lines_ob = make_match_list comp_ob len_o len_b in
  let rec common_lines lines_oa lines_ob l = match lines_oa with 
      [] -> l
    | (lo,la)::tl -> 
        match lines_ob with 
          [] -> l 
        | (lo',lb)::tl' ->
            if lo = lo' then common_lines tl tl' ((lo,la,lb)::l)
            else if lo > lo' then common_lines lines_oa tl'  l 
            else common_lines tl lines_ob  l in
  let same_lines_oab =
    List.append 
      (common_lines same_lines_oa same_lines_ob [])
      [(-1,-1,-1)] in 
  let (x, (o',a',b')) =
    List.fold_left    
    (fun ((eo,ea,eb),(o',a',b')) (so,sa,sb) ->
       (* let _ = eprintf "S %d,%d,%d " so sa sb in 
          let _ = eprintf "E %d,%d,%d " eo ea eb in *)
       let rec find_diff io ia jo ja comp = 
         if (io=jo) then
           not (ia==ja) (* file o has no more lines *)
         else if (ia=ja) then  
           true (* file a has no more lines *)
         else 
           (not comp.(io).(ia))  ||  (find_diff (io+1) (ia+1) jo ja comp) in
       let is_diff_oa = find_diff (so+1) (sa+1) eo ea comp_oa in 
       (* let _ = eprintf  "\nOA %B" is_diff_oa in  *)
       let is_diff_ob = find_diff (so+1) (sb+1) eo eb comp_ob in 
       (* let _ = eprintf  "OB %B" is_diff_ob in  *)
       (* let is_diff_ab = find_diff (sa+1) (sb+1) ea eb comp_ab in *)
       (* let _ = eprintf  "AB %B" is_diff_ab in *)
       let add_lines sl el arr dest =
         (* let _ = eprintf "%d %d %d \n" sl el (Array.length arr) in *)
	 let isfirst = (sl = -1) in 
	 let len = if isfirst then el-sl-1 else el-sl in  
	 let start = if isfirst then 0 else sl in  
	   if (len > 0) then 
	     (
               let tmp_arr = Array.init len (fun i -> arr.(start+i) ) in
		 List.append (Array.to_list tmp_arr) dest 
	     )
           else 
	     dest 
       in
	 if is_diff_oa then
         if is_diff_ob 
         then begin  (* conflict case *)
           let o'= add_lines so eo arr_o o'  in 
           let a'= add_lines sa ea arr_a a'  in 
           let b'= add_lines sb eb arr_b b'  in  
           debug (fun () -> 
             eprintf "The chunk consisting of\n          %d lines from %d - %d in file O,\n          %d lines from %d - %d in file A,\n          %d lines from %d - %d in file B\n          is in conflict\n"  (eo-so-1) (so+2) eo   (ea-sa-1)  (sa+2) (ea) (eb - sb -1) (sb+2) (eb));
           ((so,sa,sb),(o',a',b'))
         end else begin   (* Only a is different *) 
           let o'= add_lines sa ea arr_a o'  in 
           let a'= add_lines sa ea arr_a a'  in 
           let b'= add_lines sa ea arr_a b'  in
           debug (fun () ->
             eprintf "Only a is different in the chunk consisting of\n          %d lines from %d - %d in file O,\n          %d lines from %d - %d in file A,\n          %d lines from %d - %d in file B\n"  (eo-so-1) (so+2) eo   (ea-sa-1)  (sa+2) (ea) (eb - sb -1) (sb+2) (eb));
           ((so,sa,sb),(o',a',b'))        
         end else begin (* Only b is different or else all same*) 
           let o'= add_lines sb eb arr_b o'  in 
           let a'= add_lines sb eb arr_b a'  in 
           let b'= add_lines sb eb arr_b b'  in  
           debug (fun () -> 
             if is_diff_ob  then eprintf "Only b is different in the chunk consisting of\n          %d lines from %d - %d in file O,\n          %d lines from %d - %d in file A,\n           %d lines from %d - %d in file B"  (eo-so-1) (so+2) eo   (ea-sa-1)  (sa+2) (ea) (eb - sb -1) (sb+2) (eb)    
             else eprintf "All three lists are identical\n");
         ((so,sa,sb),(o',a',b'))      
         end 
    )
    ((len_o,len_a,len_b),([],[],[])) same_lines_oab 
  in
    (o',a',b')

end (* functor Make *)

