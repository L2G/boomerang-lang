module type DIFF3ARGS = sig
  type elt
  type action
  val format_action: action -> unit
  val has_conflict: action -> bool
  val eqv : elt -> elt -> bool
  val tostring : elt -> string
  val format : elt -> unit
  val sync: Schema.t -> (elt option * elt option * elt option)
         -> (action * elt option * elt option * elt option)
end

module type DIFF3RES = sig
  type elt 
  type action
  val format_action: action -> unit
  val has_conflict: action -> bool
  val sync : Schema.t
          -> (elt list * elt list * elt list)
          -> (action * elt list * elt list * elt list)
end

(***********************************************************************)

let debug = Trace.debug "diff3"

module Make(A: DIFF3ARGS) = struct

type elt = A.elt

type action = ((unit->unit) option * (unit->bool)) list 

let format_action acts =
  Format.printf "@[<hv 1>[";
  let rec loop first_elt skipping = function
      [] -> ()
    | (None, _) :: rest ->
        if not skipping then begin
          if not first_elt then Format.printf ",@,";
          Format.printf "..."
        end;
        loop false true rest
    | (Some f, _) :: rest ->
        if not first_elt then Format.printf ",@,";
        f();
        loop false false rest
  in loop true false acts;
  Format.printf "]@]"

let has_conflict acts = Safelist.exists (fun (_,hc) -> hc()) acts

let confl () = true
let noconfl () = false

(*************************************)

type lcs_type = Top | Diag | Left 

let rec zip3 = function
    (x::xs,y::ys,z::zs) -> (x,y,z)::(zip3 (xs,ys,zs))
  | ([],[],[]) -> []
  | _ -> assert false

let rec unzip3 = function
    (x,y,z)::l ->
      let (xs,ys,zs) = unzip3 l in
      (x::xs,y::ys,z::zs)
  | [] -> ([],[],[])

let rec unzip4 = function
    (x,y,z,w)::l ->
      let (xs,ys,zs,ws) = unzip4 l in
      (x::xs,y::ys,z::zs,w::ws)
  | [] -> ([],[],[],[])

let the = function None -> assert false | Some x -> x

let print_list l =
  Printf.eprintf "[";
  let rec loop l = match l with
    [] -> ()
  | [e] -> Printf.eprintf "%s" (A.tostring e)
  | e::es -> Printf.eprintf "%s" (A.tostring e); Printf.eprintf ", "; loop es
  in loop l;
  Printf.eprintf "]"

let format_list l =
  let rec loop l = match l with
    [] -> ()
  | [e] -> A.format e
  | e::es -> A.format e; Format.printf ",@ "; loop es
  in loop l

let diff3_sync elt_schema (o,a,b) =
  debug (fun () ->
	   Printf.eprintf "Inputs:\n";
	   Printf.eprintf "          o = "; print_list o; Printf.eprintf "\n"; 
	   Printf.eprintf "          a = "; print_list a; Printf.eprintf "\n"; 
	   Printf.eprintf "          b = "; print_list b; Printf.eprintf "\n"); 
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
  let (x, (acts,o',a',b')) =
    Safelist.fold_left    
      (fun ((eo,ea,eb),(acts,o',a',b')) (so,sa,sb) ->
         let rec find_diff io ia jo ja comp = 
           if (io=jo) then
             not (ia==ja) (* file o has no more lines *)
           else if (ia=ja) then  
             true (* file a has no more lines *)
           else 
             (not comp.(io).(ia))  ||  (find_diff (io+1) (ia+1) jo ja comp) in
         let is_diff_oa = find_diff (so+1) (sa+1) eo ea comp_oa in 
         let is_diff_ob = find_diff (so+1) (sb+1) eo eb comp_ob in 

	 let get_lines sl el arr =
           let len = el - sl in 
           if (len > 0) then 
             (* BCP: Next line is hideous... *)
             Array.to_list (Array.init len (fun i -> arr.(sl+i)))
           else 
             [] in

         let header s =
           Format.printf
             "-- Elements %d-%d in archive, %d-%d in replica 1, %d-%d in replica 2: --@,"
             (so+2) eo (sa+2) (ea) (sb+2) (eb) in
           (* let header s =
              Format.printf "@[<v 0>%s in the chunk consisting of" s;
              Format.printf "@    %d lines from %d-%d in archive,"
              (eo-so-1) (so+2) eo;
              Format.printf "@    %d lines from %d-%d in replica 1,"
              (ea-sa-1) (sa+2) (ea);
              Format.printf "@    %d lines from %d-%d in replica 2...@]@,"
              (eb-sb-1) (sb+2) (eb) in *)

         let showchange s nw ol =
             if nw = [] then begin
               Format.printf "Delete: @["; format_list ol; Format.printf "@]"
             end else if ol=[] then begin
               Format.printf "Add: @["; format_list nw; Format.printf "@]"
             end else begin
               Format.printf "Change: @["; format_list ol; Format.printf "@]@,";
               Format.printf "    to: @["; format_list nw; Format.printf "@]";
             end in

         (* let showchange s nw ol =
           let rec list_change_lines m = function
               [] -> ()
             | [e]     -> Format.printf "%s (%s) " m s; A.format e
             | e::rest -> Format.printf "%s (%s) " m s; A.format e;
                 Format.printf ",@,"; list_change_lines m rest in
             if nw = [] then list_change_lines "Delete" ol
             else if ol=[] then list_change_lines "Add" nw 
             else list_change_lines "Change" nw in *)

         let onew = get_lines (so+1) eo arr_o in 
         let anew = get_lines (sa+1) ea arr_a in 
         let bnew = get_lines (sb+1) eb arr_b in  
           
	 (* so, eo are the matching line numbers - so the differing lines are
	    so+1, so+2 ... eo-1.  When finally adding the lines to reconciled
            version, we need to add the common line too *)
         let common = if (sb = -1) then [] else get_lines sb (sb+1) arr_b in 
           if is_diff_oa && is_diff_ob then begin
             let len_onew = Safelist.length onew in 
             let len_anew = Safelist.length anew in 
             let len_bnew = Safelist.length bnew in 
               debug (fun() -> Printf.eprintf "o="; print_list onew;
                        Printf.eprintf "\na="; print_list anew;
                        Printf.eprintf "\nb="; print_list bnew;
                        Printf.eprintf "\n");
               if len_onew = len_anew && len_onew = len_bnew then begin
		 (* Recursively synchronize, element by element *)
		 let (subacts, onew', anew', bnew') =
		   unzip4
                     (Safelist.map
			(fun (o,a,b) ->
			   let (a, oo', oa', ob') =
                             A.sync elt_schema (Some o,Some a,Some b) in
			     (a, the oo', the oa', the ob'))
			(zip3 (onew, anew, bnew))) in
		 let act () =
		   header "Reconciling changes line by line";
		   (* Format.printf "   (Length = %d)@," len_onew; *)
		   let rec loop first = function
                       [] -> ()
                     | a::rest ->
			 if not first then Format.printf ",@,";
			 A.format_action a;
			 loop false rest in
		     loop true subacts in
		 let hc () = Safelist.exists A.has_conflict subacts in
		   ((so,sa,sb),((Some act,hc)::acts,common@onew'@o',common@anew'@a',common@bnew'@b'))
               end else begin
		 let act () =
		   header "Conflict";
		   Format.printf "@[<hv 4>Conflict between@ "; format_list anew;
		   Format.printf "@]@ @[<hv 4>and@ "; format_list bnew; Format.printf "@]" in
		   ((so,sa,sb),((Some act,confl)::acts,common@onew@o',common@anew@a',common@bnew@b'))
               end
           end else if is_diff_oa then begin   
             let act () = header "Replica 1 changed"; showchange "-->" anew bnew in
               ((so,sa,sb),((Some act,noconfl)::acts,common@anew@o',common@anew@a',common@anew@b'))        
           end else begin
             (* Only b is different or else all three are the same *) 
             let act = 
               if is_diff_ob then
		 Some (fun()-> header "Replica 2 changed"; showchange "<--" bnew anew)
               else None in
               ((so,sa,sb),((act,noconfl)::acts,common@bnew@o',common@bnew@a',common@bnew@b'))      
           end)
      ((len_o,len_a,len_b),([],[],[],[]))
      same_lines_oab 
  in
    (acts,o',a',b')

(* ---------------------------------------------------------------------- *)

exception Not_same_length

exception Element_not_found


let rec getmin l min = match l with
    h::t -> if (h < min) then getmin t h else getmin t min
  | [] -> min 

let rec getmax l max = match l with
    h::t -> if (h > max) then getmax t h else getmax t max
  | [] -> max 

 
exception Duplicate_element 

let print_block pos elt1 elt2 len = 
if len=1 then Printf.printf "%s at position %d" (A.tostring elt1) pos 
else
  Printf.printf "[%s..%s] at postion [%d..%d]   " (A.tostring elt1) (A.tostring elt2) pos (pos+len-1)  


let cycle_merge_sync elt_schema (archive, a1, a2) =
  if (List.length archive !=  List.length a1) || (List.length archive !=  List.length a2) 
  then raise Not_same_length 
  else
    let list_size = List.length archive in 
    let get_index x l =
      (* get index of x in list l *) 
      let rec f id l =
	match l with
	    [] -> raise Element_not_found
	  | h::t -> if h=x then id else f (id+1) t 
      in
	f 1 l
    in 
      (* Index_tbl holds corresponding index in a1 and a2 *)
    let final_index_tbl = Hashtbl.create (2* (List.length archive)) in 
    let orig_contents_tbl = Hashtbl.create (2* (list_size)) in 
    let index_tbl_a1 = Hashtbl.create (2* (list_size)) in
    let rev_index_tbl_a1 = Hashtbl.create (2* (list_size)) in
    let index_tbl_a2 = Hashtbl.create (2*(list_size)) in
    let rev_index_tbl_a2 = Hashtbl.create (2* (list_size)) in
    let _ =  List.fold_left 
      (fun id x ->  
	 let _ = Hashtbl.add final_index_tbl  id id in 
	 let _ = Hashtbl.add orig_contents_tbl  id x in 
	 let y = get_index x a1 in 
	 let _ = Hashtbl.add index_tbl_a1  id y in 
	 let _ = if (Hashtbl.mem rev_index_tbl_a1 y) then  raise Duplicate_element else 
	   Hashtbl.add rev_index_tbl_a1  y id in
	 let y = get_index x a2 in 
	 let _ = Hashtbl.add index_tbl_a2  id y in
	 let _ = if (Hashtbl.mem rev_index_tbl_a2 y) then  raise Duplicate_element else
	   Hashtbl.add rev_index_tbl_a2  y id in	   
	   id+1)
      1 archive in
(* If an element is duplicated in A but not in O, Element_not_found will be raised - so no need to check for duplicates in A (or B) *)
    let rec getcycle table h t l =  
      let newt = Hashtbl.find table t in 
	if newt=h then  l else getcycle table h newt (newt::l) 
    in 
    let rec is_block table h t off result = 
      if t<h then result else result && (is_block table h (t-1) off (((Hashtbl.find table t) - t) = off)) 
    in
    let rec is_cycle cycle head tbl = match cycle with
		      h::t -> if ((Hashtbl.find tbl h) = head) 
		      then  is_cycle t h tbl 
		      else false 
		    | [] -> true 
    in
      (* find cycles with source in Archive 1 *)
    let rec findcycle_a1 id = 
      let check_cycle pos_start pos_a1 = 
	let pos_end = Hashtbl.find rev_index_tbl_a1 pos_start in
	  if (pos_end <= pos_start) || (pos_a1  <= pos_start) then false
	  else 
	    let cycle = getcycle index_tbl_a1 pos_start pos_a1 [pos_a1;pos_start] in 
	    let min = getmin cycle pos_end in 
	    let max = getmax cycle pos_start in
	    let offset = (Hashtbl.find index_tbl_a2 pos_start) - pos_start in 
	      if (is_block index_tbl_a2 min max  offset true) then  
		begin
		  let rec get_block_len cycle len min max = 
		    if (max > list_size) then len else 
		      let cycle = List.map (fun x -> x+1) cycle in 
			if not (is_cycle cycle (pos_start+len) index_tbl_a1) then  len 
			else 
			if (is_block index_tbl_a2 min max  offset true) then  
			  get_block_len cycle (len+1) (min+1) (max+1)
			else len 
		  in
		  let block_len = get_block_len cycle 1 (min+1) (max+1)  in 
		  let str = if block_len=1 then "elements" else "blocks" in 
		  let _ = Printf.printf "Active cycle with source in replica 1. The cycle consists of following %s in cyclic order \n" str in 
		  let _ = List.iter
		    (fun x -> 
		       let y = Hashtbl.find rev_index_tbl_a1 x in 
		       let elt1 =   Hashtbl.find orig_contents_tbl y in
		       let elt2 =   Hashtbl.find orig_contents_tbl (y+block_len-1) in
                         print_block y elt1 elt2 block_len 
		    )
		    cycle in
		  let _ = Printf.printf"\n" in 
		  let tmp_tbl = Hashtbl.create (2* (list_size)) in
		  let _ = List.iter 
		    (fun x -> Hashtbl.add tmp_tbl x (Hashtbl.find final_index_tbl x)) cycle 
		  in
		  let rec apply_cycle cycle head = match cycle with
		      h::t -> 
			begin
			  let mod_index_h = Hashtbl.find tmp_tbl h in
			  let rec change_tables len = 
			    if (len > -1) then  
			      begin
				let _ = Hashtbl.replace rev_index_tbl_a1 (h+len) (h+len) in
				let _ = Hashtbl.replace index_tbl_a1 (h+len) (h+len) in
				let _ = Hashtbl.replace final_index_tbl (head+len) (mod_index_h+len) in
				  change_tables (len-1) 
			      end
			    else
			      () 
			  in
			  let _ = change_tables (block_len-1) in  
			    apply_cycle t h  
			end
		    | [] -> () 
		  in 

		  let _ = apply_cycle cycle pos_start in 
		    (* let _ = Hashtbl.iter iter_print final_index_tbl in *)
		    true 
		end
	      else false 
      in
	if (id < (list_size)) then 
          if (check_cycle id (Hashtbl.find  index_tbl_a1 id) ) 
	  then true else findcycle_a1 (id+1) 
	else false 
    in
 
    let rec findcycle_a2 id = 
     let check_cycle pos_start pos_a2 = 
	let pos_end = Hashtbl.find rev_index_tbl_a2 pos_start in
	  if (pos_end <= pos_start) || (pos_a2  <= pos_start) then false
	  else 
	    let cycle = getcycle index_tbl_a2 pos_start pos_a2 [pos_a2;pos_start] in 
	    let min = getmin cycle pos_end in 
	    let max = getmax cycle pos_start in
	    let offset = (Hashtbl.find index_tbl_a1 pos_start) - pos_start in 
	      if (is_block index_tbl_a1 min max  offset true) then  
		begin
		  let rec get_block_len cycle len min max = 
		    if (max > list_size) then len else 
		      let cycle = List.map (fun x -> x+1) cycle in 
			if not (is_cycle cycle (pos_start+len) index_tbl_a2) then  len 
			else 
			  if (is_block index_tbl_a1 min max  offset true) then  
			    get_block_len cycle (len+1) (min+1) (max+1)
			  else len 
		  in
		  let block_len = get_block_len cycle 1 (min+1) (max+1)  in 
		  let str = if block_len=1 then "elements" else "blocks" in 
		  let _ = Printf.printf "Active cycle with source in replica 2. The cycle consists of following %s in cyclic order \n" str in 
		  let _ = List.iter
		    (fun x -> 
		       let y = Hashtbl.find rev_index_tbl_a2 x in 
		       let elt1 =   Hashtbl.find orig_contents_tbl y in
		       let elt2 =   Hashtbl.find orig_contents_tbl (y+block_len-1) in
                         print_block y elt1 elt2 block_len 
		    )
		    cycle in
		  let _ = Printf.printf"\n" in 
		  let tmp_tbl = Hashtbl.create (2* (list_size)) in
		  let _ = List.iter 
		    (fun x -> Hashtbl.add tmp_tbl x (Hashtbl.find final_index_tbl x)) cycle 
		  in
		  let rec apply_cycle cycle head = match cycle with
		      h::t -> 
			begin
			  let mod_index_h = Hashtbl.find tmp_tbl h in
			  let rec change_tables len = 
			    if (len > -1) then  
			      begin
				let _ = Hashtbl.replace rev_index_tbl_a2 (h+len) (h+len) in
				let _ = Hashtbl.replace index_tbl_a2 (h+len) (h+len) in
				let _ = Hashtbl.replace final_index_tbl (head+len) (mod_index_h+len) in
				  change_tables (len-1) 
			      end
			    else
			      () 
			  in
			  let _ = change_tables (block_len-1) in  
			    apply_cycle t h  
			end
		    | [] -> () 
		  in 

		  let _ = apply_cycle cycle pos_start in 
		    (* let _ = Hashtbl.iter iter_print final_index_tbl in *)
		    true 
		end
	      else false 
      in
	if (id < (list_size)) then 
          if (check_cycle id (Hashtbl.find  index_tbl_a2 id) ) 
	  then true else findcycle_a2 (id+1) 
	else false 
    in 

    let rec findcycles l =
      if ( (findcycle_a1 1) || (findcycle_a2 1)) then findcycles l else () in  
    let _ = findcycles archive in 
    let a' = Hashtbl.fold (fun x y  l -> 
			     List.append l 
			       [
				 Hashtbl.find orig_contents_tbl 
				   (Hashtbl.find final_index_tbl y) 
			       ] 
			  )
      rev_index_tbl_a1 [] in 
    let b' = Hashtbl.fold (fun x y  l -> 
			     List.append l 
			       [
				 Hashtbl.find orig_contents_tbl 
				   (Hashtbl.find final_index_tbl y) 
			       ] 
			  )
      rev_index_tbl_a2 [] in  
      (* add a check if a'=b' in which case set o'=a' *)
    let o' = Hashtbl.fold (fun x y  l -> List.append l  [Hashtbl.find orig_contents_tbl y] ) final_index_tbl [] in 
    let o' = if (not (o' =a')) &  (a'=b') then 
	let _ = Printf.printf "Setting o'=a' since a'=b' "  in
	a'  
    else o'  
    in 
   let rec print_conflicts pos tbl = 
      if pos > list_size then () 
      else
	let pos_rep = Hashtbl.find tbl pos in 
	  if  (pos_rep = pos) then print_conflicts (pos +1) tbl 
	  else
	    (
	      let rec get_block_len pos len offset = 
		if (pos > list_size) then len 
		else
		  ( 
		    let pos_rep = Hashtbl.find tbl pos in 
		      if ((pos_rep - pos) = offset) then 
			get_block_len (pos+1) (len+1) offset 
		      else len  
		  )
	      in
	      let block_len = get_block_len (pos +1) 1 (pos_rep - pos) in
	      let elt1 =   Hashtbl.find orig_contents_tbl pos in
	      let elt2 =  Hashtbl.find orig_contents_tbl (pos + block_len-1) in 
	      let _ = Printf.printf "\n" in 
              let _ =   print_block pos elt1 elt2 block_len in  
	      let _ = Printf.printf " moved to position " in
	      let _ = if block_len =1 
	      then 
		Printf.printf "%d " pos_rep 
	      else
		Printf.printf "[%d..%d]" pos_rep (pos_rep + block_len -1) 
	      in
		print_conflicts (pos+block_len) tbl 
	    )
   in
   let _ = if not (o' = a') then 
     let _ = Printf.printf "\n The following changes from o' to a' can not be reconciled \n" in 
       print_conflicts 1 index_tbl_a1 
   in
   let _ = if not (o' = b') then 
     let _ = Printf.printf "\nThe following changes from o' to b' can not be reconciled \n" in 
       print_conflicts 1 index_tbl_a2 in 
 ([], o',a',b')

(* ---------------------------------------------------------------------- *)


let cycle = Prefs.createBool "cycle" false
  "Use cycle merge algorithm to synchronize lists"
  "Use cycle merge algorithm to synchronize lists"

let sync elt_schema (o,a,b) =
 if Prefs.read cycle then cycle_merge_sync elt_schema (o,a,b) 
  else 
diff3_sync elt_schema (o,a,b)

end (* functor Make *)

 
