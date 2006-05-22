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

let sync elt_schema (o,a,b) =
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

      (* old code not needed    
	 let get_lines sl el arr =
         let isfirst = (sl = -1) in 
         let len = if isfirst then el-sl-1 else el-sl in  
         let start = if isfirst then 0 else sl in  
         if (len > 0) then 
         (* BCP: Next line is hideous... *)
         Array.to_list (Array.init len (fun i -> arr.(start+i)))
             else 
         [] in
      *)
	 let get_lines sl el arr =
           let isfirst = (sl = -1) in
           let len = el - sl in 
           let start = if isfirst then 0 else sl in 
             if (len > 0) then 
               (* BCP: Next line is hideous... *)
               Array.to_list (Array.init len (fun i -> arr.(start+i)))
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
           let rec list_change_lines m = function
               [] -> ()
             | [e]     -> Format.printf "%s (%s) " m s; A.format e
             | e::rest -> Format.printf "%s (%s) " m s; A.format e;
                 Format.printf ",@,"; list_change_lines m rest in
             if nw = [] then list_change_lines "Delete" ol
             else if ol=[] then list_change_lines "Add" nw 
             else list_change_lines "Change" nw 
	 in
         let onew = get_lines (so+1) eo arr_o in 
         let anew = get_lines (sa+1) ea arr_a in 
         let bnew = get_lines (sb+1) eb arr_b in  
           
	 (* so, eo are the matching line numbers - so the differing lines are
	    so+1, so+2 ...eo-1  - 
	    When finally adding the lines to reconciled version, we need to add the common line too *)
         let common = if (sb = -1) then [] else get_lines (sb+1) eb arr_b in    
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

end (* functor Make *)

