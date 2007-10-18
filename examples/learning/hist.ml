open Lex

(** a histogram for a given token and set of chunks is a list of pairs
    of numbers: 

    the first number represents the frequency of the token in the
    chunks (i.e., how many times does it appear)

    the second number is how many chunks have the token with that
    frequency
*)
type t = (int * int) list

type histogram = Lex.token * t

type histograms = histogram list

let count_in_chunk query_t ts =
  List.fold_left
    (fun count t ->
       count + (if Lex.token_equal query_t t 
		then 1 
		else 0))
    0
    ts
   
let for_token chunks_ts query_t =
  let raw_counts = List.map (count_in_chunk query_t) chunks_ts in
  let sorted_counts = List.sort compare raw_counts in
  let rec accum_counts counts last accum =
    match counts, last with
	[], None -> []
      | [], Some last -> [(last, accum)]
      | c::counts, Some c' when c = c' -> accum_counts counts last (accum + 1)
      | c::counts, Some last -> (last, accum)::(accum_counts counts (Some c) 1)
      | c::counts, None -> accum_counts counts (Some c) 1
  in
    accum_counts sorted_counts None 0

(** collects the list of all tokens used in the list of chunks (using
    token_equal, so different strings matched by the same regex or
    delimiter count only once)
*)
let tokens_in_chunks chunks_ts =
  let already seen t = List.exists (fun t' -> Lex.token_equal t t') seen in
  let abstract t =
    match t with
	RegexToken (_, name) -> RegexToken ("", name)
      (* it is an important invariant that we preserve the left
	 and right delimiters -- the oracle will need them *)
      | MetaToken (l, _, r, name) -> MetaToken (l, [], r, name)
  in
  let collect_chunks seen ts =
    List.fold_left 
      (fun seen t ->
	 if already seen t
	 then seen
	 else (abstract t)::seen)
      seen
      ts
  in
    List.fold_left collect_chunks [] chunks_ts

(** returns the count for a given frequency, zero if it isn't found *)
let count (hist : t) (f : int) =
  try List.assq f hist
  with Not_found -> 0

(** normalizes a histogram.  this is the h-bar of Fisher, et al., a
    histogram with the zero frequency first, and then sorted
    descending by the second component, i.e., the number of chunks
    with the frequency in the first component
*)
let normal_form (hist : t) =
  (* we pull out the zero element, holding on to its count *)
  let zero_count = count hist 0 in
  let hist = List.remove_assq 0 hist in

  (* a normalized histogram is sorted descending order by the second
     component (number of chunks with a given frequency).  our custom
     compare function inverts the comparison on the second elements of
     the tuple *)
  let compare_entry (f1, c1) (f2, c2) = compare c2 c1 in
  let hist = List.sort compare_entry hist in

  (* remove anything that doesn't appear at least once *)
  let hist = List.filter (fun (_, c) -> c > 0) hist in
    (0, zero_count)::hist

let make (chunks_ts : (token list) list) : histograms =
  let tokens = tokens_in_chunks chunks_ts in
    List.map 
      (fun t -> 
	 (t, normal_form (for_token chunks_ts t)))
      tokens

let to_string hist =
  let ss = 
    List.map 
      (fun (f, c) -> 
	 "(" ^ (string_of_int f) ^ ", " ^ 
	       (string_of_int c) ^ ")") 
      hist
  in
    "[" ^ (String.concat "; " ss) ^ "]"

(** size(h-bar) = # of chunks total *)
let size (hist : t) = List.fold_left (+) 0 (List.map snd hist)

(** width(h-bar) = # of non-zero frequencies in h-bar *)
let width (hist : t) =
  List.length (List.remove_assq 0 hist)

(** the residual mass of a given column is the number of chunks in all
    less frequent columns as well as the zero column
*)
let residual_mass_for_column (hist : t) (column : int) : int =
  (* returns the histogram starting from the nth column of hist *)
  let rec residual_columns hist n =
    if n = 0
    then hist
    else residual_columns (List.tl hist) (n - 1)
  in
    
  (* remove the zero column (hist is normalized, so zero column is first *)
  let hist_nozero = List.tl hist in

  (* hist_nozero took away one column, so now we return the histogram
     starting from the n+1th column, so we leave out the column we're
     computing the residual mass for -- perfect! *)
  let residuals = List.map snd (residual_columns hist_nozero column) in
    (count hist 0) + (List.fold_left (+) 0 residuals)

(** computes the residual mass of the first column, which is all the
    oracle from Fisher et al. will care about 
*)
let residual_mass (hist : t) : int = residual_mass_for_column hist 1

(** computes the coverage of a histogram: the number of chunks in
    which the given token appears at least once 
*)
let coverage (hist : t) : int =
  let hist_nonzero = List.tl hist in
    List.fold_left (+) 0 (List.map snd hist_nonzero)

(** computes the relative entropy (asymmetric Kullback-Leibler divergence)
    of two histograms 
*)
let rec plain_relative_entropy (hist1 : t) (hist2 : (int * float) list) : float =
  let term c1 c2 = 
    let c1 = float_of_int c1 in
      c1 *. (log (c1 /. c2))
  in
  let rec pre h1 h2 =
    (* when computing the term, we use epsilon_float for new values in
       the second term rather than 0.0 because then we won't get
       infinity/neg_infinity terms, and don't run the risk of getting
       (yikes!) nan *)
    match h1, h2 with
	[], _ -> 0.0    
      | (_, c)::h1, [] -> (term c epsilon_float) +. (pre h1 [])
      | (f1, c1)::h1, (f2, c2)::h2 -> 
	  (term c1 c2) +. (pre h1 h2)
  in
    pre hist1 hist2

(** averages two histograms.  this is a greedy algorithm, and so
    expects the zeroes to be removed and the histogram to be
    sorted in ascending frequency
*)
let average (hist1 : t) (hist2 : t) : (int * float) list =
  let mean c1 c2 = ((float_of_int c1) +. (float_of_int c2)) /. 2.0 in

  (* we greedily compute the average for each frequency *)
  let rec avg hist1 hist2 = 
    match hist1, hist2 with
	[], [] -> []
	  
      (* single case -- just average them *)
      | [], (f, c)::hist
      | (f, c)::hist, [] -> (f, mean c 0)::(avg hist [])
	  
      (* same frequencies -- just average *)
      | (f1, c1)::hist1, (f2, c2)::hist2 ->
	  (f1, mean c1 c2)::(avg hist1 hist2)    
  in
    avg hist1 hist2

let relative_entropy(hist1 : t) (hist2 : t) : float =
  (* eliminate zero columns *)
  let hist1 = List.tl hist1 in
  let hist2 = List.tl hist2 in

  let avg = average hist1 hist2 in
  let re1 = plain_relative_entropy hist1 avg in
  let re2 = plain_relative_entropy hist2 avg in
(*      print_float re1; print_string " (+) "; print_float re2; print_newline (); *)
      (re1 +. re2) /. 2.0

let print_list ss =
  print_string "[";
  print_string (String.concat "; " ss);
  print_endline "]"

(* let _ =  *)
(*   let chunks = ["{(\"abc\") [123]--[456]"; *)
(* 		"{{(def) [123]--"; *)
(* 		"{(\"abc\") [110]--[312]"; *)
(* 	        "{{(bar)."] *)
(*   in *)
(*   let chunks_ts =  *)
(*     List.map  *)
(*       (fun chunk -> parse_chunk chunk delimiter_table regex_table) *)
(*       chunks *)
(*   in *)
(*     List.iter (fun p -> print_list (List.map string_of_token p)) chunks_ts; *)
(*     let hists = make chunks_ts in *)
(* 	List.iter  *)
(* 	  (fun (t, hist) -> *)
(* 	     print_string (string_of_token t); *)
(* 	     print_string " (w="; *)
(* 	     print_string (string_of_int (width hist)); *)
(* 	     print_string ")"; *)
(* 	     print_string ": "; *)
(* 	     print_endline (to_string hist)) *)
(* 	  hists; *)
(*       print_endline (string_of_int (residual_mass [(0,5);(2,25);(3,15);(1,10)])); *)
(*       let test_re hist1 hist2 = *)
(* 	let re = relative_entropy hist1 hist2 in *)
(* 	  print_float re; *)
(* 	  print_newline () *)
(*       in *)
(* 	test_re [(0,5);(2,25);(3,15);(1,10)] [(0,12);(2,25);(4,10);(1,8)]; *)
(* 	test_re	[(0,5);(2,25);(3,15);(1,10)] [(0,10);(2,50);(3,30);(1,20)]; *)
(* 	test_re [(0,5);(2,25);(3,15);(1,10)] [(0,5);(2,25);(3,15);(1,10)]; *)
(* 	test_re [(0,50);(2,250);(3,150);(1,100)] [(0,50);(2,249);(3,150);(1,100);(5,1)]; *)
(* 	test_re [(0,50);(2,250);(3,150);(1,100)] [(0,51);(2,249);(3,150);(1,100)]; *)
(* 	test_re [(0,5000);(1,4900)] [(0,4999);(1,4901)]; *)
(* 	test_re [(0,5000);(1,4900)] [(0,4990);(1,4910)] *)
