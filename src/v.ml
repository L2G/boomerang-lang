(* Views are the synchronizer's most basic abstraction. *)   

(* well formed or not, with reason *)
type wf = WF | NWF of string

(* the bool indicates if the view is well formed *)
type t = VI of wf * t Name.Map.t

type reader = string -> t option
type writer = t option -> string -> unit

let nil_tag = "*nil"
let head_tag = "*h"
let tail_tag = "*t"

exception Illformed of string * (t list)

(* ----------------------------------------------------------------------
 * Accessors
 *)

let dom (VI (_,m)) = Name.Map.domain m

let is_empty v = Name.Set.is_empty (dom v)

let get (VI (_,m)) k = 
  try
    Some (Name.Map.find k m)
  with
  | Not_found -> None

let get_required ((VI (_,m)) as v) k =
  try Name.Map.find k m
  with Not_found ->
    raise (Illformed ("V.get_required: missing child \""^k^"\" in view ", [v]))
      
let is_well_formed (VI (b,_)) = b = WF

let check_well_formed = function
  | VI (NWF s, _) as v -> raise (Illformed (s, [v]))
  | _ -> ()
    
(* ----------------------------------------------------------------------
 * Creators
 *)

let empty = VI (WF, Name.Map.empty)

let set_unsafe (VI (b,m)) k kid =
  match kid with
  | None -> VI (b,(Name.Map.remove k m))
  | Some v -> VI (b,(Name.Map.add k v m))

let set v k kid = set_unsafe v k kid 

let set_star v binds =
   Safelist.fold_left (fun vacc (k,kid) -> set_unsafe vacc k kid) v binds

let create_star binds = set_star empty binds
       
let from_list vl = VI (WF,(Name.Map.from_list vl))

(* ----------------------------------------------------------------------
 * Singletons, Values, and Fields
 *)

let singleton k v = set empty k (Some v)

let is_singleton v = Name.Set.cardinal (dom v) = 1

let singleton_dom v =
  let d = dom v in
  if Name.Set.cardinal d <> 1 then
    raise (Illformed ("singleton_dom: view with several children", [v]));
  Name.Set.choose d

(* v is a value if it has one child, and that child is [V.empty] *)
let is_value v =
  let d = dom v in 
    (Name.Set.cardinal d = 1) &&
    (is_empty (get_required v (Name.Set.choose d)))
      
let new_value s = singleton s empty 

let get_value v =
  if (is_value v) then (Name.Set.choose (dom v))
  else raise (Illformed ("get_value: not a value",[v]))

let get_field_value v k = get_value (get_required v k)

let get_field_value_option v k =
  match get v k with
    Some v' -> Some(get_value v')
  | None -> None

let set_field_value v k s = set v k (Some (new_value s))

let field_value k s = set_field_value empty k s

(* ----------------------------------------------------------------------
 * Lists
 *)

let cons v1 v2 = create_star [(head_tag, Some v1); (tail_tag, Some v2)]

let empty_list = create_star [(nil_tag, Some empty)]
		   
let is_cons v = Name.Set.equal
		  (dom v) 
		  (Name.Set.add head_tag 
		     (Name.Set.add tail_tag Name.Set.empty))
		  
let is_empty_list v = Name.Set.equal
			(dom v) 
			(Name.Set.add nil_tag Name.Set.empty)

let rec is_list v = 
  is_empty_list v || 
  (is_cons v &&
   is_list (get_required v tail_tag))

let head v = get_required v head_tag
let tail v = get_required v tail_tag

let rec list_from_structure v =
  let rec loop acc v' = 
    if is_empty_list v' then Safelist.rev acc else
      if is_list v' then
	loop ((get_required v' head_tag) :: acc) (get_required v' tail_tag)
      else raise (Illformed ("list_from_structure: this is not a list!", [v])) in 
    loop [] v 

(* ### Not tail recursive! *)
let rec structure_from_list = function
  | [] -> empty_list
  | v :: vs -> cons v (structure_from_list vs)


(* ----------------------------------------------------------------------
 * Easy building
 *)

type desc =
    V of (Name.t * desc) list
  | L of desc list
  | Val of Name.t
  | In of t
  | E

let rec from_desc = function
    E -> empty
  | Val k -> new_value k
  | L vl -> structure_from_list (Safelist.map from_desc vl)
  | V l -> from_list (Safelist.map (fun (k,d) -> (k, from_desc d)) l) 
  | In v -> v


(* ----------------------------------------------------------------------
 * Utility functions
 *)

let rec equal v1 v2 =
  if v1 == v2 then true else
  let names = dom v1 in
  Name.Set.equal names (dom v2) &&
  Name.Set.for_all (fun n -> equal (get_required v1 n) (get_required v2 n)) 
    names

let equal_opt v1o v2o =
  match v1o, v2o with
    None, None -> true
  | Some v1, Some v2 -> equal v1 v2
  | _, _ -> false

let fold f (VI (_,m)) c = Name.Map.fold f m c

let map f v =
    fold (fun k vk vacc -> set_unsafe vacc k (f vk)) v empty

let mapi f v =
    fold (fun k vk vacc -> set_unsafe vacc k (f k vk)) v empty

let for_all f = function VI (_,v) ->
  Name.Map.for_all f v

let for_alli f = function VI (_,v) ->
  Name.Map.for_alli f v

let to_list v =
  fold (fun n k acc -> (n,k) :: acc) v []

let concat v1 v2 =
  let binds = (to_list v1) @ (to_list v2) in
  try
    from_list binds
  with
  | Invalid_argument _ -> raise 
                           ( Illformed
                           ("concat: domain collision between the following two views: ",
                           [v1;v2;]))
  | Illformed _ -> raise
                     ( Illformed
                     ("concat: the view obtained while concatenating the following two view is not well formed: ",
                     [v1;v2;]))
    
let iter f (VI (_,m)) = Name.Map.iter f m

let split p v =
  let binds1,binds2 =
    fold
      (fun k kv (v1acc,v2acc) ->
	if p k then
          ((k,Some kv)::v1acc,v2acc)
	else
          (v1acc, (k,Some kv)::v2acc))
      v ([],[]) in
  (create_star binds1, create_star binds2)

let same_root_sort v1 v2 =
  Name.Set.equal (dom v1) (dom v2)

(* ---------------------------------------------------------------------- *)
(* PRETTY PRINTING *)

let raw = 
  Prefs.createBool "raw" false "Dump views in 'raw' form" ""
      
let rec format ((VI (_,m)) as v) =
  if (not (Prefs.read raw)) then
    if is_list v then begin
      let rec loop = function
          [] -> ()
	| [kid] -> format kid
	| kid::rest -> format kid; Format.printf ",@ "; loop rest in
      Format.printf "[@[<hv0>";
      loop (list_from_structure v);
      Format.printf "@]]"
    end else begin
      if (is_value v) then
	Format.printf "%s" (Misc.whack (get_value v))
      else begin
        Format.printf "{@[<hv0>";
	Name.Map.iter_with_sep
	  (fun k kid -> 
	    Format.printf "@[<hv1>%s =@ " (Misc.whack k);
	    format kid;
	    Format.printf "@]")
          (fun() -> Format.printf ",@ ")
          m;
        Format.printf "@]}"
      end
    end 
  else 
    Name.Map.dump 
      (fun ks -> ks)
      Misc.whack 
      (fun x -> format x) 
      (fun (VI (_,m)) -> Name.Map.is_empty m)
      m

let format_option = function
    None -> Format.printf "NONE"; 
  | Some v -> format v


let rec format_raw ((VI (_,m)) as v) =
  Name.Map.dump 
    (fun ks -> ks)
    Misc.whack 
    (fun x -> format_raw x) 
    (fun (VI (_,m)) -> Name.Map.is_empty m)
    m  

let string_of_t v = 
  let out,flush = Format.get_formatter_output_functions () in
  let buf = Buffer.create 64 in
    Format.set_formatter_output_functions 
      (fun s p n -> Buffer.add_substring buf s p n) (fun () -> ());
    format v;
    Format.print_flush();
    let s = Buffer.contents buf in
      Format.set_formatter_output_functions out flush;
      s

    
type msg = [ `String of string | `Name of Name.t | `Break | `View of t
           | `View_opt of t option | `Open_box | `Close_box ]

exception Error of msg list

let format_msg l = 
  let rec loop = function
    | [] -> ()
    | `String s :: r ->
        Format.printf "%s" s; (* Format.print_flush (); *) loop r
    | `Name k :: r ->
        Format.printf "%s" (Misc.whack k); (* Format.print_flush (); *) loop r
    | `Break :: r ->
        Format.printf "@,"; (* Format.print_flush (); *) loop r
    | `View v :: r ->
        Format.printf "@,  @[<v2>";
        format v;
        Format.printf "@]@,";
        (* Format.print_flush(); *)
        loop r
    | `View_opt v :: r ->
        Format.printf "@,  @[<v2>";
        format_option v;
        Format.printf "@]@,";
        loop r
    | `Open_box :: r ->
        Format.printf "@,  @[<v2>";
        loop r
    | `Close_box :: r ->
        Format.printf "@]";
        loop r

  in
  Format.printf "@[<v0>";
  loop l;
  Format.printf "@,@]"

let dump_to_file fname v =
  Format.print_flush ();
  let out,flush = Format.get_formatter_output_functions () in
  Format.set_formatter_out_channel (open_out fname);
  format v;
  Format.print_flush ();
  Format.set_formatter_output_functions out flush

let error_msg l =
  raise (Error l)
  
let pathchange path m v =
  Format.printf "%s: %s@,"
    (String.concat "/" (Safelist.rev (Safelist.map Misc.whack path)))
    m;
  Format.printf "  @[";
  format v;
  Format.printf "@]@,"

let rec show_diffs_inner v u path =
  let vkids = dom v in
  let ukids = dom u in
  let allkids = Name.Set.union vkids ukids in
  Name.Set.iter
    (fun k ->
       let path = k::path in
       match (get v k, get u k) with
         None, None -> assert false
       | Some vk, None -> pathchange path "deleted value" vk
       | None, Some uk -> pathchange path "created with value" uk
       | Some vk, Some uk -> show_diffs_inner vk uk path)
    allkids

let rec show_diffs v u =
  Format.printf "@[<v0>";
  show_diffs_inner v u [];
  Format.printf "@]"
  
(* CK's old pretty printer for views *)
(* let rec pretty_print ((VI (_,m)) as v) = *)
(*   if is_list v then  *)
(*     begin (\* A list *\) *)
(*       let rec loop pp lst =  *)
(* 	match lst with *)
(*             [] -> () *)
(* 	  | [kid] -> pp kid  *)
(* 	  | kid::rest -> pp kid; Format.printf ",@ "; loop pp rest  *)
(*       in *)
(*       let rec pp v =  *)
(* 	if is_value v then  *)
(* 	  Format.print_string (get_value v) *)
(* 	else  *)
(* 	  pretty_print v *)
(*       in *)
(*       let lst = list_from_structure v in *)
(* 	Format.printf "[@[<hv0>"; *)
(* 	loop pp lst; *)
(* 	Format.printf "@]]" *)
(*     end  *)
(*   else  *)
(*     begin *)
(*       if (is_value v) then  *)
(* 	begin (\* A value *\) *)
(* 	  Format.printf "{%s}" (Misc.whack (get_value v)) *)
(* 	end *)
(*       else  *)
(* 	begin  *)
(* 	  if (Name.Map.for_all (fun kid -> is_empty kid) m) then *)
(* 	    begin (\* All entries are values - so print only labels *\) *)
(* 	      Format.printf "{@[<hv0>"; *)
(* 	      Name.Map.iter_with_sep *)
(* 		(fun k kid -> Format.printf "%s" (Misc.whack k)) *)
(* 		(fun() -> Format.printf ",@ ") *)
(* 		m;	       *)
(* 	      Format.printf "@]}"; *)
(* 	    end *)
(* 	  else  *)
(* 	    begin (\* Not all entres are values *\) *)
(* 	      Format.printf "@[<hv0>"; *)
(*               Format.printf "{@[<hv0>"; *)
(* 	      (\* REMOVE?: if (Name.Map.size m) > 1 then Format.force_newline () else (); *\) *)
(* 	      Name.Map.iter_with_sep *)
(* 		(fun k kid ->  *)
(* 		   Format.printf "@[<hv0>%s =@ " (Misc.whack k); *)
(* 		   pretty_print kid; *)
(* 		   Format.printf "@]") *)
(* 		(fun() -> (Format.printf ",";Format.force_newline ())) *)
(* 		m; *)
(* 	      Format.printf "@]"; *)
(* 	      (\* REMOVE?: if (Name.Map.size m) > 1 then Format.force_newline () else (); *\) *)
(* 	      Format.printf "}"; *)
(* 	      Format.printf "@]"; *)
(* 	    end *)
(* 	end *)
(*     end  *)
