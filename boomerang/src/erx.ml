type tag = string 

module TagMapPlus = Mapplus.Make(
  struct
    type t = tag
    let compare (s1:tag) s2 = Pervasives.compare s1 s2
  end)
module TM = TagMapPlus.Map
module TS = TagMapPlus.KeySet
module TagMap = TM
module TagSet = TS
              
type u =
  | Box of tag * t
  | BoxStar of tag * string * t * string
  | Seq of t * t
  | Alt of t * t
  | Star of t 
  | Key
  | Leaf
and t = u * Brx.t

let rec format_t (u1,r1) = match u1 with 
  | Box(tg,t11) -> 
     Util.format "@[%s%s" "<" (if tg = "" then "" else tg ^ ":");
      format_t t11;
      Util.format "%s@]" ">" 
| BoxStar(tg,w1,t11,w2) -> 
    Util.format "@[%s%s%s" "<" (if tg = "" then "" else tg ^ ":") w1;
    format_t t11;
    Util.format "%s%s*@]" w1 ">" 
| Seq(t11,t12) | Alt(t11,t12) -> 
    let is_alt = match u1 with Alt _ -> true | _ -> false in 
      Util.format "@[";
      format_t t11;
      Util.format "%s@," (if is_alt then "|" else ".");
      format_t t12;
      Util.format "@]"
| Star(t1) -> 
      Util.format "@[";
      format_t t1;
      Util.format "*@]"
| Key | Leaf -> 
    let is_key = u1 = Key in 
      Util.format "@[%s" (if is_key then "key " else "leaf ");
      Brx.format_t r1;
      Util.format "@]"

let string_of_t t = 
  Util.format_to_string (fun () -> format_t t)

type spine_elt =
  | SBox of tag
  | SBoxStar of string * tag * string
  | SString of string

type spine = spine_elt list

let format_spine_elt = function
  | SBox(tg) -> Util.format "@[%s%s%s@]" "<" tg ">"
  | SBoxStar(w1,tg,w2) -> Util.format "@[%s%s:%s.%s%s*@]" "<" tg w1 w2 ">"
  | SString(w) -> Util.format "@[%s@]" (Misc.whack w)

let format_spine sp = 
  Util.format "@[[";
  Misc.format_list " " format_spine_elt sp;
  Util.format "]@]"
let string_of_spine sp = 
  Util.format_to_string (fun () -> format_spine sp)
   
type key = string
               
type box_content = (key * string) list
                 
let format_box_content bc = 
  Util.format "@[[";
  Misc.format_list ", " 
    (fun (k,w) -> Util.format "%s = %s" (Misc.whack k) (Misc.whack w))
    bc;
  Util.format "]@]"
let string_of_box_content bc = 
  Util.format_to_string (fun () -> format_box_content bc)

type skeleton = spine * box_content TM.t

let format_skeleton (sp,tm) = 
  Util.format "@[";
  Util.format "skel(";
  format_spine sp;
  Util.format ", {";
  ignore (TM.fold 
    (fun t bc is_fst -> 
       if not is_fst then Util.format ",";
       Util.format "%s -> " (Misc.whack t);
       format_box_content bc;
       false)
    tm true);
  Util.format "})@]"
let string_of_skeleton sk = 
  Util.format_to_string (fun () -> format_skeleton sk)
    
let rec has_box (u,_) = match u with
  | Box(_) | BoxStar(_) -> true
  | Key | Leaf -> false
  | Seq(t1,t2) -> 
      has_box t1 || has_box t2
  | Alt(t1,t2) -> 
      has_box t1 || has_box t2
  | Star(t1) -> 
      has_box t1 

let rec has_key (u,_) = match u with
  | Key -> true
  | Box _ | BoxStar _ | Leaf -> false
  | Seq(t1,t2) -> 
      has_key t1 || has_key t2
  | Alt(t1,t2) -> 
      has_key t1 || has_key t2
  | Star(t1) -> 
      has_key t1

let rec flatten (u,r) = match u with
  | Box(tg,t1) -> Some ("",Some(tg,t1,""))
  | Key | Leaf -> 
      if Brx.is_singleton r then         
        let w = match Brx.representative r with 
          | None -> assert false 
          | Some w -> w in 
        Some (w,None)
      else None
  | Seq(t1,t2) -> 
      begin match flatten t1,flatten t2 with
        | Some(w11,Some(tg,t11,w12)),Some(w2,None) -> 
            Some(w11,Some (tg,t11,w12 ^ w2))
        | Some(w1,None),Some(w21,Some(tg,t21,w22)) -> 
            Some(w1 ^ w21,Some(tg,t21,w22))
        | Some(w1,None),Some(w2,None) -> 
            Some(w1 ^ w2,None)
        | _ -> None end
  | Alt(t1,t2) -> None
  | BoxStar(_) -> None
  | Star(t1) -> None

(* lifted operations ... *)
let iterable t1 = 
  match flatten t1 with 
    | Some _ -> true
    | None -> not (has_box t1)

let mk_star t1 = 
  let (_,r1) = t1 in     
  let u = match flatten t1 with
    | Some(w1,Some(tg,t11,w2)) -> BoxStar(tg,w1,t11,w2) 
    | _ -> 
        if has_box t1 then 
          Berror.run_error (Info.M "mk_star") 
            (fun () -> Util.format "@[mk_star: %s contains a box that cannot be flattened@]" (string_of_t t1))
        else if has_key t1 then Star(t1)
        else Leaf in
  (u,Brx.mk_star r1)

let mk_seq t1 t2 = 
  let (_,r1) = t1 in 
  let (_,r2) = t2 in 
  let u = 
    if has_box t1 || has_key t1 || has_box t2 || has_key t2 then Seq(t1,t2)
    else Leaf in 
  (u,Brx.mk_seq r1 r2)

let mk_alt t1 t2 = 
  let (_,r1) = t1 in 
  let (_,r2) = t2 in 
  let u = 
    if has_box t1 || has_key t1 || has_box t2 || has_key t2 then Alt(t1,t2)
    else Leaf in 
   (u,Brx.mk_alt r1 r2)

let mk_key r1 = 
  (Key,r1)

let mk_leaf r1 = 
  (Leaf,r1)

let mk_box tg t1 = 
  let (_,r1) = t1 in 
  (Box(tg,t1),r1)

let lift1 f (_,r) = f r 
let lift2 f (_,r1) (_,r2) = f r1 r2 

let erase (_,r) = r

let match_string = lift1 Brx.match_string 
let star_split = lift1 Brx.star_split
let seq_split (_,r1) (_,r2) w = 
  match Brx.seq_split r1 r2 w with 
    | None -> 
        Berror.run_error (Info.M "Erx.seq_split")
          (fun () -> Util.format "@[the concatenation of %s and %s is ambiguous@]" 
             (Brx.string_of_t r1) (Brx.string_of_t r2))
    | Some p -> p

(* operations *)
let rec key (u,_) w = match u with
| Box(_)       -> ""
| BoxStar(_)   -> ""
| Key          -> w
| Seq(t1,t2)   ->
    let w1,w2 = seq_split t1 t2 w in
    key t1 w1 ^ key t2 w2
| Alt(t1,t2)   ->
    if match_string t1 w then key t1 w
    else key t2 w
| Leaf         -> ""
| Star(t1)      -> 
    Safelist.fold_left (fun acc wi -> acc ^ key t1 wi) "" (star_split t1 w)
    
(* LATER: this contains many horrible appends/combines... optimize *)        
let rec parse t w =
  let u,_ = t in 
  match u with
    | Box(tg,t1) ->        
      let k = key t1 w in
      ([SBox tg], TM.add tg [k,w] TM.empty)
    | BoxStar(tg,w1,t1,w2) ->
        let n1 = String.length w1 in 
        let n2 = String.length w2 in 
        let l1 = mk_leaf (Brx.mk_string w1) in 
        let l2 = mk_leaf (Brx.mk_string w2) in 
        let pad_t1 = mk_seq l1 (mk_seq t1 l2) in 
        let wl = star_split pad_t1 w in 
        let tm =
          Safelist.fold_left
            (fun tmacc wi ->
               let unpad_wi = String.sub wi n1 (String.length wi - n1 - n2) in 
               let ki = key t1 unpad_wi in 
               let tm_tg = TM.safe_find tg tmacc [] in
               TM.add tg (tm_tg@[ki,unpad_wi]) tmacc)
            TM.empty wl in
        ([SBoxStar(w1,tg,w2)],tm)                
    | Key | Leaf | Star _ -> 
      ([SString w],TM.empty)
    | Seq(t1,t2) ->
        let w1,w2 = seq_split t1 t2 w in
        let sp1,tm1 = parse t1 w1 in
        let sp2,tm2 = parse t2 w2 in
        let tm12 = 
          TM.fold (fun tgi bci acc -> 
                     let bc1i = TM.safe_find tgi tm1 [] in 
                     TM.add tgi (bc1i @ bci) acc)
            tm2 TM.empty in 
        (sp1@sp2,tm12)
    | Alt(t1,t2) ->
        if match_string t1 w then parse t1 w
        else parse t2 w
        
let unparse_spine_elt tm = function
  | SBox(tg) ->
      (match TM.find tg tm with
         | (_,w)::rest -> (w,TM.add tg rest tm)
         | []     -> assert false)
        
  | SBoxStar(w1,tg,w2) ->
      let kl = TM.find tg tm in
      (Misc.concat_list "" (Safelist.map (fun (ki,wi) -> w1 ^ wi ^ w2) kl),
       TM.remove tg tm)
          
  | SString(w) -> (w,tm)
    
let unparse (sp,tm) =
  let w,_ = 
    Safelist.fold_left 
      (fun (acc,tmi) spi -> 
         let wi,tmj = unparse_spine_elt tmi spi in 
         (acc ^ wi,tmj)) 
      ("",tm) sp in 
  w
      
let box_content (_,tm) tg =
  TM.safe_find tg tm []

let rec box_type (ty,_) tg = match ty with
 | Box(tg',t1) | BoxStar(tg',_,t1,_) ->
     if tg = tg' then Some t1
     else None
 | Key | Leaf | Star _ -> None
 | Seq(t1,t2) ->
     (match box_type t1 tg,box_type t2 tg with
     | Some t,None | None, Some t -> Some t
     | Some t,Some _ -> Some t
     | _ -> None)
 | Alt(t1,t2) ->
     (match box_type t1 tg,box_type t2 tg with
     | Some t,_ | _, Some t -> Some t
     | _ -> None)

let spine_tags sp =
  Safelist.fold_left
    (fun acc spi -> match spi with
    | SBox(tg) | SBoxStar(_,tg,_) -> TS.add tg acc
    | _ -> acc)
    TS.empty sp



               
