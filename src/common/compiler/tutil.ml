(* Feature Tree Types
 * tutil.ml - utilities (parser wraper, pretty printers, etc.)
 * Nate Foster <jnfoster@cis.upenn.edu>
 * $Id: tutil.ml,v 1.5 2004/12/13 17:05:25 ckirke Exp $
 *)

open Syntax

let length = List.length
	  		   
(* generic pretty-printing helper functions *)
let concat sep list = 
  List.fold_right 
    (fun h t -> if (t = sep) then h else (h ^ sep ^ t))
    list
    sep
let enclose s p1 p2 = concat "" [p1; s; p2]
let braces s = enclose s "(" ")"
let brackets s = enclose s "[" "]" 
let curlybraces s = enclose s "{" "}" 

(* coerce a pre-type to string *)
let rec pt2str = function
  | PTEmpty(_) -> "empty"
  | PTEmptyView(_) -> "{}"
  | PTVar(s,_) -> s
  | PTAny(o,f,pt,_) ->       
      concat ""
	["!"
	; if (length f = 0) then "" else "\\" ^ (curlybraces (concat " " f))
	; if o then "?" else "" 
	; brackets (pt2str pt)
	]
  | PTAll(o,f,pt,_) ->       
      concat ""
	["*"
	; if (length f = 0) then "" else "\\" ^ (curlybraces (concat " " f))
	; if o then "?" else "" 
	; brackets (pt2str pt)
	]
  | PTName(o,n,pt,_) -> 
      concat "" [ n ; if o then "?" else ""; brackets (pt2str pt)]
  | PTCat(pt1,pt2,_) ->
      concat "" [ pt2str pt1 ; "." ; pt2str pt2]
  | PTInter(pt1,pt2,_) -> 
      braces (concat " " [ pt2str pt1 ; "&" ; pt2str pt2])
  | PTUnion(pt1,pt2,_) -> 
      braces (concat " " [ pt2str pt1 ; "|" ; pt2str pt2])
  | PTDiff(pt1,pt2,_) -> 
      braces (concat " " [ pt2str pt1 ; "-" ; pt2str pt2])
	
let rec pdef2str (x,_,pt) = concat " " ["type"; x; "="; (pt2str pt)]
let rec pdefs2str ds = concat "\n" (List.map pdef2str ds)

let cs2str (x,is,ds) = 
  concat "" 
    (x::
       if (is = [] && ds = []) then []
       else 
	 [ "&"; if (is = []) then "{}" else curlybraces (concat " " is)
	 ; "-"; if (ds = []) then "{}" else curlybraces (concat " " ds)
	 ])      

let rec t2str = function
  | TAny(f,x,_)  ->
      concat ""
	[ "!"
	; if (f = []) then "" else "\\" ^ (curlybraces (concat " " f))
	; brackets (cs2str x)]
  | TAll(f,x,_)  ->
      if (f = [] && x = ("EMPTY",[],[])) then "{}" else
	concat ""
	  [ "*"
	  ; if (f = []) then "" else "\\" ^  (curlybraces (concat " " f))
	  ; brackets (cs2str x)]
  | TName(n,x,_) ->
      concat ""
	[ n; brackets (cs2str x)]
  | TCat(cs,_)   -> 
      concat "." (List.map t2str cs)
  | TUnion(ts,_)  -> 
      braces (concat " | " (List.map t2str ts))
  | TEmpty(_)     -> "empty"  
  | TEmptyView(_) -> "{}"
      
let rec def2str (x,_,t) = concat " " ["type"; x; "="; (t2str t)]
let rec defs2str ds = concat "\n" (List.map def2str ds)

let tl2str tl = brackets (if tl = [] then "" else concat "; " (List.map t2str tl))
let tll2str tll = brackets (if tll = [] then "" else concat " " (List.map tl2str tll))
let tpl2str tpl = 
  brackets ( 
    if tpl = [] then "" 
    else 
      concat "; " (List.map (fun (x,y) -> 
			       braces (concat "" [t2str x;", "; t2str y])) 
		     tpl)
  )
