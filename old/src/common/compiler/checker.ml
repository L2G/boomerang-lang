(***************************************)
(* Simple Type-checking to debug Focal *)
(***************************************)

(* Takes a prog, and gives an annotated prog *)

open Error
open Syntax
open Library

let projt = function 
    T t -> t 
  | _ -> assert false

(******************)
(** Environments **)
(******************)

type env = (string * typeschema) list
type refenv = ((string * typeschema) list) ref

let exists a env = List.mem_assoc a env
let assoc a env= List.assoc a env

let refexists a env = List.mem_assoc a (!env)
let refassoc a env = List.assoc a (!env)

(* Make a copy of an environment *)
let copy refenv = 
  let x = ref [] in x := !refenv ; x


(********************************)
(** String conversion of types **)
(********************************)
let type_to_string =
  let rec type_to_string_par par= function
    Vari x -> "'"^x
  | Lens -> "lens"
  | Arrow (s,t) ->if par then "("^(type_to_string_par true s)^" -> "^(type_to_string_par false t)^")"
    else (type_to_string_par true s )^" -> "^(type_to_string_par false t)
  | Predicate -> "pred"
  | View -> "view"
  | Maparg -> "map"
  | Name -> "name"
  | Schema -> "schema"
  | Undef _ -> "undefview" in
  type_to_string_par false

(* String conversion of an association list *)
let rec env_to_string = function
    [] -> ""
  | (s,T t)::q-> s^" : "^(type_to_string t)^"\n"^(env_to_string q)
  | (s,For_all(_,t))::q -> env_to_string ((s,t)::q)

(******************************************)
(** Fresh strings for variables in types **)
(******************************************)
let varcounter = ref 0
let init_varcounter () = varcounter:=0
let newvar ()=
  incr varcounter;
  "a"^(string_of_int (!varcounter))


(***************************)
(** Substitution of types **)
(***************************)
(* substitution of a by b in a type expression *)
let rec substype a b = function
    Vari s->if s=a then b else Vari s
  | Undef s->if s=a then b else Undef s
  | Arrow(x,y) -> Arrow(substype a b x,substype a b y)
  | x -> x

(* substitution of a by b in a type schema *)
let rec subschema a b = function
    T t -> T (substype a b t)
  | For_all (v,t) -> if v=a then For_all (v,t) else For_all (v,subschema a b t)

(* Application of substitutions *)
let rec apply sub = function
    Vari x -> if List.mem_assoc x sub then List.assoc x sub else Vari x
  | Undef x -> if List.mem_assoc x sub then List.assoc x sub else Undef x
  | Arrow(s,t) -> Arrow (apply sub s,apply sub t)
  | z -> z
(* Composition of two substitution *)
let compose sub1 sub2 =
  let sub3 = List.map (function (x,t)->(x,apply sub1 t)) sub2 in
  let rec select s = function
      [] -> s
    | (a,b)::q -> if List.mem_assoc a s then select s q else (a,b)::(select s q) in
  select sub3 sub1
(* Application of a substitution on a type *)
let rec apply_on_type sub = function
    T t -> T (apply sub t)
  | For_all (v,t) -> For_all (v,apply_on_type (List.remove_assoc v sub) t)
(* Application of a substitution on an environment *)
let apply_on_refenv sub env = 
  let l = List.map (function (a,t) -> (a,apply_on_type sub t)) (!env) in
  env := l
(* Application of a substitution on an environment *)
let apply_on_env sub = List.map (function (a,t) -> (a,apply_on_type sub t))


(********************)
(** Free Variables **)
(********************)
(* remove a from the list *)
let rec substract a =function
    []->[]
  | h::r-> (if h=a then [] else [h])@(substract a r)
(* Union *)
let rec union l=function
    [] -> l
  | a::q -> union (a::(substract a l)) q
(* Free variables of a type expression *)
let rec fvt=function
    Vari a-> [a]
  | Undef _-> []
  | Arrow(s,t)->union (fvt s) (fvt t)
  | _ -> []
(* Free variables of a type schema *)
let rec fvtype=function
    T t -> fvt t
  | For_all (v,t) -> substract v (fvtype t)


(*****************************)
(** Instantiation of a type **)
(*****************************)
let rec inst = function
    T c -> c
  | For_all (v,t) -> let x=newvar() in inst (subschema v (Vari x) t)


(*****************************)
(** Generalisation of types **)
(*****************************)
(* Free variables minus an environment *)
let fvgen env a =
  let rec vardef en=function
      [] -> []
    | b::q -> (if List.mem_assoc b en then [] else [b])@(vardef en q) in
  vardef env (fvt a)

(* Generalisation of a type expression *)
let generalize env a =
  let rec genaux t = function
      [] -> t
    | b::q -> For_all(b,genaux t q) in
  let newenv = (fvgen env a) in
    genaux (T a) newenv

(*********)
(** MGU **)
(*********)
let present x l=
  let rec presentexpr x=function
      Vari y -> x=y
    | Undef y -> x=y
    | Arrow(u,v) -> (presentexpr x u) || (presentexpr x v)
    | _ -> false in
  let rec presentlist x = function
      [] -> false
    | (a,b)::q -> (presentexpr x a) || (presentexpr x b) || (presentlist x q) in
  presentlist x l

let rec absent x = function
    Vari y -> x<>y
  | Undef y -> x<>y
  | Arrow(u,v) -> (absent x u) && (absent x v)
  | _ -> true

let rec remplace x y = function
    [] -> []
  | (a,b)::q -> (substype x y a,substype x y b)::(remplace x y q)

(* Test if the system is in the resolved form *)
let testresolved equ =
  let aux1 l = function
      (Vari y,t) -> (absent y t) && not (present y l)
    | (Undef y,t) -> (absent y t) && not (present y l)
    | _ -> false in
  let rec aux2 l = function
      [] -> true
    | a::q -> (aux1 (l@q) a) && (aux2 (l@[a]) q) in
  aux2 [] equ
(* Conversion to a substitution *)
let rec resolved_to_sub = function
    [] -> []
  | (Vari x,t)::q -> (x,t)::(resolved_to_sub q)
  | (Undef x,t)::q -> (x,t)::(resolved_to_sub q)
  | _ -> assert false

(* MGU computation *)
let mgu i (t1,t2) =
  let rec transform lequ b =
    if b then
      begin
	function m -> ( lequ @ m, true)
      end
    else
      begin
	function
	    [] -> (lequ,false)
	  | (a,b)::q when a=b -> transform lequ true q
	  | (Arrow (s,t),Arrow (u,v))::q -> transform (lequ@[(s,u);(t,v)]) true q
	  | (Vari x,z)::q->
	      if (present x (lequ @ q))&&(absent x z)
	      then transform ((remplace x z (lequ@q))@[(Vari x,z)]) true []
	      else transform (lequ@[Vari x,z]) false q
	  | (y, Vari x)::q -> transform (lequ@[Vari x,y]) true q
	  | (Undef x,(View as z))::q->
	      if (present x (lequ @ q))
	      then transform ((remplace x z (lequ@q))@[(Undef x,z)]) true []
	      else transform (lequ@[Undef x,z]) false q
	  | (Undef x,(Predicate as z))::q->
	      if (present x (lequ @ q))
	      then transform ((remplace x z (lequ@q))@[(Undef x,z)]) true []
	      else transform (lequ@[Undef x,z]) false q
	  | (Undef x,(Undef _ as z))::q->
	      if (present x (lequ @ q))
	      then transform ((remplace x z (lequ@q))@[(Undef x,z)]) true []
	      else transform (lequ@[Undef x,z]) false q
	  | (y, Undef x)::q -> transform (lequ@[Undef x,y]) true q
	  | (a,b)::_ -> raise (Type_error ("The expression at "^(info_to_string i)^" has type: "^
					   (type_to_string a)^", but is used with type: "^
					   (type_to_string b),i))
      end in
  let still = ref ([t1,t2],true) in
  while snd (!still) do
    still := transform [] false (fst !still)
  done;
  let sys = fst !still in
  if testresolved sys
  then resolved_to_sub sys
  else 
    begin
      raise (Type_error ("The expression at "^(info_to_string i)^" has type: "^
			 (type_to_string t1)^", but is used with type: "^
			 (type_to_string t2),i))
    end

(***************************************************)
(* Link between variables and types ... *)
(* 
   - Replacing Variables with Names when necessary.
   - Replacing Views by variables with the same name as the undef constructor
   - New environment for views, to store the content.
   

 *)

(* Test for predicates vs views 
   Allows to recognize the views that can't be predicates *)
let rec ispredicate = function
    [] -> true
  | (_,AstView([],_,_,_))::q -> ispredicate q
  | _ -> false

(**********************************************************)
(** Algorithm of Damas-Milner-Tofte for type computation **)
(**********************************************************)

let rec type_expr viewenv env astexp = match astexp with
    AstVar (x,i) -> 
      if exists x env then (astexp,inst (assoc x env),[])
      else (AstName (x,i),Name,[])
  | AstName (x,i) ->
      (AstName (x,i),Name,[])
  | AstView (l,_,b,i) -> 
      let v = newvar () in
      viewenv := (v,T (Undef v))::(!viewenv);
      let rec eval_the_types accenv acclist accsub = function
	  [] -> 
	    if ispredicate acclist 
	    then (AstView(acclist,v,b,i),Undef(v), accsub)
	    else (AstView(acclist,v,b,i),View,accsub)
	| (s,e)::q -> 
	    let exp1,typ1,sub1 = type_expr viewenv accenv s in
	    let m1 = mgu i (typ1,Name) in
	    begin
	      apply_on_refenv m1 viewenv;
	      let accenv = (apply_on_env m1 (apply_on_env sub1 accenv)) in
	      let exp2,typ2,sub2 = type_expr viewenv accenv e in
	      match exp2,typ2 with
		  AstName (x,inf),Name -> 
		    eval_the_types (apply_on_env sub2 accenv) 
		    (acclist@[exp1,AstView([AstName (x,inf),emptyView inf],"",false,inf)]) (compose sub2 (compose m1 (compose sub1 accsub))) q
		| _,t ->
		    let u = newvar () in
		    let m2 = mgu i (t,Undef u) in
		    begin
		      apply_on_refenv m2 viewenv;
		      let newenv = apply_on_env m2 (apply_on_env sub2 accenv) in
		      eval_the_types newenv (acclist@[exp1,exp2]) (compose m2 (compose sub2 (compose m1 (compose sub1 accsub)))) q
		    end 
	    end in
      eval_the_types env [] [] l
  | AstMap (l,i) -> 
      let rec eval_the_types accenv acclist accsub = function
	  [] -> (AstMap(acclist,i),Maparg,accsub)
	| (s,e)::q -> 
	    let exp1,typ1,sub1 = type_expr viewenv accenv s in
	    let m1 = mgu i (typ1,Name) in
	    begin
	      apply_on_refenv m1 viewenv;
	      let accenv = (apply_on_env m1 (apply_on_env sub1 accenv)) in
	      let exp2,typ2,sub2 = type_expr viewenv accenv e in
	      let m2 = mgu i (typ2,Lens) in
	      apply_on_refenv m2 viewenv;
	      let newenv = apply_on_env m2 (apply_on_env sub2 accenv) in
	      eval_the_types newenv (acclist@[exp1,exp2]) (compose m2 (compose sub2 (compose m1 (compose sub1 accsub)))) q
	    end in
      eval_the_types env [] [] l
  | AstFun (x,e,i) -> 
      let a = newvar() in
      let exp,typ,sub = type_expr viewenv ((x,T (Vari a))::env) e in
      (AstFun (x,exp,i),Arrow (apply sub (Vari a),typ),sub)
  | AstApp (e,arg,i) -> 
      let exp1,typ1,sub1 = type_expr viewenv env e in
      let v = newvar() in
      let w = newvar() in
      let m1 = mgu (getInfo e) (typ1,Arrow (Vari v, Vari w)) in
      begin
	apply_on_refenv m1 viewenv;
	let accenv = (apply_on_env m1 (apply_on_env sub1 env)) in
	let exp2,typ2,sub2 = type_expr viewenv accenv arg in
	let m2 = mgu i (typ2,apply sub2 (apply m1 (Vari v))) in
	apply_on_refenv m2 viewenv;
	(AstApp(exp1,exp2,i), apply m2 (apply sub2 (apply m1 (Vari w))),compose m2 (compose sub2 (compose m1 sub1)))
      end
  | AstLet(f,arglist,e1,e2,i) -> 
      begin
	match List.rev arglist with
	    [] -> 
	      let exp1,typ1,sub1 = type_expr viewenv env e1 in
	      let newenv = apply_on_env sub1 env in
              let exp2,typ2,sub2 = type_expr viewenv ((f, generalize newenv typ1)::newenv) e2 in
	      (AstLet(f,[],exp1,exp2,i),typ2,compose sub2 sub1)
	  | a::q -> type_expr viewenv env (AstLet(f,List.rev q,AstFun(a,e1,i),e2,i))
      end
  | AstLetrec(deflist,expr,i) ->
      let rec collect = function
	  [] -> []
	| (f,_,_,_)::q ->let x = newvar() in (f,T (Vari x))::(collect q) in
      let newenv = (collect deflist)@env in
      let rec eval_the_types accenv accdef accsub = function
	  [] -> accenv,accdef,accsub
	| (f,_,e,i)::q ->
	    let exp,typ,sub = type_expr viewenv accenv e in
	    let m = mgu i (typ,Lens) in
	    begin
	      apply_on_refenv m viewenv;
	      let newenv = apply_on_env m (apply_on_env sub accenv) in
	      eval_the_types newenv (accdef@[f,[],exp,i]) (compose m (compose sub accsub)) q
	    end in
      let finalenv,finaldef,finalsub = eval_the_types newenv [] [] deflist in
      let exp,typ,sub = type_expr viewenv finalenv expr in
      (AstLetrec(finaldef,exp,i),typ,compose sub finalsub)
      

let rec type_def viewenv env progt defs = function
    [] -> progt,defs
  | (Deflet(f, l,e,i))::q ->
      begin
	match List.rev l with
	    [] -> 
	      let exp,typ,sub = type_expr viewenv env e in
	      let newenv = apply_on_env sub env in
	      type_def viewenv ((f,generalize newenv typ)::newenv) 
		(progt@[f,generalize newenv typ]) (defs@[Deflet(f,[],exp,i)]) q
	  | s::t -> type_def viewenv env progt defs ((Deflet(f,List.rev t, AstFun(s,e,i),i))::q)
      end
  | (Defletrec(ldef))::q ->
      let rec collect gen= function
	  [] -> []
	| (f,_,_,_)::q ->(f,gen())::(collect gen q) in
      let colldef = (collect (function () -> T (Vari (newvar ()))) ldef) in
      let newenv = colldef@env in
      let rec eval_the_types accenv accdef accsub = function
	  [] -> accdef
	| (f,_,e,i)::q ->
	    let exp,typ,sub = type_expr viewenv accenv e in
	    let m = mgu i (typ,Lens) in
	    begin
	      apply_on_refenv m viewenv;
	      let newenv = apply_on_env m (apply_on_env sub accenv) in
	      eval_the_types newenv (accdef@[f,[],exp,i]) (compose m (compose sub accsub)) q
	    end in
      let finaldef = eval_the_types newenv [] [] ldef in
      let deftypes = collect (function () -> T (Lens)) ldef in
      type_def viewenv (deftypes@env) (progt@deftypes) (defs@[Defletrec(finaldef)]) q

let type_prog (deflist,e) =
  let firstenv = (List.map (function a,b,_ -> a,b) (get_lens_library ())) in
  let viewenv = ref [] in
  let deftype,defs = type_def viewenv firstenv [] [] deflist in
  let exp,typ,_ = type_expr viewenv (deftype@firstenv) e in
  let finalviewenv = List.map (function (a,t) -> (a,inst t)) (!viewenv) in
  (!viewenv,(defs,exp),apply_on_env (finalviewenv) deftype,typ)

let annot_view astv =
  let (v,_,_) = type_expr (ref []) [] astv in
    v
      
(** Substitution of the old variables by fresh and good-looking ones **)

let counter=ref 96

let nicevar () =
  incr counter;
  String.make 1 (char_of_int (!counter)) 


let rec auxcount acc = function
    T (Vari x) -> if List.mem_assoc x acc then acc else (x,Vari (nicevar()))::acc
  | T (Arrow (x,y)) -> auxcount (auxcount acc (T x)) (T y)
  | For_all (_,t) -> auxcount acc t
  | _ -> acc 

let rec count accu = function
      [] -> accu
    | (_,b)::q -> count (auxcount accu b) q

let rec replace_on_type sub = function
    T t -> T (apply sub t)
  | For_all (v,t) -> For_all (v,replace_on_type sub t)

(* Output of the types of the program *)
let nicetypesoftheprog p = 
  counter := 96;
  let viewenv,prog,lt,t = (*finaltype*) type_prog p in
  let sub = count [] lt in
  let sub = auxcount sub (T t) in 
  (viewenv,prog),(List.map (function (a,t) -> (a,replace_on_type sub t)) lt,T (apply sub t))

(* conversion to string of the types of the program *)
let print_the_type (env,t) = 
  print_string (env_to_string env);
  print_string "main : ";
  print_string (type_to_string (projt t));
  print_newline ()

let types_to_string (env,t) =
  (env_to_string env)^
  "main : "^
  (type_to_string (projt t))^
  "\n"

