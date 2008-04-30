open Bsyntax

let msg = Util.format

let name_ctxt : (int * ((int * string) list)) ref = ref (0,[])
let reset_name_ctxt () = 
  name_ctxt := (0,[])

let format_svar_tag x = 
  let i,assoc = !name_ctxt in 
  let x_str = 
    try Safelist.assoc x assoc
    with Not_found -> 
      let chr = Char.chr (97 + i mod 26) in 
      let sub = i / 26 in 
      let x_str = 
        if sub=0 then Printf.sprintf "'%c" chr
        else Printf.sprintf "'%c_%d" chr sub in 
        name_ctxt := (succ i,(x,x_str)::assoc);
        x_str in     
    Util.format "%s" x_str
      (* Util.format "x%d" x *)

let rec format_sort = function
  | SUnit -> Util.format "unit"
      
  | SString -> Util.format "string"

  | SInteger -> Util.format "int"
      
  | SRegexp -> Util.format "regexp"
      
  | SLens -> Util.format "lens"
      
  | SCanonizer -> Util.format "canonizer"
      
  | SFunction(x0, s1, s2) ->
      Util.format "@[(";
      if not (Id.equal x0 Id.wild)
      then Util.format "%s:@," (Id.string_of_t x0);
      format_sort s1;
      Util.format "@ ->@ ";
      format_sort s2;
      Util.format ")@]"
        
  | SProduct(s1,s2) -> 
      Util.format "@[<2>";
      format_sort s1;
      Util.format "@ *@ ";
      format_sort s2;
      Util.format "@]"
  | SData([],q1) -> Util.format "@[%s@]" (Qid.string_of_t q1)
  | SData([s],q1) -> 
      msg "@[";
      format_sort s;
      msg "@ ";
      msg "%s@]" (Qid.string_of_t q1)
  | SData(ms,q1) ->         
      Util.format "@[(@[<2>";
      Misc.format_list ",@ " (format_sort) ms;      
      Util.format "@])@ %s@]" (Qid.string_of_t q1)
  | SVar(sv) -> format_svar false sv
  | SRawVar(x) -> Util.format "~%s" (Id.string_of_t x)
  | SRefine(x0,s0,e0) ->
      Util.format "@[(";
      if not (Id.equal x0 Id.wild)
      then Util.format "%s:@," (Id.string_of_t x0);
      format_sort s0;
      Util.format "@ |@ ";
      format_exp e0; (* TODO detect e0 == true *)
      Util.format ")@]"

and format_svar print_cons (x,cr) = 
  (* msg "<%d>:" x; *)
  match !cr with
  | Bnd s -> 
      format_sort s
  | Fre   -> 
      format_svar_tag x
  | Con c -> 
      msg "@[";
      if print_cons then 
        (msg "{@[<2>";
         Misc.format_list "," format_base_sort (BSSet.elements c);
         msg "@]}@ ");
      format_svar_tag x;
      msg "@]";
 
and format_base_sort = function
  | Unt -> Util.format "unit"
  | Str -> Util.format "string"
  | Int -> Util.format "int"
  | Reg -> Util.format "regexp"
  | Lns -> Util.format "lens"
  | Can -> Util.format "canonizer"

and format_scheme (svl,s) = 
  Util.format "@[<2>";
  let con_fsvs = 
    Safelist.fold_left 
      (fun l ((_,cr) as svi) -> match !cr with | Con _ -> svi::l | _ -> l)
      [] svl in 
  if Safelist.length con_fsvs <> 0 then
    (Misc.format_list ", " (format_svar true) con_fsvs;
     Util.format " => ");
  format_sort s;
  Util.format "@]"

and format_pat p0 = match p0.desc with 
  | PWld -> Util.format "_"
  | PUnt -> Util.format "()"
  | PVar(x) -> Util.format "%s" (Id.string_of_t x)
  | PPar(p1,p2) -> 
      Util.format "@[<2>(";
      format_pat p1;
      Util.format ",@,";
      format_pat p2;
      Util.format ")@]";
  | PVnt(l,None) -> Util.format "%s" (Qid.string_of_t l)
  | PVnt(l,Some p1) ->  
      Util.format "@[<2>(%s@ " (Qid.string_of_t l);
      format_pat p1;
      Util.format ")@]"

and format_param = function
  | Param(_,x,s) ->
      Util.format "@[(%s:" (Id.string_of_t x);
      format_sort s;
      Util.format ")@]"

and format_binding (Bind (_, x, e)) =
  Util.format "@[<2>";
  format_pat x;
  Util.format "@ =@ ";
  format_exp e;
  Util.format "@]"

and format_exp e0 = match e0.desc with 
  | EApp (e1,e2) ->
	Util.format "@[<2>(";
	format_exp e1;
	Util.format "@ ";
	format_exp e2;
	Util.format ")@]"

    | EVar(q) -> 
	Util.format "@[%s@]" (Qid.string_of_t q)

    | EFun (p,s,e) ->
	Util.format "@[<2>(fun@ ";
	format_param p;
	(match s with
	   | None -> ()
           | Some s -> Util.format "@ :@ "; format_sort s);
	Util.format "@ ->@ ";
	format_exp e;
	Util.format ")@]";

    | ETyFun(sv,e) ->
        Util.format "@[<2>(tyfun@ ";
        format_svar false sv;
        Util.format "@ . @ ";
        format_exp e;
        Util.format ")@]";               

    | ETyApp(e,s) ->
        Util.format "@[<2>(@ ";
        format_exp e;
        Util.format "@ [@ "; 
        format_sort s;
        Util.format "])@]"

    | ELet (b,e) ->
	Util.format "@[<2>let@ ";
	format_binding b;
	Util.format "@ in@ ";
	format_exp e;
	Util.format "@]";

    | EUnit -> Util.format "()"

    | EPair(e1,e2) -> 
        Util.format "@[<2>(";
        format_exp e1;
        Util.format ",";
        format_exp e2;
        Util.format ")@]"

    | ECase(e1,pl) -> 
        Util.format "@[<2>match@ ";
        format_exp e1;
        Util.format "@ with@ ";
        Misc.format_list "@ |@ "
          (fun (p,e) -> 
             Util.format "@[<2>@ ";
             format_pat p;
             Util.format "@ ->@ ";
             format_exp e;
             Util.format "@]")
          pl;
        Util.format "@]"

    | EString (s) ->
	Util.format "@[\"%s\"@]" (Bstring.escaped (Bstring.string_of_t s))


    | EInteger (i) ->
	Util.format "@[%d@]" i

    | ECSet (negated, ranges) ->
	Util.format "@[[";
	(if negated
	 then Util.format "^"
	 else ());
	Misc.format_list ""
	  (fun (first, last) ->
	     if Bstring.compare_sym first last = 0
	     then Util.format "%s" (Bstring.escaped_repr first)
	     else Util.format "%s-%s" 
	       (Bstring.escaped_repr first)
	       (Bstring.escaped_repr last))
	  ranges;
	  Util.format "]@]"

and format_test_result tr =
  match tr with
    | TestValue e -> 
        Util.format "= @[<2>" ; 
        format_exp e;
        Util.format "@]";
    | TestError -> Util.format "= @[<2>error@]"
    | TestShow -> Util.format "= @[<2>?@]"
    | TestSort(None) -> Util.format ": @[<2>?@]"
    | TestSort(Some s) -> 
        Util.format ": @[<2>";
        format_sort s;
        Util.format "@]"
    | TestLensType(e1o,e2o) -> 
        let format_eo = function
          | None -> Util.format "?"
          | Some e1 -> format_exp e1 in 
        Util.format ": @[<2>";
        format_eo e1o;
        Util.format " <-> ";
        format_eo e2o;
        Util.format "@]"

and format_decl d0 =
  match d0.desc with
      DLet (b) ->
	Util.format "@[let@ ";
	format_binding b;
	Util.format "@]"

    | DType(svl,x,cl) ->  
        Util.format "@[type@ ";
        (match svl with 
          | [] -> ()
          | [sv] -> format_sort sv
          | _ -> 
              msg "(";
              Misc.format_list ",@ " format_sort svl;
              msg ")@ ");
        msg "%s@ =@ " (Qid.string_of_t x);
        Misc.format_list " | "
          (fun (l,s) -> match s with
             | None -> Util.format "%s" (Id.string_of_t l)
             | Some s -> 
                 Util.format "(%s@ " (Id.string_of_t l);
                 format_sort s;
                 Util.format ")")
          cl;
        Util.format "@]"        

    | DMod (m, ds) ->
	format_module (mk_mod d0.info (Mod (m, [], ds)))

    | DTest (e, tr) ->
	Util.format"@[<2>test@ @[";
	format_exp e;
	Util.format "@ =@ ";
	format_test_result tr;
	Util.format "@]@]"
and format_module m0 = match m0.desc with 
  | Mod (m, qs, ds) ->
      Util.format "@[module %s =@\n  @[" (Id.string_of_t m);
      if qs <> [] then 
        Misc.format_list "@\n" 
          (fun x -> Util.format "open %s" (Id.string_of_t x))
          qs;
      Misc.format_list "@\n" format_decl ds;
      Util.format "@\n@]@\n@]"

(* string_of_sort : s -> string
 *
 * [string_of_sort s] produces a string representing [s] 
 *)
let string_of_sort s = Util.format_to_string (fun () -> format_sort s)
let string_of_scheme s = Util.format_to_string (fun () -> format_scheme s)
let string_of_param p = Util.format_to_string (fun () -> format_param p)
let string_of_pat p = Util.format_to_string (fun () -> format_pat p)
