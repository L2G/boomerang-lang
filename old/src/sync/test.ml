(* $Id: test.ml,v 1.25 2005/03/11 05:32:03 jnfoster Exp $ *)

(* basic synchronization testing *)

type test_suite = {failures: int; name: string}
 
let current_suite = ref {failures = 0; name = ""}

(* pef stands for Print Endline and Flush *)
let pef s = print_endline s; flush stdout

let begin_test_suite desc =
  begin
    current_suite := {failures = 0; name = desc}; 
    pef ("Test Suite: " ^ !current_suite.name)
  end

let end_test_suite () =
  print_string !current_suite.name;
  let fails = !current_suite.failures in
  if fails = 0 then
    pef (Misc.color " passed all tests" Misc.Green)
  else
    pef (Misc.color
           (" failed " ^ (string_of_int fails)
            ^ " test" ^ (if fails = 1 then "" else "s"))
           Misc.Red)

let dotest desc b =
  Printf.printf "     %-35s" desc;
  flush stdout;
  if b then
    begin
      pef (Misc.color " passed" Misc.Green);
    end
  else
    begin
      pef (Misc.color " failed" Misc.Red);
      current_suite :=
        {!current_suite with failures = !current_suite.failures+1}
    end;
  b
 
(* a basic test of the typed synchronizer *)
let dosynctest desc deltastr cmp = function 
    (`Before (o, a, b), `After (afto, afta, aftb)) ->
      let at = Types.string2abstract_type deltastr in
      let (act,o',a',b') = Sync.sync at o a b in
	if not (dotest desc (cmp act o' afto a' afta b' aftb))
	then
	  begin
	    V.format_msg [`String "o' spec="; `View_opt afto;
			  `String "o' = "; `View_opt o';
			  `String "a' spec="; `View_opt afta;
			  `String "a' = "; `View_opt a';
			  `String "b' spec="; `View_opt aftb;
			  `String "b' = "; `View_opt b'];	    
	    Format.printf "** SYNC ACTION ***\n";
            Sync.format act;
            Format.printf "\n";
            Format.print_flush ()
	  end

(*compare two view options for equality *)
let eqvo vo1 vo2 = 
  match (vo1, vo2) with 
      Some v1, Some v2 -> V.equal v1 v2
    | None, None          -> true
    | _                   -> false
	
(* compare o,a,b against spec *)
let eqcmp _ o' ospec a' aspec b' bspec = 
     (eqvo o' ospec)
  && (eqvo a' aspec)
  && (eqvo b' bspec)

(* compare a,b against spec *)   
let eqabcmp _ _ _ a' aspec b' bspec = (eqvo a' aspec) && (eqvo b' bspec)
    	    
let basic_sync_tests () =
  begin_test_suite "Basic Sync Tests";

  let v_ = V.empty in
  let va = V.from_list [("a",v_)] in
  let vb = V.from_list [("b",v_)] in
  let vab = V.from_list [("a",v_); ("b",v_)] in

  dosynctest 
    "base case"                    (* desc *)	      
    "type X = empty" 		   (* abstract type *) 
    eqcmp			   (* comparison *)    
    (`Before (None, None, None),   (* spec *)          
     `After  (None, None, None));
  dosynctest 
    "one child 1"                         
    "type X = *[X]"                
    eqcmp                                 
    (`Before (Some va, Some va, Some va), 
     `After (Some va, Some va, Some va));
  dosynctest 
    "one child 2"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va, None),
     `After (Some va, Some va, Some va));
  dosynctest 
    "one child 3"
    "type X = *[X]"
    eqcmp
    (`Before (None, None, Some va),
     `After (Some va, Some va, Some va));
  dosynctest 
    "one child 4"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va, Some va),
     `After (Some va, Some va, Some va));
  dosynctest 
    "one child 5"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, None, Some va),
     `After (None, None, None));
  dosynctest 
    "one child 6"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some va, None),
     `After (None, None, None));
  dosynctest 
    "one child 7"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, None, None),
     `After (None, None, None));

  (* two children *)
  dosynctest 
    "two children 1"     
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va, Some vb),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 2"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vb, Some va),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 3"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vab, None),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 4"
    "type X = *[X]"
    eqcmp
    (`Before (None, None, Some vab),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 5"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va, Some vab),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 6"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vb, Some vab),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 7"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vab, Some va),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 8"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vab, Some vb),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 9"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some vab, Some va),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 10"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some va, Some vab),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 11"
    "type X = *[X]"
    eqcmp
    (`Before (Some vb, Some vb, Some vab),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 12"
    "type X = *[X]"
    eqcmp
    (`Before (Some vb, Some vab, Some vb),
     `After (Some vab, Some vab, Some vab));
  dosynctest 
    "two children 13"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vab, Some vab),
     `After (Some vab, Some vab, Some vab));

  (* various deletes *)
  dosynctest 
    "various deletes 1"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some va, Some va),
     `After (Some va, Some va, Some va));
  dosynctest 
    "various deletes 2"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vb, Some vb),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "various deletes 3"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vab, Some va),
     `After (Some va, Some va, Some va));
  dosynctest 
    "various deletes 4"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vab, Some vb),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "various deletes 5"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some va, Some vab),
     `After (Some va, Some va, Some va));
  dosynctest 
    "various deletes 6"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vb, Some vab),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "various deletes 7"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, None, None),
     `After (None, None, None));
  dosynctest 
    "various deletes 8"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, None, Some vab),
     `After (None, None, None));
  dosynctest 
    "various deletes 9"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vab, None),
     `After (None, None, None));
  dosynctest 
    "various deletes 10"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some va, None),
     `After (Some vab, Some va, None)); (* DeleteConflict *)
  dosynctest 
    "various deletes 10.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some va, Some v_),
     `After (Some v_, Some v_, Some v_));
  dosynctest 
    "various deletes 11"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vb, None),
     `After (Some vab, Some vb, None)); (* DeleteConflict *)
  dosynctest 
    "various deletes 11.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vb, Some v_),
     `After (Some v_, Some v_, Some v_));
  dosynctest 
    "various deletes 12"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, None, Some va),
     `After (Some vab, None, Some va)); (* DeleteConflict *)
  dosynctest 
    "various deletes 12.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some v_, Some va),
     `After (Some v_, Some v_, Some v_));
  dosynctest 
    "various deletes 13"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, None, Some vb),
     `After (Some vab, None, Some vb)); (* DeleteConflict *)
  dosynctest 
    "various deletes 13.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some v_, Some vb),
     `After (Some v_, Some v_, Some v_));
  dosynctest 
    "various deletes 14"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some va, Some vb),
     `After (Some v_, Some v_, Some v_));
  dosynctest 
    "various deletes 15"
    "type X = *[X]"
    eqcmp
    (`Before (Some vab, Some vb, Some va),
     `After (Some v_, Some v_, Some v_));

  (* delete and adds *)
  dosynctest 
    "delete and add 1"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, None, Some vab),
     `After (Some va, None, Some vab)); (* DeleteConflict *)
  dosynctest 
    "delete and add 1.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some v_, Some vab),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "delete and add 2"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some vab, None),
     `After (Some va, Some vab, None)); (* DeleteConflict *)
  dosynctest 
    "delete and add 2.1"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some vab, Some v_),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "delete and add 3"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some va, Some vb),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "delete and add 4"
    "type X = *[X]"
    eqcmp
    (`Before (Some va, Some vb, Some va),
     `After (Some vb, Some vb, Some vb));

  end_test_suite ();
  ()


let multi_level_sync_tests () =

  begin_test_suite "Multi-level Sync Tests";

  let v_ = V.empty in
  let va = V.from_list [("a",v_)] in
  let vb = V.from_list [("b",v_)] in
  let vab = V.from_list [("a",v_); ("b",v_)] in
  let v1a = va in
  let v1 = V.new_value "1" in
  let v2 = V.new_value "2" in
  let v3 = V.new_value "3" in
  let va1 = V.from_list [("a",v1)] in
  let va2 = V.from_list [("a",v2)] in
  let vb1 = V.from_list [("b",v1)] in
  let vb2 = V.from_list [("b",v2)] in
  let va1b = V.from_list [("a",v1);("b",v_)] in
  let va2b = V.from_list [("a",v2);("b",v_)] in
  let va3b = V.from_list [("a",v3);("b",v_)] in
  let va1c = V.from_list [("a",v1);("c",v_)] in
  let va2c = V.from_list [("a",v2);("c",v_)] in
  let va3c = V.from_list [("a",v3);("c",v_)] in
  let va3bc = V.from_list [("a",v3);("b",v_);("c",v_)] in
  let va1b1 = V.from_list [("a",v1);("b",v1)] in
  let va1b2 = V.from_list [("a",v1);("b",v2)] in
  let va2b1 = V.from_list [("a",v2);("b",v1)] in
  let va2b2 = V.from_list [("a",v2);("b",v2)] in

  dosynctest 
    "multi-level synchronization 1"
    "type X = *[X]"
    eqcmp
    (`Before (None, None, Some va1),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 2"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va1, None),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 3"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va1, Some va1),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 4"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some va1, Some va1),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 5"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some va2, Some va1),
     `After (Some va2, Some va2, Some va2));
  dosynctest 
    "multi-level synchronization 6"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some va1, Some va2),
     `After (Some va2, Some va2, Some va2));
  dosynctest 
    "multi-level synchronization 7"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some va2, Some va2),
     `After (Some va2, Some va2, Some va2));
  dosynctest 
    "multi-level synchronization 8"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some va1, Some vb),
     `After (Some va1b, Some va1b, Some va1b));
  dosynctest 
    "multi-level synchronization 9"
    "type X = *[X]"
    eqcmp
    (`Before (None, Some vb, Some va1),
     `After (Some va1b, Some va1b, Some va1b));
  dosynctest 
    "multi-level synchronization 10"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some v_, Some vb),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "multi-level synchronization 11"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some vb, Some v_),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "multi-level synchronization 12"
    "type X = *[X]"
    eqcmp
    (`Before (Some vb, Some v_, Some va1),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 13"
    "type X = *[X]"
    eqcmp
    (`Before (Some vb, Some va1, Some v_),
     `After (Some va1, Some va1, Some va1));
  dosynctest 
    "multi-level synchronization 14"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some v_, Some va1b),
     `After (Some vb, Some vb, Some vb));
  dosynctest 
    "multi-level synchronization 15"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1, Some va1b, Some v_),
     `After (Some vb, Some vb, Some vb));

  (* cross-propagation *)
  dosynctest 
    "cross-propagation 1"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1b1, Some va2b1, Some va1b2),
     `After (Some va2b2, Some va2b2, Some va2b2));

  (* delete/modify conflicts *)
  dosynctest 
    "delete/modify conflicts 1"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1b, Some vb, Some va2b),
     `After (Some va1b, Some vb, Some va2b));
  dosynctest 
    "delete/modify conflicts 2"
    "type X = *[X]"
    eqcmp
    (`Before (Some va1b, Some va2b, Some vb),
     `After (Some va1b, Some va2b, Some vb));

  (* maximality: propagations even with conflicts *)
  dosynctest 
    "maximality: propagations even with conflicts 1"
    "type X = *[![{}] | {}]"
    eqcmp
    (`Before (Some va1b, Some va2, Some va3bc),
     `After (Some va1c, Some va2c, Some va3c));  
  
  end_test_suite ();
  ()
    
(* a simple test suite for the all-or-nothing typed synchronizers *)
let typed_sync_tests () = 
  let va = Compiler.compile_view "{a={}}" in

  let typeconflict = 
    (fun act _ _ a' aspec b' bspec ->
       match a',aspec,b',bspec,(Sync.find_conflict act) with 
	 | Some a', Some aspec, Some b', Some bspec, Some act' -> 
	     (* Sync.format act';
	     Format.print_flush ();*)	     
	     (Sync.schema_conflict act')
	     && (V.equal a' aspec) 
	     && (V.equal b' bspec)
	 | _                -> false)
  in      

  
  let _ = begin_test_suite "JNF's Typed Sync Tests" in

  (* SIMPLE UNIT TESTS *)
  let _ = 
    let o,a,b = V.empty, V.empty, V.empty in
    let o',a',b' = o,a,b in
      dosynctest "empty test"
	"type X = {}"
	eqcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  let _ = 
    let va = Compiler.compile_view "{a={}}" in
    let o,a,b = V.empty, V.empty, va in
    let o',a',b' = va, va, va in
      dosynctest 
	"propagate value"
	"type X = *\\{a}[{}] | a[X]"
	eqcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* SIMPLE PHONE BOOK *)
  let _ = 
    let o = Compiler.compile_view "{Pat={Phone={\"000-0000\"}}}" in
    let a = Compiler.compile_view "{Pat={Phone={\"111-2222\"}}}" in
    let b = Compiler.compile_view "{Pat={Phone={\"333-4444\"}}}" in
    let o' = V.empty (* bogus *) in
    let a' = Compiler.compile_view "{Pat={Phone={\"111-2222\" \"333-4444\"}}}" in
    let b' = Compiler.compile_view "{Pat={Phone={\"111-2222\" \"333-4444\"}}}" in
      dosynctest "Pat/Chris/Jo Phone Book - no schema"
	"type any = *[any]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in


  let _ = 
    let o = Compiler.compile_view "{Pat={Phone={\"333-4444\"}} Chris={Phone={\"888-9999\"}}}" in
    let a = Compiler.compile_view "{Pat={Phone={\"111-2222\"}} Chris={Phone={\"888-9999\"}}}" in
    let b = Compiler.compile_view "{Pat={Phone={\"123-4567\"}} Jo={Phone={\"888-9999\"}}}" in
    let o' = V.empty (* bogus *) in
    let a' = Compiler.compile_view "{Pat={Phone={\"111-2222\"}} Jo={Phone={\"888-9999\"}}}" in
    let b' = Compiler.compile_view "{Pat={Phone={\"123-4567\"}} Jo={Phone={\"888-9999\"}}}" in
      dosynctest "Pat/Chris/Jo Phone Book - simple schema"
	"type phonebook = *[Phone[![{}]]]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* TUPLEs/LISTs *)
  let _ = 
    let o = Compiler.compile_view "[o]" in    
    let a = Compiler.compile_view "[a]" in
    let b = Compiler.compile_view "[]" in
    let o' = o (* bogus *) in
    let a' = a in
    let b' = b in
      dosynctest 
	"list schema conflict"
	"type t = \"*h\" [![{}]] . \"*t\" [t] | \"*nil\"[{}]"
	typeconflict
      (`Before (Some o, Some a, Some b),
       `After (Some o', Some a', Some b'))
  in

  let _ = 
    let o = Compiler.compile_view "[o1 o2]" in
    let a = Compiler.compile_view "[a1 a2]" in
    let b = Compiler.compile_view "[o1]" in
    let o' = V.empty (* bogus, but ok since equalab doesn't check o' *) in
    let a' = Compiler.compile_view "[a1 a2]" in
    let b' = Compiler.compile_view "[a1]" in
      dosynctest 
	"list modify head/delete tail"
	"type t = \"*h\" [![{}]] . \"*t\" [t] | \"*nil\"[{}]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  let _ = 
    let o = Compiler.compile_view "[{Pat=333} {Chris=888}]" in
    let a = Compiler.compile_view "[{Chris=123}]" in
    let b = Compiler.compile_view "[{Pat=333} {Chris=888} {Jo=314}]" in
    let o' = V.empty (* bogus *) in
    let a' = Compiler.compile_view "[{Chris=123}]" in
    let b' = Compiler.compile_view "[{Chris=123} {Chris=888} {Jo=314}]" in
      dosynctest 
	"list Pat/Chris/Jo I (simple list type)"
	"type t = \"*h\" [entry] . \"*t\" [t] | \"*nil\"[{}]
         type entry = ![![{}]]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in    

  (* BCP's proposed alternate list schema. Encodes the advanced
     list-schema encoding using atomic *)
  let _ = 
    let o = Compiler.compile_view "{Pat={*h=333 *t={Chris={*h=888 *t=*nil}}}}" in
    let a = Compiler.compile_view "{Chris={*h=123 *t=*nil}}" in
    let b = Compiler.compile_view "{Pat={*h=333 *t={Chris={*h=888 *t={Jo={*h=333 *t=*nil}}}}}}" in
    let o' = V.empty (* bogus *) in
    let a' = a in
    let b' = b in
      dosynctest 
	"list Pat/Chris/Jo II (extended list type)"
 	"type t = emptylist | ! \\ { \"*nil\" } [ \"*h\" [![{}]] . \"*t\"[t]]
         type emptylist = \"*nil\"[{}]"
 	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in    

  (* BOOKMARKs *)
  let bookmarkTypes = 
    "type AContents = ListAItem
     type Val = ![{}]  
     type All = *[All]
     type ALink1 = name[Val].url[Val]
     type ALink  = link[ALink1]
     type AFolder1 = name[Val].contents[AContents]
     type AFolder = folder[AFolder1]
     type AItem = ALink | AFolder
     type ListAItem = \"*h\"[AItem].\"*t\"[ListAItem] | emptylist 
     type emptylist = \"*nil\"[{}]"
  in
  let origBookmarks = 
    Compiler.compile_view
      "[{folder =
          {contents =
            [{link = {name = \"?\"  url =\"http://www.google.com/\"}}
              {folder =
                {contents =
                  [{link = {name = cis  url = \"http://www.cis.upenn.edu/\"}}
                   {link = {name = plclub  url = \"http://www.cis.upenn.edu/proj/plclub/\"}}
                   {link = {name = penn  url = \"http://www.upenn.edu/\"}}
                   {link = {name = pennportal  url = \"http://www.upenn.edu/penn_portal/view.php\"}}
                   {link = {name = benjamin  url = \"http://www.cis.upenn.edu/~bcpierce/\"}}
                   {link = {name = webmail  url = \"https://webmail.seas.upenn.edu/\"}}]
                 name = penn}}
              {folder =
                {contents =
                  [{link = {name = cambridge  url = \"http://www.cam.ac.uk/\"}}
                   {link = {name = emma  url = \"http://www.emma.cam.ac.uk/\"}}
                   {link = {name = cuvc  url = \"http://www.srcf.ucam.org/cuvc/index.php\"}}
                   {link = {name = cvc  url = \"http://www.cambridgevolleyball.org/\"}}] 
                 name = cam}}]
           name = \"Nate's Bookmarks\"}}]"
    in
  let _ = 
    let o = origBookmarks in
    let a = Compiler.compile_view
	"[{folder =
            {contents =
              [{link = {name = \"google\"  url =\"http://www.google.com/\"}}
                {folder =
                  {contents =
                    [{link = {name = cis  url = \"http://www.cis.upenn.edu/\"}}
                     {link = {name = plclub  url = \"http://www.cis.upenn.edu/proj/plclub/\"}}
                     {link = {name = penn  url = \"http://www.upenn.edu/\"}}
                     {link = {name = pennportal  url = \"http://www.upenn.edu/penn_portal/view.php\"}}
                     {link = {name = benjamin  url = \"http://www.cis.upenn.edu/~bcpierce/\"}}
                     {link = {name = webmail  url = \"https://webmail.seas.upenn.edu/\"}}]
                  name = penn}}]
          name = \"Nate's Bookmarks\"}}]"
    in
    let b = Compiler.compile_view
	"[{folder =
            {contents =
              [{link = {name = \"?\"  url =\"http://www.google.com/\"}}
                {folder =
                  {contents =
                    [{link = {name = cis  url = \"http://www.cis.upenn.edu/\"}}
                     {link = {name = plclub  url = \"http://www.cis.upenn.edu/proj/plclub/\"}}
                     {link = {name = penn  url = \"http://www.upenn.edu/\"}}]
                  name = penn}}
                {folder =
                  {contents =
                    [{link = {name = cambridge  url = \"http://www.cam.ac.uk/\"}}
                     {link = {name = emma  url = \"http://www.emma.cam.ac.uk/\"}}
                     {link = {name = cuvc  url = \"http://www.srcf.ucam.org/cuvc/index.php\"}}
                     {link = {name = cvc  url = \"http://www.cambridgevolleyball.org/\"}}] 
                  name = cam}}]
          name = \"Nate Foster's Bookmarks\"}}]"
    in
    let o' = Compiler.compile_view
	"[{folder =
            {contents =
              [{link = {name = \"google\"  url =\"http://www.google.com/\"}}
                {folder =
                  {contents =
                    [{link = {name = cis  url = \"http://www.cis.upenn.edu/\"}}
                     {link = {name = plclub  url = \"http://www.cis.upenn.edu/proj/plclub/\"}}
                     {link = {name = penn  url = \"http://www.upenn.edu/\"}}]
                  name = penn}}]
          name = \"Nate Foster's Bookmarks\"}}]"
    in
    let a' = o' in 
    let b' = o' in
      dosynctest 
	"bookmarks"
	bookmarkTypes
        eqcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* CALENDARs *)
  let calendarTypes =
    "type CAL = *[ENTRY]
     type VAL = ![{}]
     type ENTRY = date[VAL].desc[VAL].time[begin[VAL].end[VAL]]"
  in
  let origCalendar = 
    Compiler.compile_view 
      "{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD deadline\" 
          time = {begin = 23:30  end = 00:00}} 
        20041114T032117Z-25528-1000-1-1@doobies =
         {date = 11-17-2004 
          desc = \"CIS 500 Exam\" 
          time = {begin = 12:00  end = 12:30}} 
        20041114T032122Z-25528-1000-1-2@doobies =
         {date = 11-16-2004 
          desc = \"JGM student meeting\" 
          time = {begin = 11:00  end = 11:30}} 
        20041114T032142Z-25528-1000-1-5@doobies =
         {date = 11-17-2004 
          desc = \"Volleyball (Pigs of Philly)\" 
          time = {begin = 20:00  end = 21:30}} 
        20041114T032231Z-25528-1000-1-7@doobies =
         {date = 11-05-2004 
          desc = \"POPL camera ready\" 
          time = {begin = 23:30  end = 00:00}}
      }"
  in

  let _ = 
    let o = 
      Compiler.compile_view
	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD deadline\" 
          time = {begin = 23:30  end = 00:00}}}"
    in
    let a = 
      Compiler.compile_view
	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD deadline\" 
          time = {begin = 23:59  end = 00:00}}}"
    in
    let b = V.empty in
    let o' = V.empty (* bogus *) in
    let a' = a in
    let b' = b in
      dosynctest 
	"iCalendar appointements #1"
	calendarTypes
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  let _ = 
    let o = origCalendar in
    let a = 
      Compiler.compile_view
      	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD submission deadline\" 
          time = {begin = 23:30  end = 00:00}} 
        20041114T032122Z-25528-1000-1-2@doobies =
         {date = 11-16-2004 
          desc = \"JGM student meeting\" 
          time = {begin = 11:00  end = 11:30}} 
        20041114T032142Z-25528-1000-1-5@doobies =
         {date = 11-17-2004 
          desc = \"Volleyball (Pigs of Philly) @ Hutch\" 
          time = {begin = 20:00  end = 21:30}}
        20041114TBOGUS-1000-1-13@doobies = 
         {date = 11-13-2004
          desc = \"JKF 50th Birthday\"
          time = {begin= 00:01  end = 23:59}}}"
    in
    let b =
      Compiler.compile_view
      	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD deadline\" 
          time = {begin = 23:59  end = 00:00}} 
        20041114T032117Z-25528-1000-1-1@doobies =
         {date = 11-17-2004 
          desc = \"CIS 500 Exam\" 
          time = {begin = 12:00  end = 13:30}} 
        20041114T032122Z-25528-1000-1-2@doobies =
         {date = 11-16-2004 
          desc = \"JGM student meeting\" 
          time = {begin = 11:00  end = 12:00}} 
        20041114T032142Z-25528-1000-1-5@doobies =
         {date = 11-17-2004 
          desc = \"Volleyball (Pigs of Philly)\" 
          time = {begin = 20:00  end = 21:30}} 
        20041114T032231Z-25528-1000-1-7@doobies =
         {date = 11-05-2004 
          desc = \"POPL camera ready deadline\" 
          time = {begin = 23:59  end = 00:00}}}"
    in      
    let o' = V.empty in (* bogus *)
    let a' =
      Compiler.compile_view
      	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD submission deadline\" 
          time = {begin = 23:59  end = 00:00}} 
        20041114T032122Z-25528-1000-1-2@doobies =
         {date = 11-16-2004 
          desc = \"JGM student meeting\" 
          time = {begin = 11:00  end = 12:00}} 
        20041114T032142Z-25528-1000-1-5@doobies =
         {date = 11-17-2004 
          desc = \"Volleyball (Pigs of Philly) @ Hutch\" 
          time = {begin = 20:00  end = 21:30}}
        20041114TBOGUS-1000-1-13@doobies = 
         {date = 11-13-2004
          desc = \"JKF 50th Birthday\"
          time = {begin= 00:01  end = 23:59}}}"
    in
    let b' =
      Compiler.compile_view
      	"{20041114T032112Z-25528-1000-1-0@doobies =
         {date = 11-17-2004 
          desc = \"SIGMOD submission deadline\" 
          time = {begin = 23:59  end = 00:00}} 
        20041114T032117Z-25528-1000-1-1@doobies =
         {date = 11-17-2004 
          desc = \"CIS 500 Exam\" 
          time = {begin = 12:00  end = 13:30}} 
        20041114T032122Z-25528-1000-1-2@doobies =
         {date = 11-16-2004 
          desc = \"JGM student meeting\" 
          time = {begin = 11:00  end = 12:00}} 
        20041114T032142Z-25528-1000-1-5@doobies =
         {date = 11-17-2004 
          desc = \"Volleyball (Pigs of Philly) @ Hutch\" 
          time = {begin = 20:00  end = 21:30}} 
        20041114T032231Z-25528-1000-1-7@doobies =
         {date = 11-05-2004 
          desc = \"POPL camera ready deadline\" 
          time = {begin = 23:59  end = 00:00}}
        20041114TBOGUS-1000-1-13@doobies = 
         {date = 11-13-2004
          desc = \"JKF 50th Birthday\"
          time = {begin= 00:01  end = 23:59}}}"
    in
      dosynctest 
	"iCalendar appointements #2"
	calendarTypes        
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in      

  (* ADDRBOOK SCHEMA 
     -loosely- based on vcard, xcard 
  *)
  let contacts = 
    "type CONTACTS   = *[ ENTRY ]

     (* collections of values *)
     type EV         = {}
     type VALUE      = ![EV]
     type VALUE-SET  = *[EV]
     type VALUE-PSET = pref[VALUE].alts?[VALUE-SET]
     type VALUE-LIST = \"*h\"[VALUE].\"*t\"[VALUE-LIST] | \"*nil\"[EV]

     (* top level structure *)
     type ENTRY      = name[NAME].INFO
     type INFO       =   PROFTEL.org[ORG].addr[ADDR].OPTINFO 
                       | PERSTEL.org?[ORG].addr?[ADDR].OPTINFO
     type PROFTEL    = work[VALUE].home?[VALUE]
     type PERSTEL    = work?[VALUE].home[VALUE]
     type OPTINFO    = email?[EMAIL]   (* .url?[VALUE].fax?[VALUE].cell?[VALUE] *)

     (* one level down structure *)
     type NAME       = first[VALUE].last[VALUE].title?[VALUE].prefix?[VALUE-LIST].other?[VALUE-LIST].suffix?[VALUE-LIST].nickname?[VALUE-SET]
     type ORG        = name[VALUE] | name[VALUE].unit[VALUE]
     type EMAIL      = !\\{pref alts}[EV] | VALUE-PSET
     type ADDR       = street[VALUE].locality?[VALUE].region?[VALUE].pcode?[VALUE].country?[VALUE]"
  in
  (* first test: showing things that go wrong if we use no schema *)
  let _ = 
    let o = Compiler.compile_view
      "{cele=
         {name={first=Cristin last=Ellis other=[Elizabeth Linsley]}
          email={pref=\"ccc@wso.williams.edu\" alts=\"ccc@jhu.edu\"}
          home=\"617-xxx-yyyy\"
          work=\"410-www-zzzz\"
          org= {name=\"The Johns Hopkins University\" unit=\"English Department\"}
         }
       }"
    in
    let a = Compiler.compile_view 
      "{cele= 
         {name={first=Cristin last=Ellis other=[Elizabeth]}
          email={pref=\"ccc@jhu.edu\"} 
          home=\"617-xxx-yyyy\"
         }
       }"
    in
    let b = Compiler.compile_view
       "{cele=
          {name={first=Cristin last=Ellis other=[]}
           email={pref=\"ccc@williams.edu\" alts=\"ccc@jhu.edu\"}
           work=\"410-www-zzzz\"
           org={name=\"The Johns Hopkins University\" unit=\"English Department\"}
          }
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = Compiler.compile_view 
      "{cele=
         {name={first=Cristin last=Ellis other={\"*nil\"={} \"*t\"=[]}}
          email={pref={\"ccc@williams.edu\" \"ccc@jhu.edu\"}}
         }}"
    in
    let b' = Compiler.compile_view
      "{cele=
         {name={first=Cristin last=Ellis other=[]}
          email={pref={\"ccc@williams.edu\" \"ccc@jhu.edu\"}}

       }}"
    in      
      dosynctest 
	"addrbook no schema"
        "type Any=*[Any]"	
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* first test: jnf entry starts off with org, home, work one side
     makes it a PERS entry, the other a PROF entry *)
  let _ = 
    let o = Compiler.compile_view 
      "{jnf = {name={first=Nathan last=Foster} 
               org={name=\"University of Pennsylvania\" 
                    unit=\"Computer and Information Science\"}
               home=\"215-893-0474\" 
               work=\"215-573-2580\" 
              }
       }"
    in 
    let a = Compiler.compile_view
      "{jnf = {name={first=Nathan last=Foster}
               home=\"734-741-4271\"
              }
       }"
    in
    let b = Compiler.compile_view
      "{jnf = {name={first=Nathan last=Foster}
               org={name=\"University of Pennsylvania\" 
                    unit=\"Computer and Information Science\"}
               work=\"257-342-1099\"
               addr={street={\"3330 Walnut\"}}
              }
       }"
    in
      
    let o' = o (* BOGUS *) in
    let a' = a in
    let b' = b in
      dosynctest 
	"addrbook schema conflict #1"
        contacts	
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (*** examples from VLDB submission ***)

  (* the ADR schema*)
  let adr = 
    "type C = name[N].work[V].home?[V].org[O].email[E]
            | name[N].work?[V].home[V].org?[O].email[E]
     type N = first[V].last[V].other?[VL]
     type E = !\\{pref alts}[{}] | pref[V].alts[VS]
     type O = orgname[V].orgunit[V]
     type V = ![{}]
     type VS = *[{}]
     type VL = \"*h\"[V].\"*t\"[VL] | \"*nil\"[{}]"
  in
    
  (* original ADR tree *)
  let orig = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
  in

  (*** PAPER EXAMPLE #1 ***)
  (*   a removes work, org edges *)
  (*   b removes home *)
  (*   w/o schema --> invalid entry: doesn't have home or work *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
      "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
        }"
    in
    let b = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o in (* BOGUS *)
    let a' = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}        
        }"
    in
    let b' = a' in
    let _ = dosynctest 
      "addrbook #1 : Any : no phone at top-level"
      "type Any = *[Any]"	
      eqabcmp
      (`Before (Some o, Some a, Some b),
       `After (Some o', Some a', Some b'))
    in      
      dosynctest 
	"addrbook #1 : ADR : schema conflict"
	adr	
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o, Some a, Some b))
  in      
  (* PAPER EXAMPLE #2 *)
  (* names *)
  (*   a renames Meg -> Maggie and clips out Liz and Jo *)
  (*   b renames Meg -> Megan and removes Jo*)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Maggie} other=[] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
      "{name={first={Megan} other=[Liz] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = Compiler.compile_view 
       "{name={first={Maggie Megan} other=[] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b' = Compiler.compile_view 
      "{name={first={Maggie Megan} other={\"*nil\" \"*t\"=[]} last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let _ = 
      dosynctest 
	"addrbook #2 : Any : two first names, mangled other child"
        "type Any=*[Any]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
    in
      dosynctest 
	"addrbook #2 : ADR : schema conflicts on name and other"
        adr
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a, Some b))      
  in

  (* PAPER EXAMPLE #2a *)
  (* names *)
  (*   a renames Liz -> Elizabeth *)
  (*   b renames Jo -> Joanna *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Elizabeth Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Joanna] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = Compiler.compile_view 
       "{name={first={Meg} other=[Elizabeth Joanna] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b' = a' in
      dosynctest 
	"addrbook #2a : ADR : change both names in other"
        adr
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* PAPER EXAMPLE #2b *)
  (* names *)
  (*   a adds Mary to front *)
  (*   b adds Claude to front *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Mary Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
       "{name={first={Meg} other=[Claude Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = a in
    let b' = b in
      dosynctest 
	"addrbook #2b : ADR : add two names to front"
        adr
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

  (* PAPER EXAMPLE #2c *)
  (* names *)
  (*   a removes Liz *)
  (*   b renames Jo to Joanna *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Joanna] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = a in
    let b' = Compiler.compile_view 
      "{name={first={Meg} other=[Jo Joanna] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
      dosynctest 
	"addrbook #2c : ADR : delete Liz, rename Jo"
        adr
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  (* PAPER EXAMPLE #2d *)
  (* names *)
  (*   a adds Mary *)
  (*   b renames Jo to Joanna *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Mary Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Joanna] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = a in
    let b' = Compiler.compile_view 
      "{name={first={Meg} other=[Mary Joanna Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
      dosynctest 
	"addrbook #2d : ADR : add Mary, rename Jo"
        adr
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in


  (* PAPER EXAMPLE #3 *)
  (* email addresses *)
  (*   a gives Meg a single address  *)
  (*   b changes pref and alts *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={\"meg@smith.com\"}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
      "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"meg.smith@cs.city.edu\"} alts={\"msmith@city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = o (* BOGUS *) in
    let a' = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={\"meg@smith.com\"}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b' = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={\"meg@smith.com\" pref={\"meg.smith@cs.city.edu\"} alts={\"msmith@city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let _ = 
      dosynctest 
	"addrbook #3 : Any : mangled email contacts"
        "type Any=*[Any]"
	eqabcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
    in
      dosynctest 
	"addrbook #3 : ADR : schema conflict on email"
        adr
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a, Some b))      
  in
  (* PAPER EXAMPLE #3a *)
  (* email addresses *)
  (*   a and b both add addresses  *)
  let _ = 
    let o = orig in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\" \"maggie@gmail.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
      "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\" \"meg.smith@cs.city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\" 
                                                 \"maggie@gmail.com\"
                                                 \"meg.smith@cs.city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let a' = o' in
    let b' = o' in
      dosynctest 
	"addrbook #3a : ADR : add multiple addresses to alts"
        adr
	eqcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  (* PAPER EXAMPLE #3b *)
  (* email addresses *)
  (*   a and b both add/remove addresses  *)
  let _ = 
    let o = Compiler.compile_view
      "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\" \"maggie@gmail.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let a = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"megan.smith@pobox.city.edu\" 
                                                 \"maggie@gmail.com\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let b = Compiler.compile_view 
      "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"meg@smith.com\" 
                                                 \"meg.smith@cs.city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let o' = Compiler.compile_view 
       "{name={first={Meg} other=[Liz Jo] last=Smith}         
         email={pref={\"msmith@city.edu\"} alts={\"megan.smith@pobox.city.edu\" 
                                                 \"meg.smith@cs.city.edu\"}}
         home=\"555-6666\"
         work=\"555-7777\"
         org={orgname=\"City University\" orgunit=\"Dept of CS\"}
        }"
    in
    let a' = o' in
    let b' = o' in
      dosynctest 
	"addrbook #3b : ADR : add/remove addresses in alts"
        adr
	eqcmp
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  let kvl = "type KVL = !\\{nil}[head[{}].tail[KVL]] | nil[{}]" in
  let _ = 
    let o = Compiler.compile_view
      "{Liz={head={} tail={Jo={head={} tail=nil}}}}" 
    in
    let a = Compiler.compile_view 
      "{Jo={head={} tail=nil}} "
    in
    let b = Compiler.compile_view 
      "{Liz={head={} tail={Joanna={head={} tail=nil}}}}" 
    in
    let o' = o in (* BOGUS *)
    let a' = a in
    let b' = b in
      dosynctest 
	"addrbook #4a : KVL : key'd list "
	kvl
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in
  let _ = 
    let o = Compiler.compile_view
      "{Liz={head={} tail={Jo={head={} tail=nil}}}}" 
    in
    let a = Compiler.compile_view 
      "{Mary={head={} tail={Liz={head={} tail={Jo={head={} tail=nil}}}}}}"
    in
    let b = Compiler.compile_view 
      "{Liz={head={} tail={Joanna={head={} tail=nil}}}}" 
    in
    let o' = o in (* BOGUS *)
    let a' = a in
    let b' = b in
      dosynctest 
	"addrbook #4b : KVL : key'd list "
	kvl
	typeconflict
	(`Before (Some o, Some a, Some b),
	 `After (Some o', Some a', Some b'))
  in

    end_test_suite ()
      
let do_tests () = 
  basic_sync_tests ();
  multi_level_sync_tests ();
  typed_sync_tests ();
