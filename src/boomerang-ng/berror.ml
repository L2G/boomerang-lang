
(* format a string, converting newlines to @\n *)
let nlify s = Misc.format_list "@\n" 
  (Util.format "%s") 
  (Misc.split_nonescape '\n' s)

let nlify_str s = nlify (Bstring.string_of_t s)

let id x = x

let static_error i n ?(suppl =  id) msg = 
  raise (Error.Harmony_error(fun () -> 
    Util.format "@[%s: static error in@\n" (Info.string_of_t i);
    Util.format "  @["; 
    nlify n;
    Util.format "@]@\n@\n";
    Util.format "  [@["; 
    nlify msg; 
    suppl ();
    Util.format "@]]@\n"))

let type_error i t s1 (s3l,s3r,approx) =
  raise (Error.Harmony_error (fun () -> 
    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
    Util.format "  T=@[%s@]@\n@\n" t;
    Util.format "  @["; 
    nlify s1;
    Util.format "@]@\n@\n";
    Util.format "  [@["; 
    nlify_str s3l; 
    if approx then
      Util.format "@]]@\n<<AROUND HERE>>@\n  [@["
    else
      Util.format "@]]@\n<<HERE>>@\n  [@[";
    nlify_str s3r; 
    Util.format "@]]@]@\n"))

let split_error i t pos nf =
  raise (Error.Harmony_error (fun () -> 
    Util.format "@[%s: type error in@\n" (Info.string_of_t i);
    Util.format "  Cannot find any string in @\nT=@[%s@]@\n@\n" t;
    Util.format "  in the %s file at posistion %d@]" nf pos;))
