%{

  open Lexing
    
  let parse_error _ =
    let pos1,pos2 = Parsing.symbol_start_pos (),Parsing.symbol_end_pos () in
    let l1,l2 = pos1.pos_lnum,pos2.pos_lnum in
    failwith  (Printf.sprintf "parse error at characters %d-%d lines %d-%d"
		 (Parsing.symbol_start ()) (Parsing.symbol_end ()) l1 l2 )

(*
    let s =
      failwith (  Printf.sprintf "parse error at characters %d-%d lines %d-%d"
		    (Parsing.symbol_start ()) (Parsing.symbol_end ()) l1 l2)  in
    failwith s
 *)

  let vallist pl l = 
    V.set (
      V.set V.empty  "DATA" (Some (V.structure_from_list (List.map V.new_value l)))
    )
      "PARAM" (Some (V.from_list (List.map (function x -> (x,V.empty)) pl)))
%}

%token BEGINVCARD ENDVCARD
%token LOGO PHOTO LABEL FN TITLE SOUND VERSION 
%token TEL EMAIL TZ GEO NOTE URL BDAY ROLE REV
%token UID KEY MAILER ADR ORG N NICKNAME
%token <string> X PARAM ITEM
%token <string list> VAL
%token AGENT EOF


%type <V.t> vcard

%start vcard

%%

  vcard : 
  vcardlist             { V.structure_from_list $1 }
  ;
  
  vcardlist :
    BEGINVCARD itemlist ENDVCARD vcardlist { (V.set V.empty "VCARD" (Some (V.structure_from_list $2)))::($4) }
  | EOF                                    { [] }
  |                                        { [] }
  ;
  
  itemlist :
    itemlist item                 { $1 @ [$2] }
  |                               { [] }
  ;
  
  item :
    ITEM pl valist                   {V.set V.empty ($1) (Some (vallist $2 $3)) }
/*
    LOGO pl valist                   {V.set V.empty ("LOGO") (Some (vallist $2 $3)) }
  | PHOTO pl valist                  {V.set V.empty ("PHOTO") (Some (vallist $2 $3)) }
  | LABEL pl valist                  {V.set V.empty ("LABEL") (Some (vallist $2 $3)) }
  | FN pl valist                     {V.set V.empty ("FN") (Some (vallist $2 $3)) }
  | TITLE pl valist                  {V.set V.empty ("TITLE") (Some (vallist $2 $3)) }
  | SOUND pl valist                  {V.set V.empty ("SOUND") (Some (vallist $2 $3)) }
  | VERSION pl valist                {V.set V.empty ("VERSION") (Some (vallist $2 $3)) }
  | TEL pl valist                    {V.set V.empty ("TEL") (Some (vallist $2 $3)) }
  | EMAIL pl valist                  {V.set V.empty ("EMAIL") (Some (vallist $2 $3)) }
  | TZ pl valist                     {V.set V.empty ("TZ") (Some (vallist $2 $3)) }
  | GEO pl valist                    {V.set V.empty ("GEO") (Some (vallist $2 $3)) }
  | NOTE pl valist                   {V.set V.empty ("NOTE") (Some (vallist $2 $3)) }
  | URL pl valist                    {V.set V.empty ("URL") (Some (vallist $2 $3)) }
  | BDAY pl valist                   {V.set V.empty ("BDAY") (Some (vallist $2 $3)) }
  | ROLE pl valist                   {V.set V.empty ("ROLE") (Some (vallist $2 $3)) }
  | REV pl valist                    {V.set V.empty ("REV") (Some (vallist $2 $3)) }
  | UID pl valist                    {V.set V.empty ("UID") (Some (vallist $2 $3)) }
  | KEY pl valist                    {V.set V.empty ("KEY") (Some (vallist $2 $3)) }
  | MAILER pl valist                 {V.set V.empty ("MAILER") (Some (vallist $2 $3)) }
  | ADR pl valist                    {V.set V.empty ("ADR") (Some (vallist $2 $3)) }
  | ORG pl valist                    {V.set V.empty ("ORG") (Some (vallist $2 $3)) }
  | N pl valist                      {V.set V.empty ("N") (Some (vallist $2 $3)) }
  | NICKNAME pl valist               {V.set V.empty ("NICKNAME") (Some (vallist $2 $3)) }
  | AGENT pl valist                  {V.set V.empty ("AGENT") (Some (vallist $2 $3)) }
  | X pl valist                      {V.set V.empty (("X-"^($1))) (Some (vallist $2 $3)) }
*/ 
 ;

  pl :
  |                            { [] }
  | PARAM pl                   { if $1 = "TYPE" then $2 else ($1::($2)) }  
  ;
  
  valist :
  | VAL                        { $1 }
  | VAL valist                 { $1@$2 }
