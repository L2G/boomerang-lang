{

open Printf
exception Error of string

let to_string = function
  | None -> ""
  | Some s -> sprintf "%s" s

} 

let blank = [' ''\t''\n']*
let str = [^'<''>']*
let nonambigous = [^';']*

    rule decompose = parse
| blank "<" ("HOME" as arg) ">" blank "</HOME>"
| blank "<" ("HOME" as arg) "/>"
| blank "<" ("WORK" as arg) ">" blank "</WORK>"
| blank "<" ("WORK" as arg) "/>"
| blank "<" ("POSTAL" as arg) ">" blank "</POSTAL>"
| blank "<" ("POSTAL" as arg) "/>"
| blank "<" ("PARCEL" as arg) ">" blank "</PARCEL>"
| blank "<" ("PARCEL" as arg) "/>"
| blank "<" ("DOM" as arg) ">" blank "</DOM>"
| blank "<" ("DOM" as arg) "/>"
| blank "<" ("INTL" as arg) ">"  blank "</INTL>"
| blank "<" ("INTL" as arg) "/>"
| blank "<" ("PREF" as arg) ">" blank "</PREF>"
| blank "<" ("PREF" as arg) "/>"
| blank "<" ("VOICE" as arg) ">" blank "</VOICE>"
| blank "<" ("VOICE" as arg) "/>"
| blank "<" ("FAX" as arg) ">" blank "</FAX>"
| blank "<" ("FAX" as arg) "/>"
| blank "<" ("PAGER" as arg) ">" blank "</PAGER>"
| blank "<" ("PAGER" as arg) "/>"
| blank "<" ("MSG" as arg) ">" blank "</MSG>"
| blank "<" ("MSG" as arg) "/>"
| blank "<" ("CELL" as arg) ">" blank "</CELL>"
| blank "<" ("CELL" as arg) "/>"
| blank "<" ("VIDEO" as arg) ">" blank "</VIDEO>"
| blank "<" ("VIDEO" as arg) "/>"
| blank "<" ("BBS" as arg) ">" blank "</BBS>"
| blank "<" ("BBS" as arg) "/>"
| blank "<" ("MODEM" as arg) ">" blank "</MODEM>"
| blank "<" ("MODEM" as arg) "/>"
| blank "<" ("ISDN" as arg) ">" blank "</ISDN>"
| blank "<" ("ISDN" as arg) "/>"
| blank "<" ("PCS" as arg) ">" blank "</PCS>"
| blank "<" ("PCS" as arg) "/>"
| blank "<" ("INTERNET" as arg) ">" blank "</INTERNET>"
| blank "<" ("INTERNET" as arg) "/>"
| blank "<" ("X400" as arg) ">" blank "</X400>"
| blank "<" ("X400" as arg) "/>"
    {
     let s = decompose lexbuf in
     "  <"^arg^">\n" ^ s ^ "  </"^arg^">\n"
   }
| blank "<" (str as tag1) ">" 
    {
     "  <"^tag1^">\n" ^ (decompose lexbuf)
   }
| blank "</" (str as tag) ">"
    {
     "  </"^tag^">" ^ (decompose lexbuf)
   }
| blank "<" (str as tag) "/>"
    {
     "  <"^tag^"/>" ^ (decompose lexbuf)
   }
| blank str as arg
    {"    "^arg ^"\n"^ (decompose lexbuf)}
| blank eof {""}

    {

let analyse info =
  decompose (Lexing.from_string info)

}
