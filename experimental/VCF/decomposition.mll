 {

exception Error of string

}

let tag = [^':']*

rule decompose = parse
| (tag as arg1) ':' (tag as arg2) 
    {
     "<"^arg1^">\n"^arg2^"\n</"^arg1^">"
    }
| _ {raise (Error("invalid format .vcf"))}

{

let analyse url =
  decompose (Lexing.from_string url)

}

 
