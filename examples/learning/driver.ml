open Oracle

let _ = 
  let show_structure strings =
    let parse_chunk c = Lex.parse_chunk c Lex.delimiter_table Lex.regex_table in
    let chunks = List.map parse_chunk strings in
    let s = Struct.discover chunks in
    let s' = Struct.refine s in
      print_endline ("Cost before refinement: " ^ (string_of_float (Struct.cost s)));
      print_endline ("Cost after refinement:  " ^ (string_of_float (Struct.cost s')));
      print_endline (Struct.to_string s');
      print_newline ()
  in
    show_structure ["123";"456";"789";"123451432"];
    show_structure ["(123)";"(456)";"(789)";"(123451432)"];
    show_structure ["(123abc)";"(456abc)";"(789abc)";"(123451432abc)"];
    show_structure ["{abc}--"; "1234"; "5678"; "9101"; "{def}--"];
    show_structure ["abc--";"123}}}"];
    show_structure ["{abc}--"; "{def}--"; "{foo}--"; "{bar}--"];
    show_structure ["{abc}--"; "{def}--"; "--{foo}"; "--{bar}"];
    show_structure ["1,2,3|"; 
		    "4,5,,baz,|"; 
		    "bar,,,5|"; 
		    "12,,bibble,,,15|"; 
		    "15,0,3,4,1,foo|"; 
		    "30,,5,,,5,,,25,,,-3|"; 
		    ",,,quux|"];
    show_structure ["1,2,3,4,5"; "10,3,49"; "12,55,0,0,60,10,3"; "1,1,2,3,5,8,13,21,34,55,89"];
    show_structure [""; "1,2,3,4,5"; "10,3,49"; "12,55,0,0,60,10,3"; "1,1,2,3,5,8,13,21,34,55,89"];
    show_structure ["|abc--"; "|def--"; "|ghi--"; "|123--"; "|456--"]
      
