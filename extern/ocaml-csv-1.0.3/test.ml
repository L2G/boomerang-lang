(* $Id$ *)

open Printf
open Csv

let do_testcsv filename expected =
  let csv = load filename in
  if csv <> expected then (
    printf "input file: %s\n" filename;
    printf "Csv library produced:\n";
    print csv;
    printf "Expected:\n";
    print expected;
    failwith "failed"
  )

let testcsv1 =
  do_testcsv
    "testcsv1.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns." ] ]
let testcsv2 =
  do_testcsv
    "testcsv2.csv"
    [ [ "Normal field"; "Quoted field"; "Quoted field with \"\" quotes" ] ]
let testcsv3 =
  do_testcsv
    "testcsv3.csv"
    [ [ "" ];
      [ ""; "" ];
      [ ""; ""; "" ];
      [ ""; ""; ""; "" ];
      [ ""; ""; ""; ""; "" ] ]
let testcsv4 =
  do_testcsv
    "testcsv4.csv"
    []
let testcsv5 =
  do_testcsv
    "testcsv5.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns.";
	"a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]
let testcsv6 =
  do_testcsv
    "testcsv6.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns\nand \000";
	"a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]

;;

print_endline "All tests succeeded."
