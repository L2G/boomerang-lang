(* See also 'test.ml' for examples, and 'csv.mli' for documentation.
 * $Id: example.ml,v 1.1 2005/02/17 15:51:47 rich Exp $ *)

open Printf
open Csv

let csvs =
  List.map (fun name -> name, load name)
    [ "example1.csv"; "example2.csv" ] ;;

List.iter (
  fun (name, csv) ->
    print_endline name;
    print_readable csv
) csvs
