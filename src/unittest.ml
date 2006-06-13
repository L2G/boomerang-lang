
(*** Modules, Types, and Exceptions ***)

exception Unittest

(*** State ***)

let unittests = ref (fun () -> ())
let testcount = ref 0
let passcount = ref 0
let failcount = ref 0

(*** formatting ***)

let format_bool (b : bool) : unit =
  Format.printf "%B" b

let format_string (s : string) : unit =
  Format.printf "\"%s\"" s

let format_option (fmt : 'a -> unit) (opt : 'a option) : unit =
  match opt with
  | None -> Format.printf "None"
  | Some x ->
      Format.printf "@[Some (";
      fmt x;
      Format.printf ")@]"

(*** Assertions ***)

let assert_equal fmt equal exp act =
  if not (equal exp act) then
    begin
      Format.printf "@[Expected: ";
      fmt exp;
      Format.printf "@]@\n";
      Format.printf "@[Actual: ";
      fmt act;
      Format.printf "@]@\n";
      raise Unittest
    end

let assert_eq fmt exp act =
  assert_equal fmt (=) exp act

let assert_true = assert_eq format_bool true

let assert_false = assert_eq format_bool false

let assert_exn e code =
  try
    begin
      code ();
      Format.printf "@[Expected: %s, but no excpetion was raised.@]@\n"
        (Printexc.to_string e);
      raise Unittest;
    end
  with
  | e' -> if e' <> e then raise e'

(*** Testing ***)

let add_test (s : string) (t : unit -> unit) : unit =
  let tests = !unittests in
  unittests := fun () ->
    begin
      tests ();
      Format.printf "Running test: %s ...@\n" s;
      try
        begin
          incr testcount;
          t ();
          Format.printf "Passed: %s@\n" s;
          incr passcount;
        end
      with
      | Unittest ->
          Format.printf "FAILED: %s@\n" s;
          incr failcount;
      | e ->
          Format.printf "Unexpected exception: %s@\n" (Printexc.to_string e);
          Format.printf "FAILED: %s@\n" s;
          incr failcount;
    end

let run () =
  begin
    (!unittests) ();
    Format.printf "@\nRan %d tests.@\n" (!testcount);
    Format.printf "%d tests passed.@\n" (!passcount);
    Format.printf "%d tests failed.@\n" (!failcount);
  end

