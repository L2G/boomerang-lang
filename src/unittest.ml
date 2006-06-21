
(*** Modules, Types, and Exceptions ***)

exception Unittest

(*** State ***)

let unittests = ref (fun () -> ())
let testcount = ref 0
let passcount = ref 0
let failcount = ref 0

(*** formatting ***)

let format_bool (b : bool) : unit =
  Util.format "%B" b

let format_string (s : string) : unit =
  Util.format "\"%s\"" s

let format_option (fmt : 'a -> unit) (opt : 'a option) : unit =
  match opt with
  | None -> Util.format "None"
  | Some x ->
      Util.format "@[Some (";
      fmt x;
      Util.format ")@]"

(*** Assertions ***)

let assert_equal fmt equal exp act =
  if not (equal exp act) then
    begin
      Util.format "@[Expected: ";
      fmt exp;
      Util.format "@]@\n";
      Util.format "@[Actual: ";
      fmt act;
      Util.format "@]@\n";
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
      Util.format "@[Expected: %s, but no excpetion was raised.@]@\n"
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
      Util.format "Running test: %s ...@\n" s;
      try
        begin
          incr testcount;
          t ();
          Util.format "Passed: %s@\n" s;
          incr passcount;
        end
      with
      | Unittest ->
          Util.format "FAILED: %s@\n" s;
          incr failcount;
      | e ->
          Util.format "Unexpected exception: %s@\n" (Printexc.to_string e);
          Util.format "FAILED: %s@\n" s;
          incr failcount;
    end

let run () =
  begin
    (!unittests) ();
    Util.format "@\nRan %d tests.@\n" (!testcount);
    Util.format "%d tests passed.@\n" (!passcount);
    Util.format "%d tests failed.@\n" (!failcount);
  end

