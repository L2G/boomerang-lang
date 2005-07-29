open Unix

let icalviewer = "../../extern/iCalendar/iCalViewer"
let harmony = "../../src/harmony"

let atos args =
  Array.fold_left (fun s a -> s^a^" ") "" args

let exec_in_fork_and_wait cmd args =
  match fork () with
    0 -> execv cmd (Array.append [|cmd|] args)
  | _ -> 
      (match wait () with
	_, WEXITED 0 -> 
	  let s = Printf.sprintf 
	      "Successfully executed the following command :\n %s %s" cmd (atos args) in
	  print_endline s
      |	_ -> 
	  let s = Printf.sprintf 
	      "Something went wrong with the following command :\n %s %s" cmd (atos args) in
	  failwith s )
	    
let read_ics () =
  let (r1,r2,r3) = (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)) in
  let (r1m, r2m, r3m) = (r1^".meta", r2^".meta", r3^".meta") in
  exec_in_fork_and_wait icalviewer [|"ascal"; r1; r1m|];
  exec_in_fork_and_wait icalviewer [|"ascal"; r2; r2m|];
  exec_in_fork_and_wait icalviewer [|"ascal"; r3; r3m|];
  (r1m, r2m, r3m)

let harmonize r1 r2 ar =
  let (newr1, newr2, newar) = ("new"^r1, "new"^r2, "new"^ar) in
  let harm_args = [|"-schema"; "ICalendar.ICalendar_A";
		    "-ar"; ar; "-r1"; r1; "-r2"; r2;
		    "-lensar"; "ICalendar.l";
		    "-lensr1"; "ICalendar.l";
		    "-lensr2"; "ICalendar.l";
		    "-newar"; newar; "-newr1"; newr1; "-newr2"; newr2;
		    "-I"; "../../lenses"; "-I"; "."
		  |] in
  exec_in_fork_and_wait harmony harm_args;
  (newr1, newr2, newar)
    
let write_ics (r1m, r2m, r3m) =
  let (r1, r2, r3) = (Sys.argv.(4), Sys.argv.(5), Sys.argv.(6)) in
  exec_in_fork_and_wait icalviewer [|"asmeta"; r1m; r1|];
  exec_in_fork_and_wait icalviewer [|"asmeta"; r2m; r2|];
  exec_in_fork_and_wait icalviewer [|"asmeta"; r3m; r3|];
  ()

let usage = "Usage : harmonize-calendars REP1 REP2 ARCH NEWR1 NEWR2 NEWARCH"

let _ = match Array.length Sys.argv with
  7 -> 
    print_endline (atos Sys.argv);
    let (r1m, r2m, arm) = read_ics () in
    write_ics (harmonize r1m r2m arm)
| _ -> prerr_endline usage
