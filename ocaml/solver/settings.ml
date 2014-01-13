(* Prepare for parsing the command line. *)

let filename = ref None
let insert name = filename := Some name

let verbose = ref 0
(* let interpret = ref false *)
(* let prolog = ref false *)
(* let pl_filename = ref None *)
(* let pl_output name = pl_filename := Some name *)

(* let pl_eval = ref None *)
(* let set_pl_eval name = pl_eval := Some name *)

(* let decouple = ref None *)
(* let set_decouple n = decouple := Some n *)

let options = Arg.align [
  "-v", Arg.Unit (fun() -> incr verbose), "increase verbosity"

  (* "-i", Arg.Set interpret, "interpret the input file"; *)
  (* "-interpret", Arg.Set interpret, "interpret the input file"; *)
  
  (* "-pl", Arg.Set prolog, "print the prolog access graph on the standard output"; *)
  (* "-o", Arg.String pl_output, "if -pl option is set, change the output to the given filename"; *)
  (* "-pleval", Arg.String set_pl_eval, "if -pl and -o options are set, change the filename of the prolog evaluator"; *)
  (* "-decouple", Arg.String set_decouple, "if -pl and -o options are set, set the filename of the prolog constraints file" *)
]

let usage =
  Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* Export the settings. *)

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

let verbose = !verbose
(* let interpret = !interpret *)
(* let prolog = !prolog *)
(* let pl_filename = !pl_filename *)
(* let pl_eval = !pl_eval *)
(* let decouple = !decouple *)
