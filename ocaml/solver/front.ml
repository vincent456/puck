let report_loc (b,e) =
  Lexing.(
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
    Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" Settings.filename l fc lc)


module Printer = Ag_printer.Make(Pair_label)
module Solver = Solver.Make(Pair_label)
module Converter = Ag_of_list_ag.Make(Pair_label)

open Access_graph

let () = 
    let channel = open_in Settings.filename in
    let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <-
    { 
      Lexing.pos_fname = Settings.filename; 
      Lexing.pos_lnum  = 1;
      Lexing.pos_bol   = 0; 
      Lexing.pos_cnum  = 0
    };
    try
      let parsed = Parser.file Lexer.main lexbuf in
      close_in channel;
      begin
	(* Parsed_graph_printer.eprint_ag parsed; *)
	(* Format.eprintf "********************@\n"; *)
	(* Format.eprintf "********************@\n"; *)
	(* Format.eprintf "********************@\n"; *)
	(* if Settings.verbose > 0 then *)
	(* 	(Format.printf "Parsed :@\n"; *)
	(* 	 Parsed_terms_printer.print_program p_program); *)
	match Converter.ag_of_list_ag  parsed with
	| [] -> failwith "No roots !"
	| hd :: [] ->  (* Ag_dot_printer.print hd; *)
		       (* let violations = Access_graph.find_violations hd in *)
		       (* Format.eprintf " violations: @\n"; *)
		       (* List.iter (fun (user, usee) -> Access_graph.( *)
		       (* 				     let ((uid1,_), (uid2,_)) = user.value, usee.value in *)
		       (* 				     Format.eprintf "%s -> %s@\n" uid1 uid2)) violations; *)
		       ignore (Solver.find_and_repare hd);
		       Printer.Dot.print hd
	| l -> List.iter (fun n -> Printer.Prolog.fprint_node Format.str_formatter n.value) l;
	       let nodes = Format.flush_str_formatter () in
	       failwith ("too much roots : " ^ nodes)
      end
    with
    | Lexer.Lexical_error s ->
	report_loc Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	Format.eprintf "lexical error: %s\n@." s;
	exit 1

    | Parser.Error ->
	report_loc Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	Format.eprintf "syntax error\n@.";
	exit 1
    | e ->
        Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
        exit 2
