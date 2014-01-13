
let report_loc (b,e) =
  Lexing.(
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
    Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" Settings.filename l fc lc)

let handle_prolog graph =
  if Settings.prolog then 
    begin
      let f x =
	match Settings.pl_filename with
	| None -> Format.printf "Prolog :@\n"; x ()
	| Some fn -> Format.set_formatter_out_channel  (open_out fn);
		     let evaluator = match Settings.pl_eval with
		       | None -> "./evaluator.pl"
		       | Some e -> e
		     and decouple = match Settings.decouple with
		       | None -> ""
		       | Some d -> ":-ensure_loaded('" ^ d ^ "').\n"
		     in
		     Format.printf ":-ensure_loaded('%s').@\n%s" evaluator decouple; x ();
		     Format.printf ":-pl2dot.@\n:-halt.@\n"
      in
      f ( fun _ -> List_ag_printer.print_ag  graph)
    end

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
      let ((env, t) as p_program) = Parser.file Lexer.main lexbuf in
      close_in channel;
      if Settings.verbose > 0 then
	(Format.printf "Parsed :@\n";
	 Parsed_terms_printer.print_program p_program);
      
      let m_prog = Mutable_p_terms.(List.map (fun (n, def) -> n, mp_term_of_p_term def) env,
					  mp_term_of_p_term t) in
      Mp_terms_printer.print_program m_prog;
      
      let exp_env = List.map (fun (l,t) ->(l, Parsed_terms_expanser.expanse t)) env in
      let exp_t = Parsed_terms_expanser.expanse t in
      
      Format.printf "Expansed :@\n";
      Terms_printer.print_program (exp_env, exp_t);

      if Settings.interpret then
	(Format.printf "Eval :@\n";
	 Terms_printer.print_term (Interpret.interpret exp_env exp_t));
      
      (* prolog_graph Ag_constructor.ag_of_program (exp_env, exp_t); *)
      let (nodes, edges) = List_ag_constructor.ag_of_p_program p_program in
      handle_prolog (nodes,edges, []);
      
    with
    | Lexer.Lexical_error s ->
	report_loc Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	Format.eprintf "lexical error: %s\n@." s;
	exit 1

    | Parser.Error ->
	report_loc Lexing.(lexeme_start_p lexbuf, lexeme_end_p lexbuf);
	Format.eprintf "syntax error\n@.";
	exit 1
    | Interpret.Wrong (msg, t) ->
       Format.eprintf "Interpret; %s %a\n@." msg Terms_printer.fprint_term t;
    | e ->
        Format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
        exit 2

