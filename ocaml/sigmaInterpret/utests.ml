open OUnit2;;

let parse_expanse str = 
  let ((env, t) as p_program) = Parser.file Lexer.main (Lexing.from_string str) in
  List.map (fun (l,t) ->(l, Parsed_terms_expanser.expanse t)) env, Parsed_terms_expanser.expanse t
  

let eval_str str= 
  let exp_env, exp_t = parse_expanse str in
  PrintUtils.string_of Terms_printer.fprint_term (Interpret.interpret exp_env exp_t)

let format str =
  let exp_env, exp_t = parse_expanse str in
  PrintUtils.string_of Terms_printer.fprint_term exp_t


let make_test prog res = fun test_ctxt -> assert_equal res (eval_str prog);;

(*normal form term*)
let nft = "[]" 

(* field selection *)
let fsp = "[x = 5].x"
let fsr = "5"

(* method invocation *)
let mip = "[x = 42, get_x = S(s) s.x].get_x"
let mir = "42"

(* field update *)
let fup = "[f = []].f := 5"
let fur = "[f = 5]"

(* method update *)
let mup = "[f = []].f <- S(s) s.l"
let mur = format "[f = S(s) s.l ]"

(* lookup in env *)
let lep = "let x = 5 in x"
let ler = "5"

(* Name the test cases and group them together *)
let suite =
  "suite" >:::
    ["Evaluation of a normal form term" >:: make_test nft nft;
      "Field selection" >:: make_test fsp fsr;
      "Method invocation" >:: make_test mip mir;
      "Field update" >:: make_test fup fur;
      "Method update" >:: make_test mup mur;
      "Lookup in env" >:: make_test lep ler]
;;


let () =
  run_test_tt_main suite
;;
