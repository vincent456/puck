
open Terms
open Format
open PrintUtils

let fprint_const = Parsed_terms_printer.print_const

let fprint_op = Parsed_terms_printer.print_op

let rec fprint_term fmt e = match e with
  | Variable v -> fprintf fmt "%s" v
  | Object o -> fprintf fmt "[@[%a@]]" (print_list_eol fprint_method_def ",") o
  | Selection (t,l)-> fprintf fmt "%a.%s" fprint_term t l
  
  | Update (t,l,(Some b, body, env)) -> 
     fprintf fmt "@[%a.%s <- S(%s)@[%a%a@]@]" fprint_term t l b fprint_term body fprint_env env
  | Update (t,l,(None, body, env)) -> 
     fprintf fmt "@[%a.%s := @[%a@]@]" fprint_term t l fprint_term body

  | Const c -> fprint_const fmt c
  | MathExpr (o, t1, t2) -> fprintf fmt "%a %a %a" fprint_term t1 fprint_op o fprint_term t2

and fprint_method_def fmt (name, m) = 
  fprintf fmt "@[%s = %a@]" name fprint_method m
and fprint_term_def fmt (name, t) = 
    fprintf fmt "@[%s = %a@]" name fprint_term t
and fprint_method fmt (binder, body, env) =
  match binder with
  | None -> fprintf fmt "@[%a%a@]" fprint_term body fprint_env env
  (* | Some b -> fprintf fmt "@[ς(%s) %a@]" b fprint_term body *)
  | Some b -> fprintf fmt "S(%s)@[%a%a@]" b fprint_term body fprint_env env
and fprint_env fmt env =
  match env with
  | [] -> ()
  | _ -> fprintf fmt "{{@[%a@]}}" (print_list_eol fprint_term_def ",") env

let fprint_defs fmt env =
  match env with
  | [] -> ()
  | _ -> fprintf fmt "@[let %a @\nin@]" (print_list_pre_eol fprint_term_def " and") env


(* let fprint_program (env, t) = *)
(*   List.iter (fun nd -> Format.printf "%a@\n@." fprint_node nd) ndl *)
let fprint_program fmt (env,t) =
  fprintf fmt "%a %a" fprint_defs env fprint_term t

let print_program (env, t) =
  Format.printf "%a %a@\n" fprint_defs env fprint_term t

let print_term = Format.printf "%a@\n" fprint_term
let print_env = Format.printf "%a@\n" fprint_env
