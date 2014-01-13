
open Parsed_terms
open Format
open PrintUtils

let print_const fmt c = match c with
  | Pbool b -> fprintf fmt "%b" b
  | Pint i -> fprintf fmt "%d" i
  | Preal f -> fprintf fmt "%f" f

let print_op fmt = function
  | Plus -> fprintf fmt "+"
  | Times -> fprintf fmt "*"

let rec print_term fmt e = match e with
  | PVariable v -> fprintf fmt "%s" v
  | PObject o -> fprintf fmt "[@[%a@]]" (print_list_eol print_method_def ",") o
  | PSelection (t,l)-> fprintf fmt "%a.%s" print_term t l
  | PUpdate (t,l,(Some b, body)) -> fprintf fmt "%a.%s <- S(%s)@[%a@]" print_term t l b print_term body
  | PUpdate (t,l,(None, body)) -> fprintf fmt "%a.%s := @[%a@]" print_term t l print_term body
			      
  | PConst c -> print_const fmt c
  | PMathExpr (o, t1, t2) -> fprintf fmt "%a %a %a" print_term t1 print_op o print_term t2
  (* | PLambda (b, t) -> fprintf fmt "λ(%s) @[%a@]" b print_term t *)
  | PLambda (b, t) -> fprintf fmt "L(%s) @[%a@]" b print_term t
  | PApply (t1, t2) -> fprintf fmt "%a(@[%a@])" print_term t1 print_term t2

and print_method_def fmt (name, t) = 
  fprintf fmt "@[%s = %a@]" name print_method t
and print_method fmt (binder, body) =
  match binder with
  | None -> fprintf fmt "@[%a@]" print_term body
  (* | Some b -> fprintf fmt "@[ς(%s) %a@]" b print_term body *)
  | Some b -> fprintf fmt "S(%s) @[%a@]" b print_term body

let print_env fmt env =
  let rec print fmt e = match e with
    | [] -> ()
    | (name, t) :: [] -> fprintf fmt "@[%s = %a@]" name print_term t
    | (name, t) :: tl -> fprintf fmt "@\nand @[%s = %a@]" name print_term t
  in
  match env with
  | [] -> ()
  | _ -> fprintf fmt "let %a @\nin" print env

(* let print_program (env, t) = *)
(*   List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl *)

let print_program (env, t) =
  Format.printf "%a %a@\n" print_env env print_term t
