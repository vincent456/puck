
open Mutable_p_terms
open Format
open PrintUtils

let fprint_const = Parsed_terms_printer.print_const 
let fprint_op = Parsed_terms_printer.print_op 

let rec fprint_term fmt e = match e with
  | MPVariable v -> fprintf fmt "%s" v.v
  | MPObject o -> fprintf fmt "[@[%a@]]" (print_list_eol fprint_method_def ",") o.methods
  | MPSelection s -> fprintf fmt "%a.%s" fprint_term s.receiver s.label
  | MPUpdate u -> 
     begin
       match u.new_val.binder with
       | Some b -> fprintf fmt "%a.%s <- S(%s)@[%a@]" fprint_term u.sel.receiver u.sel.label b fprint_term u.new_val.body
       | None -> fprintf fmt "%a.%s := @[%a@]" fprint_term u.sel.receiver u.sel.label fprint_term u.new_val.body
     end
			      
  | MPConst c -> fprint_const fmt c
  | MPMathExpr (o, t1, t2) -> fprintf fmt "%a %a %a" fprint_term t1 fprint_op o fprint_term t2
  (* "λ(%s) @[%a@]" *)
  | MPLambda l -> fprintf fmt "L(%s) @[%a@]" l.binder fprint_term l.body
  | MPApply (t1, t2) -> fprintf fmt "%a(@[%a@])" fprint_term t1 fprint_term t2

and fprint_method_def fmt (name, m) = 
  fprintf fmt "@[%s = %a@]" name fprint_method m
and fprint_method fmt m =
  match m.binder with
  | None -> fprintf fmt "@[%a@]" fprint_term m.body
  (* | Some b -> fprintf fmt "@[ς(%s) %a@]" b print_term body *)
  | Some b -> fprintf fmt "S(%s) @[%a@]" b fprint_term m.body

let fprint_env fmt env =
  let rec print fmt e = match e with
    | [] -> ()
    | (name, t) :: [] -> fprintf fmt "@[%s = %a@]" name fprint_term t
    | (name, t) :: tl -> fprintf fmt "@\nand @[%s = %a@]" name fprint_term t
  in
  match env with
  | [] -> ()
  | _ -> fprintf fmt "let %a @\nin" print env

(* let print_program (env, t) = *)
(*   List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl *)

let print_program (env, t) =
  Format.printf "%a %a@\n" fprint_env env fprint_term t
