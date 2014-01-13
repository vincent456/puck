open Format

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let rec print_list_pre_eol f sep fmt l = match l with (*pre : sep before eol*)
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s%a@\n" f h sep (print_list_eol f sep) t

let string_of f elt = 
  f str_formatter elt;
  flush_str_formatter ()
