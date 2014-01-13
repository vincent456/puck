
type edge_t = 
  | Uses of string * string  (* uid * uid *)
  | Contains of string * string  (* uid * uid *)

type constraint_t =
  | HideFrom of string * string
  | IsFriendOf of string * string

