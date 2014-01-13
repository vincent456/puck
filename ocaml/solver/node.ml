
module type Label =
  sig
    type t

    val equal : t -> t -> bool

    val uid : t -> string
    val nickname : t -> string

    val abstract : t -> t -> t (* label of value to astract -> label of the future parent of the abstraction -> abstraction *)
    val string_of_label : t -> string

  end
