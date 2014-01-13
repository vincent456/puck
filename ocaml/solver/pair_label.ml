
  type t = string * string

  let equal (uid1, _) (uid2, _) = uid1 = uid2
 
  let uid = fst
  let nickname = snd
  					   
  let abstract (_, nickname)  (parent_uid , _) =
    ((parent_uid ^ ".abstract_" ^ nickname), "abstract_" ^ nickname)

  let string_of_label (uid, _) = uid
