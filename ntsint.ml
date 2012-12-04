
      

module P =
  struct
    type t = string
    type anot_type = unit
    let anot_parser () = ()
    let pprint_keyid s = s
    let key_val_of_string s = s 
    let compare_keyid = String.compare
    let pprint_anot _ = "" 
  end


module Nts_int = Nts_functor.Make(P)

