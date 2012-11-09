
(* Type of a variable diary *)
type vars_entry (*= UVars_diary of (string, unit) Hashtbl.t*)

type vars_entry_by_name (*=
    UNamedVarsDiary of (string, Nts_types.nts_var) Hashtbl.t*)

type called_subsystems_diary 

val create_empty_var_diary : unit -> vars_entry
val create_fun_name_in_call_table : unit -> called_subsystems_diary 

val get_diary_table : vars_entry -> (string, unit ) Hashtbl.t

val add_vars_of_cnt_trans_label_to_diary :
  vars_entry -> Nts_types.nts_trans_label -> unit


val register_called_subsystems : called_subsystems_diary -> Nts_types.nts_trans_label -> unit

val add_vars_of_trans_label_list_to_diary :
  vars_entry ->  Nts_types.nts_trans_label list -> unit
val add_fun_name_in_call_table :  called_subsystems_diary->string -> unit

(** Answer yes when the variable is listed in the entry, false
in the opposite case.*)

val contains_var : vars_entry -> Nts_types.nts_var -> bool
val contains_nts_genrel_var : vars_entry -> Nts_types.nts_genrel_var -> bool

val pprint_diary : vars_entry -> unit

