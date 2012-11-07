module P :
  sig
    type t = string
    type anot_type = unit
    val anot_parser : unit -> unit
    val pprint_keyid : 'a -> 'a
    val compare_keyid : String.t -> String.t -> int
    val pprint_anot : 'a -> string
  end
module Nts_int :
  sig
    type anotations = Nts_functor.Make(P).anotations
    type control = Nts_functor.Make(P).control
    type transitions_container = Nts_functor.Make(P).transitions_container
    type states_container = Nts_functor.Make(P).states_container
    type inv_relation_container = Nts_functor.Make(P).inv_relation_container
    val fold_states_containers :
      states_container -> ('a -> control -> 'a) -> 'a -> 'a
    val fold_transitions_container :
      transitions_container ->
      ('a -> control -> Nts_types.nts_trans_label list -> control -> 'a) ->
      'a -> 'a
    val is_state_in_inv_relation : inv_relation_container -> control -> bool
    type nts_automaton =
      Nts_functor.Make(P).nts_automaton = {
      mutable nts_automata_name : string;
      mutable anot : anotations;
      init_states : states_container;
      final_states : states_container;
      error_states : states_container;
      input_vars : Nts_types.nts_genrel_var list;
      output_vars : Nts_types.nts_genrel_var list;
      local_vars : Nts_types.nts_genrel_var list;
      transitions : transitions_container;
    }
    type nts_system =
      Nts_functor.Make(P).nts_system = {
      nts_system_name : string;
      nts_global_vars : Nts_types.nts_genrel_var list;
      nts_automata : (string, nts_automaton) Hashtbl.t;
      nts_gvars_init : Nts_types.nts_gen_relation list option;
      nts_system_threads : (string * Big_int.big_int) list option;
    }
    val pprint_control : control -> string
    val anot_parser : unit -> anotations
    val states_container_of_states_list : control list -> states_container
    val transitions_container_of_trans_list :
      (control * control * Nts_types.nts_trans_label list) list ->
      transitions_container
    val control_of_id_param : P.t -> control
    val out_degree_of_control_state : control -> nts_automaton -> int
    val get_varinfo_by_optname :
      nts_system ->
      string option -> string -> Nts_types.nts_genrel_var option
    val get_varinfo_by_optcautomaton :
      nts_system ->
      nts_automaton option -> string -> Nts_types.nts_genrel_var option
    val get_transition_from :
      nts_automaton ->
      control -> control -> Nts_types.nts_trans_label list list option
    val get_successor_of : nts_automaton -> control -> states_container
    val get_one_state : states_container -> control option
    val pprint_inputvars : nts_automaton -> string
    val pprint_outputvars : nts_automaton -> string
    val pprint_localvars : nts_automaton -> string
    val nt_system_var_cleaner : nts_system -> nts_system
    val pprint_to_nts : nts_automaton -> string
    val pprint_nts : nts_system -> string
    val pprint_transitions : string -> nts_automaton -> string
    val compute_pred_relation : nts_automaton -> inv_relation_container
  end
