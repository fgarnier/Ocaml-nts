(**

Generic interface for numerical transitions systems.

(C) Verimag 2012

For questions and/or remarks :

 Write to florent dot garnier at imag dot fr


*)


open Nts_types
open Hashtbl
open Lexing 

exception UnboundVarName of string 
exception No_such_counter_automata_in_nts_system of string * string 

module type NTS_PARAM =
  sig
    type t         
      (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val anot_parser : unit -> anot_type 
      (*Needed In case of anotation
	are using mutable container that
	need to be created, hashtbl for
	instance*)
      
    val pprint_keyid : t -> string (*Types for pprinting anotations*)
    val compare_keyid : t-> t -> int (* comparision function for keyid*)
    val pprint_anot : anot_type -> string
  end 



(** Signature definition of the Make functor. Types are abstracts.*)
module Make :
  functor( Param : NTS_PARAM ) ->
sig 
      
  type anotations (** Type for anotations*)
  type control   (** Type of a control state*)
  type transitions_container (** Type used to encode transitions
				 between control states *)
  type states_container  (** Container used to sore a state collection*)
  type inv_relation_container (** Type used to encode the inverse of the
				  unlabelled successor relation transition
			      *)

  val fold_states_containers : states_container ->  ( 'a -> control -> 'a ) -> 'a -> 'a
  val fold_transitions_container : transitions_container ->  ('a -> control -> nts_trans_label list-> control -> 'a ) -> 'a -> 'a 
 

  val iter_transitions_container : transitions_container ->  ( control -> nts_trans_label list-> control -> unit ) -> unit 
 (** 

      'a is the type of the folded value.
      A nts transition is defined by a tuple of type 
      ( control * nts_gen_rel list * control ). A function of
      type ('a -> control -> nts_gen_rel list-> control -> 'a ) is
      required by this folder. e.g : type 'a = string for any pretty
      printting.
									        *)
    
  val is_state_in_inv_relation : inv_relation_container -> control -> bool


    
  (** counter automata with inputs and
      output variables and hierachical 
      calls enabled.
  *)
  type nts_automaton = {
    
    mutable nts_automata_name : string;
    mutable anot : anotations  ;
    
    init_states : states_container;
    final_states : states_container;
    error_states : states_container;
    
    input_vars : nts_genrel_var list; (*Variable ordering is important*)
    output_vars : nts_genrel_var list;
    local_vars : nts_genrel_var list;
    transitions : transitions_container;
   
  }

  


  type nts_system = (** Hierarchical numerical transition systems *)
      {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
	nts_gvars_init : Nts_types.nts_gen_relation list option;
        nts_system_threads : (string * Big_int.big_int) list option;
      }
  
  (** 
      Experimental section 
  *)
  

	
	
  val pprint_control : control -> string	
  val anot_parser : unit -> anotations
    
  val states_container_of_states_list : control list -> states_container  
  val transitions_container_of_trans_list : (control * control * Nts_types.nts_trans_label list ) list -> transitions_container
    
  (*val rename_nts_system : nts_system -> string -> unit*)
  
  val control_of_id_param : Param.t -> control 
  
  val out_degree_of_control_state :  control ->  nts_automaton -> int

  val get_varinfo_by_optname : nts_system -> string option -> string -> nts_genrel_var option 

  val get_varinfo_by_optcautomaton : nts_system -> nts_automaton option ->string -> nts_genrel_var option
    
    
  val get_transition_from :
    nts_automaton ->
    control -> control -> Nts_types.nts_trans_label list list option
  
  val get_successor_of : nts_automaton -> control -> states_container
  val get_one_state : states_container -> control option

  (** Picks an outing transiton from control in the automaton*)
  val get_one_transition : nts_automaton -> control -> (control * Nts_types.nts_trans_label list) 
    
  val pprint_inputvars : nts_automaton  -> string
  val pprint_outputvars : nts_automaton  -> string 
  val pprint_localvars : nts_automaton  -> string
    
  (**
     computes a numerical transition system in which all local variables
     list of each automaton has been cleared of non used varibles
  *)
  val nt_system_var_cleaner : nts_system -> nts_system 

  val pprint_to_nts : nts_automaton -> string
  val pprint_nts : nts_system -> string 
    (* Here for debuging purposes. Shall be removed for release
    versions*)
  val pprint_transitions : string -> nts_automaton -> string
  
  (** Compute the set of one step predecessors of all control states*)
  (*val compute_pred_relation : nts_automaton -> 
    (control, (control , unit) Hashtbl.t ) Hashtbl.t*)
  val compute_pred_relation : nts_automaton -> inv_relation_container
    
end

