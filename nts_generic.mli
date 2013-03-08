(*
This files defines the functions prototypes used to declare/use/transform
the numerical transitions systems and their relations. The intended relations
complies with the NTL lib documentations, see README.txt.


(c) Verimag 2012

Questions and remarks : write to florent_dot_garnier_AT_gmail_dot_com
  


This files contains the implementation of the Numerical Transition Library 
--see http://richmodels.epfl.ch/ntscomp-- main objects, namely :

_ Numerical transitions subsystems, (i.e. parametric counter automaton
  with return values upon return)
_ Hyerarchical transistions subsystems .


Plus a parser, a pretty printer as well as cleanup functions.
A type checker will be added.



Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA



*)

exception Found_a_primed_var
exception Type_mismatch_in_arithm_expression of 
    Nts_types.nts_genrel_arithm_exp
  *  Nts_types.nts_genrel_arithm_exp


val zero : Nts_types.nts_genrel_arithm_exp
val one : Nts_types.nts_genrel_arithm_exp

(** Pretty prints a variable name, regardless of its type*)
val nts_pprint_genvar : Nts_types.nts_genrel_var -> string

(**Pretty prints a variable list, with each variable name separated by a comma.*)
val pprint_ntsgen_var_list : Nts_types.nts_genrel_var list -> string

(** Pretty prints a variable and its type*)
val nts_pprint_nts_typeinfo_genvar : Nts_types.nts_genrel_var -> string

(** Returns true whenever a variable type has type int.*)
val is_int_var : Nts_types.nts_genrel_var -> bool

(** Returns true whenever a variable type has type real.*)
val is_real_var : Nts_types.nts_genrel_var -> bool

(** Pretty prints both strings separared by a comma when the first one is non-empty,
or pretty prints the non empty argument when there is one, or returs an emptry 
string when both are empty.
*)
val concat_if_first_arg_nonzero : string -> string -> string

val concat_comma_both_arg_non_empty : string -> string -> string


(*Pretty prints all variables, sorted by types with their associated types.*)
val pprint_typeinfo_nts_genvar_list : Nts_types.nts_genrel_var list -> string

(** Shall be removed from this part. Used for pretty printing artithmetical terms
and booleans expressions.
*)

val size_genrel_arithm_deeper_than :
  Nts_types.nts_genrel_arithm_exp -> int -> bool

val size_genrel_deeper_than : Nts_types.nts_gen_relation -> int -> bool


(** Pprints a general expression --Constrainsts between prime and unprimed
variables.*)
val nts_pprint_genrel_arithm_exp : Nts_types.nts_genrel_arithm_exp -> string
val type_of_gen_arithmetic_expr : Nts_types.nts_genrel_arithm_exp -> 
  Nts_types.nts_base_types

val pprint_gen_rel_arithm_list :
  Nts_types.nts_genrel_arithm_exp list -> string

(** This function returns Some(type_t) whenever both expression have
type_t as type and None if their types differs.*)
val arithm_exp_same_type : Nts_types.nts_genrel_arithm_exp -> 
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_base_types option
  

val nts_pprint_bool_binop :
  string -> Nts_types.nts_gen_bool_binop -> string -> string

val nts_pprint_aritm_binop : Nts_types.nts_gen_arithm_binop -> string

val nts_pprint_arithm_unop : Nts_types.nts_gen_arithm_unop -> string

val nts_pprint_genrel : Nts_types.nts_gen_relation -> string

val boolean_relation : Nts_types.nts_genrel_arithm_exp -> bool

(**Removes some --the most ugly of them -- tautologies from booleans expressions*)
val simplify_genrel_bottom_top :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation

val simplify_gen_rel :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation

(** Does the input relation only depends on deterministic variables ?*)
val is_gen_bool_det : Nts_types.nts_gen_relation -> bool
(** Does the arithmecic expression only depends on deterministic variables ?*)
val is_gen_arithm_exp_a_function : Nts_types.nts_genrel_arithm_exp -> bool

(** Determines whether a boolean expression is false --e.g. contains a false*)
val static_check_if_gen_relation_false : Nts_types.nts_gen_relation -> bool

val static_check_if_gen_translist_unsat :
  Nts_types.nts_trans_label list -> bool

val nts_pprint_gen_trans_label : Nts_types.nts_trans_label -> string

val is_label_true : Nts_types.nts_trans_label -> bool

(** Pretty prints a transition label*)
val nts_pprint_gen_trans_label_list :
  Nts_types.nts_trans_label list -> string


(** Creates a constantes and types it as a nts_genrel_arithm expressions*)
val make_nts_int_cst_of_int : int -> Nts_types.nts_genrel_arithm_exp
val make_nts_int_cst : Big_int.big_int -> Nts_types.nts_genrel_arithm_exp
val make_nts_real_cst : float -> Nts_types.nts_genrel_arithm_exp
val make_nts_bool_cst : bool -> Nts_types.nts_genrel_arithm_exp


val make_nts_genvar : string -> Nts_types.nts_base_types -> 
  Nts_types.nts_genrel_var

val primerized_nts_var : Nts_types.nts_genrel_var -> Nts_types.nts_genrel_var

val aexpr_of_nts_var : Nts_types.nts_var -> Nts_types.nts_genrel_arithm_exp

val aexpr_of_nts_genrel_var : Nts_types.nts_genrel_var -> Nts_types.nts_genrel_arithm_exp

val affect_aexpr_to_nts_var : Nts_types.nts_var -> Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation

val affect_aexpr_to_nts_genrel_var : Nts_types.nts_genrel_var -> Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation

val make_affect_to_var_from_exp : Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation

val add_arithm_expr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp

val sub_arithm_expr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp

val mul_arithm_expr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp

val div_arithm_expr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp

val mod_arith_expr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp


val inv_sign_of_aexp :
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_genrel_arithm_exp
val guard_lt_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation
val guard_gt_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation
val guard_leq_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation
val guard_geq_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation
val guard_eq_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation

val guard_neq_aexpr :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation

val make_transition_of_translabel :
  Nts_types.nts_trans_label -> Nts_types.nts_trans_label list

val add_to_transition :
  Nts_types.nts_trans_label list ->
  Nts_types.nts_trans_label -> Nts_types.nts_trans_label list


val and_of_genrel : 
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation ->
  Nts_types.nts_gen_relation 

val or_of_genrel :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation ->
  Nts_types.nts_gen_relation 
  
val neg_of_genrel : Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation

val neg_cond_in_guard : Nts_types.nts_trans_label -> Nts_types.nts_trans_label

val make_guard_of_relation : Nts_types.nts_gen_relation ->
  Nts_types.nts_trans_label


module Vars_acc :
  sig
    type elt = Nts_types.nts_genrel_var
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end


val primed_vars_of_genrel_aexpr :
  Nts_types.nts_genrel_arithm_exp -> Vars_acc.t
val primed_vars_of_genrel : Nts_types.nts_gen_relation -> Vars_acc.t
