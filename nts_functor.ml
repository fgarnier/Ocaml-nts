(*

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

open Nts_types
open Hashtbl
open Nts_generic
open Simplification (* Contains what needed to remove
		    useless variable declarations*)


exception Var_name_already_used
exception Found_genvar of nts_genrel_var
exception No_such_counter_automata_in_nts_system of string * string 
exception UnboundVarName of string 
exception Nts_i_have_a_binding 



 type ('a , 'b) gen_bin_relation =  [`Generic_relation of 'a * 'b]
 

let pprint_trans_list_foldleft (s : string ) ( trans : nts_trans_label ) =
  match (s,trans) with 
    | ("",CntGenGuard(guard))-> 
      let s_guard = Nts_generic.simplify_gen_rel guard in
      begin
	match s_guard with 
	    CntGenTrue -> ""
	  | _ -> Nts_generic.nts_pprint_genrel s_guard
      end
    | ("",_) ->
      (Nts_generic.nts_pprint_gen_trans_label trans )
    | (_,CntGenGuard(guard)) -> 
      let s_guard = Nts_generic.simplify_gen_rel guard in
      begin
	match s_guard with 
	    CntGenTrue -> s
	  | _ -> s^ " and "^(Nts_generic.nts_pprint_genrel s_guard) 
      end
	
    | (_,_) -> s^" and "^(Nts_generic.nts_pprint_gen_trans_label trans )
  



module type NTS_PARAM =
  sig
    type t         (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val anot_parser : unit -> anot_type
    val pprint_keyid : t -> string
    val compare_keyid : t -> t -> int
    val pprint_anot : anot_type -> string (*Types for pprinting anotations*)
  end


module Make =
  functor( Param : NTS_PARAM )->
struct 
  type anotations = Nts_Anot of Param.anot_type
  type control = Nts_State of Param.t (* Control state *)

  type states_container = (control , unit ) Hashtbl.t
  type transitions_container = (control, (control , nts_trans_label list ) Hashtbl.t) Hashtbl.t
  type inv_relation_container = (control, (control , unit) Hashtbl.t ) Hashtbl.t
      
  let size_hash = 97
  let pprint_control c =
    match c with
	Nts_State(s) -> Param.pprint_keyid s

  (* let pprint = Param.pprint_keyid *)
  let pprint_anotation a =
    match a with
	Nts_Anot(l)-> Param.pprint_anot l
      
  type nts_automaton =
      {
	mutable nts_automata_name : string; 
	mutable anot : anotations;
	(*states : (control , unit ) Hashtbl.t;*)
	init_states : states_container ;
	final_states : states_container;
	error_states : states_container;
	input_vars : nts_genrel_var list; (*Variable ordering is important*)
        output_vars : nts_genrel_var list;
        local_vars : nts_genrel_var list;
	transitions : transitions_container ;
      }

  type nts_system = 
      {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
	nts_gvars_init : nts_gen_relation list option; (* 
							  Initial conditions
							  that global variables
							  satisfy. 
						       *)
	nts_system_threads : ( string * Big_int.big_int ) list  option;   (*
							Optional
							threads declaration,
							name of the nts subsystem
							associated to the number
							of occurences.
						      *)

      }



    let anot_parser = (fun s -> Nts_Anot((Param.anot_parser s)))

  (*Need not appear in the API*)
  let get_cautomata_names_of_nts nts_sys =
    let key_name_folder vname _ retstring  =
      match retstring with 
	  "" -> vname
	| _ -> vname ^","^ retstring
    in
    (Hashtbl.fold key_name_folder  nts_sys.nts_automata "")


  let control_of_id_param p =
    Nts_State (p)

  let is_state_in_inv_relation table cstate =
    Hashtbl.mem table cstate
    

  (**Returns the collection of transitions betwenn sorg and sdests
     The result has type cnt_translabel list list
  *)
	
  let get_transition_from  cautomata sorg sdest =
    if not (Hashtbl.mem cautomata.transitions sorg)
    then None
    else 
      begin
	let origin_table = Hashtbl.find cautomata.transitions sorg in
	  try
	    let transitions = Hashtbl.find_all origin_table sdest in
	      Some(transitions)
	  with
	      Not_found -> None
      end

	
  (*None is returned if no transition exists*)
	
	
  (*  Search for a variable name, return Some(sort)  if referenced
      in the globals or within the cautomaton, or none if not found at 
      all 
  *)  

  let get_varinfo_by_optname nts_sys  (cname : string option) (vname : string) =
    let search_varname_iterator vname ntvar =
      match ntvar with
	 NtsGenVar(NtsVar(name,_),_)  ->
	  if (String.compare name vname )==0 then
	    raise (Found_genvar(ntvar))
	  else ()
    in
    try
      List.iter (search_varname_iterator vname) nts_sys.nts_global_vars;
      match cname with
	  Some(cname)-> 
	    begin
	      try
		let c = Hashtbl.find nts_sys.nts_automata cname 
		in
		List.iter (search_varname_iterator vname) c.input_vars;
		List.iter (search_varname_iterator vname) c.output_vars;
		List.iter (search_varname_iterator vname) c.local_vars;
		(*If found, the raised exception of type Found_var is
		handled in the topmost try ... with block.*)
	
		None (* This is the default value, i.e. matching variable*)
	      with
		  Not_found -> 
		    begin
		      let cautomata_name_list = 
			get_cautomata_names_of_nts nts_sys in
		      let ex = 
			No_such_counter_automata_in_nts_system
			  (vname,cautomata_name_list) in
		      raise ex
		    end   
	    end
	| None -> None
    with 
	Found_genvar v -> Some(v)



  let get_varinfo_by_optcautomaton nts_sys  (cautomatopt : nts_automaton option) (vname : string) =
    let search_varname_iterator vname ntvar =
      match ntvar with
	 NtsGenVar(NtsVar(name,_),_)  ->
	  if (String.compare name vname )==0 then
	    raise (Found_genvar(ntvar))
	  else ()
    in
    try
      List.iter (search_varname_iterator vname) nts_sys.nts_global_vars;
      match cautomatopt with
	  Some(c)-> 
	    begin
	      try
		List.iter (search_varname_iterator vname) c.input_vars;
		List.iter (search_varname_iterator vname) c.output_vars;
		List.iter (search_varname_iterator vname) c.local_vars;
		(*If found, the raised exception of type Found_var is
		handled in the topmost try ... with block.*)
	
		None (* This is the default value, i.e. matching variable*)
	      with
		  Not_found -> 
		    begin
		      let cautomata_name_list = 
			get_cautomata_names_of_nts nts_sys in
		      let ex = 
			No_such_counter_automata_in_nts_system
			  (vname,cautomata_name_list) in
		      raise ex
		    end   
	    end
	| None -> None
    with 
	Found_genvar v -> Some(v)


	  
  (** Binding between the generic definition of fold_states_containers  
      provided in the interface and the specific implementation proposed
      in this file.
  *)

  let fold_states_containers statec folder_fun init_val =
    let bind_folder control () prefix =
      folder_fun prefix control
    in
    Hashtbl.fold bind_folder statec init_val
    

  (** In this implementation, transc has type 
      (control,(control, nts_gen_rel list) t ) t. 
  *)
  let fold_transitions_container transc folder_fun init_val =
    let inner_folder external_control curr_control transit prefix =
      let ret_val = folder_fun prefix external_control  transit curr_control 
      in
      ret_val
    in
    let outter_folder external_control inner_table prefix =
      Hashtbl.fold ( inner_folder external_control) inner_table prefix
    in
    Hashtbl.fold  outter_folder transc init_val

 
  let iter_transitions_container transc iter_fun =
    let inner_iterator  external_control curr_control transit =
      iter_fun external_control  transit curr_control
    in
     let outter_iterator external_control inner_table =
       Hashtbl.iter ( inner_iterator external_control) inner_table
     in
     Hashtbl.iter  outter_iterator transc



  let pprint_inputvars cautomata = 
     Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.input_vars
      
  let pprint_outputvars cautomata =
    Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.output_vars

  let pprint_localvars cautomata =
    Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.local_vars
    
  let pretty_label tlist =
    Nts_generic.nts_pprint_gen_trans_label_list tlist
   


  let pprint_states_list l =
    let lfolder pre elem =
      match pre with
	  "" -> pprint_control elem
	| _ -> pre^","^(pprint_control elem)
    in
    List.fold_left lfolder "" l  

  let list_of_hastbl_states t =
    let lfolder var () l =
      var::l
    in
    let state_list =  Hashtbl.fold lfolder t [] in
    let sorted_state_list =
      List.sort 
	( fun s t -> 
	  begin
	    match s,t with 
		Nts_State(s),Nts_State(t) -> Param.compare_keyid s t
	  end 
	) state_list
    in
    sorted_state_list
      
  let pprint_initial_states c =
    let c_list = list_of_hastbl_states c.init_states in
    let ret_candidate = pprint_states_list  c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "initial "^ret_candidate^";"
	
  let pprint_final_states c =
    let c_list = list_of_hastbl_states c.final_states in
    let ret_candidate = pprint_states_list c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "final "^ret_candidate^";"	

  let pprint_error_states c =
    let c_list = list_of_hastbl_states c.error_states in
    let ret_candidate = pprint_states_list c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "error "^ret_candidate^";"	

 

	  
  let pprint_transitions (prescript :string) (cautomata : nts_automaton )=
    let dest_table_print_folder ( origin : control ) (dest : control ) label 
	(prescript : string ) =
      if (Nts_generic.static_check_if_gen_translist_unsat label) 
      then prescript 
      else
	begin
	  (* let label = Nts.rewrite_ndet_assignation label in *)
	  (*let label = Nts.havocise label in*)
	  let post_script = Format.sprintf "%s \n %s->%s { %s }" prescript ( pprint_control origin)  ( pprint_control dest) 
	    (pretty_label label)
	  in 
	  post_script
	end	       
    in
    let origin_table_print_folder (origin : control ) table_dest 
	(pre_script :  string ) =
      Hashtbl.fold (dest_table_print_folder origin) table_dest pre_script
    in
    Hashtbl.fold origin_table_print_folder cautomata.transitions prescript
      



  (** Returns the number of successor of a control state in a 
given automaton.*)

  let out_degree_of_control_state (control_state : control ) 
      (cautomaton : nts_automaton ) =
    let count_folder a b sharp_entry =
      sharp_entry + 1
    in
    try
      let control_table = 
	Hashtbl.find cautomaton.transitions control_state 
      in
      (Hashtbl.fold count_folder control_table 0)
    with
	Not_found -> 0
  

 

  (** This function aims at printing all the transitions in a fixed 
      order, using the lexicographical order on the couples of orig and
destination states.*)

  let pprint_transitions_lexico_sorted (prescript :string) (cautomata : nts_automaton ) =
    let lex_control c d =
      match c, d with 
	  ((Nts_State(g),Nts_State(d)),(Nts_State(l),Nts_State(r))) 
	  ->
	    begin
	      let cmpare =  Param.compare_keyid g l in 
	      if cmpare = 0 
	      then 
		begin
		  Param.compare_keyid d r
		end 
	      else 
		cmpare
	    end
    in
    let dest_table_to_list_folder ( origin : control ) (dest : control ) 
	label dest_list =
      (origin,dest,label)::dest_list 
    in
    let dest_orig_list_to_list_folder (origin : control ) dest_table  
	dest_list =
      let inner_list = Hashtbl.fold  ( dest_table_to_list_folder origin) dest_table [] 
      in
      inner_list@dest_list
    in
    
    let pprint_list_folder prepprint (origin,dest,label)  = 
      let post_script = Format.sprintf "%s \n %s->%s { %s }" prepprint ( pprint_control origin)  ( pprint_control dest) 
	(pretty_label label)
      in 
      post_script
    in
    let flat_list = 
      (Hashtbl.fold dest_orig_list_to_list_folder cautomata.transitions []) 
    in
    let sorted_list  =  List.sort 
      ( fun (g,d,_) (l,r,_) -> 
	lex_control (g,d) (l,r) 
      ) 
      flat_list 
    in
    prescript^(List.fold_left pprint_list_folder "" sorted_list) 
      
      
  
  let subst_dot_by_underscore str =
    let res = ref "" in
    let index = ref 0 in
    let len = String.length str in
    while (!index < len ) 
    do
      if (str.[!index]='.' || str.[!index]='/' ||str.[!index]='-' ) then
	res := !res^"_"
      else
	res := !res ^(String.make 1 (str.[!index]));
      index:=!index + 1
    done;
    !res
      

  let pprint_to_nts cautomata = 
      (* let current_ecfg_node = Hashtbl.get vertex current_vertex_id in *)
    
      let res_string = cautomata.nts_automata_name^"{\n" in
      let res_string = (
	if List.length cautomata.input_vars > 0 then
	res_string^"in "^(pprint_inputvars cautomata )^";\n"
	else res_string
      )
      in
      let pprint_loc = pprint_localvars cautomata in
      let res_string=res_string^"\n"^Nts.concat_if_first_arg_nonzero pprint_loc ";\n" in
    
      
      let ret_vars = pprint_outputvars cautomata in
      let res_string =  (
	if String.length ret_vars > 0 
	then res_string^"out "^ret_vars^";\n"
	else
	  res_string
      ) 
      in
      let res_string = res_string^((pprint_initial_states cautomata))^"\n"  in
      let res_string = res_string^((pprint_final_states cautomata))^"\n" in
      let res_string = res_string^((pprint_error_states cautomata)) in
     (* let res_string = res_string^((pprint_transitions "" cautomata))*)
      let res_string = (pprint_transitions_lexico_sorted res_string cautomata )
      in
      let res_string = res_string^"\n}" in
      res_string


(** This function prints all the automata of an nts w.r.t. the lexicographical
ordering on their name. *)

 let pprint_automata_lexico_sorted ( cautomata_table : 
					(string, nts_automaton ) Hashtbl.t ) =
    
    let pprint_folder prev_str (_,cautomaton) =
      match prev_str with
	   "" ->  (pprint_to_nts cautomaton) 
	| _ ->
	  begin
	    let ret_str = prev_str ^"\n"^(pprint_to_nts cautomaton)
	    in ret_str
	  end
    in
    let extract_list_folder a_name automat l  =
      (a_name,automat)::l in

    let ret_list =  Hashtbl.fold extract_list_folder cautomata_table []   in
    let ret_list = List.sort (fun (a,_) (b,_) -> String.compare a b) 
      ret_list in
    (List.fold_left pprint_folder "" ret_list)
      

 let get_outing_transitions_of cautomaton state =
   let returned_table = Hashtbl.create 7 in
   let succss_table = Hashtbl.find cautomaton.transitions state in
   let build_iterator state l =
     Hashtbl.add returned_table state l 
   in
   Hashtbl.iter  build_iterator succss_table ;
   returned_table

 let get_successor_of cautomaton state =
   let returned_table = Hashtbl.create 7 in
   let succss_table = Hashtbl.find cautomaton.transitions state in
   let build_iterator state _ =
     Hashtbl.add returned_table state () 
   in
   Hashtbl.iter  build_iterator succss_table ;
   returned_table


     
 let get_one_binding tbl =
   let gen_binding = ref None in
    let get_first_elem_iterator a b =
      gen_binding := Some( ( a , b ) );
      raise Nts_i_have_a_binding
    in
    try 
      Hashtbl.iter get_first_elem_iterator tbl;
      !gen_binding
    with
	Nts_i_have_a_binding -> !gen_binding
   

 let get_one_state tbl =
   let one_binding = get_one_binding tbl in
   match one_binding with 
       Some((a,b)) -> Some(a)
     | None -> None
   
 
(** Picks-up one transition among that which are exiting the 
control state "state" in the subsystem cautomaton.
*)

 let get_one_transition cautomaton state =
   let tbl = get_outing_transitions_of cautomaton state in
   let one_binding = get_one_binding tbl 
   in
   match one_binding with 
       Some((a,b)) -> (a,b)
     | None -> assert false 


 let compute_pred_relation cautomaton =
   let invert_table = Hashtbl.create 7 in
   let inner_relation_iterator curr_state succs _ =
     if not ( Hashtbl.mem invert_table succs )
     then
       begin
	 let succs_entry = Hashtbl.create 7 in
	 Hashtbl.add succs_entry curr_state ();
	 Hashtbl.add invert_table succs succs_entry 
       end
     else
       begin
	 let inner_table = Hashtbl.find invert_table succs in
	 if not (Hashtbl.mem inner_table curr_state) 
	 then Hashtbl.add inner_table curr_state ()
	 else ()
       end
   in
   let outer_relation_iterator curr_state succs_table =
     Hashtbl.iter (inner_relation_iterator curr_state ) succs_table
   in
   Hashtbl.iter outer_relation_iterator cautomaton.transitions;
   invert_table


 let states_container_of_states_list ( l : control list) =
   let ret_hash = Hashtbl.create 97 in
  List.iter ( fun s -> Hashtbl.add ret_hash s () ) l;
  ret_hash   



(* Converts the (control * control * tlist) list 
   into
   (control, (control, tlist) Hashtbl.t ) Hastbl.t
*)

let transitions_container_of_trans_list ( tlist :  (control * control * Nts_types.nts_trans_label list) list ) =
  let ret_hash = Hashtbl.create 97 in
  let build_iterator (c1,c2, tlabel ) =
    if not (Hashtbl.mem ret_hash c1) then
      begin
	let new_rel_hash = Hashtbl.create 97 in
	Hashtbl.add new_rel_hash c2 tlabel;
	Hashtbl.add ret_hash c1 new_rel_hash
      end
    else
      begin
	let inner_relation = Hashtbl.find ret_hash c1 in
	Hashtbl.add inner_relation c2 tlabel
      end
  in
  List.iter build_iterator tlist; ret_hash


  let pprint_all_cautomata cautomata_table =
    let pprint_automata_folder cname cautomaton prev_str =
      match prev_str with
	  "" ->  (pprint_to_nts cautomaton) 
	| _ ->
	  begin
	    let ret_str = prev_str ^"\n"^(pprint_to_nts cautomaton)
	    in ret_str
	  end
    in
    Hashtbl.fold pprint_automata_folder cautomata_table "" 



 let pprint_nts_init_condition_folder pre ntrel =
    match pre with
        "" -> (Nts_generic.nts_pprint_genrel ntrel)
      | _ -> pre^" and "^(Nts_generic.nts_pprint_genrel ntrel)


  let pprint_optional_init nt_sys prefix =
    match nt_sys.nts_gvars_init with

      | Some([]) -> prefix
      | Some(l) ->
          begin
            let cnd = List.fold_left  pprint_nts_init_condition_folder "" l in
              Format.sprintf "%sinit %s;\n" prefix cnd
          end
      | None -> prefix



  let pprint_optional_thread_list nt_sys prefix =
    let thread_pprint_iterator ret_string (thread_name, nb_instances) =
      match ret_string with
	  "" -> Format.sprintf "instances %s [%s]" 
	    thread_name (Big_int.string_of_big_int nb_instances)
	| _ -> Format.sprintf "%s,%s [%s]" 
	  ret_string thread_name (Big_int.string_of_big_int nb_instances) 
    in
    match nt_sys.nts_system_threads  with
	Some(thread_list ) ->
	  let str = List.fold_left thread_pprint_iterator "" thread_list in
	  Format.sprintf "%s%s;\n" prefix str 
      | None -> prefix
	


  let pprint_nts nt_system =
    let ret_string =  Format.sprintf "nts %s ; \n"
      (subst_dot_by_underscore nt_system.nts_system_name) 
    in 
    let gvars_pprint =
      Nts_generic.pprint_typeinfo_nts_genvar_list nt_system.nts_global_vars 
    in
    let gvars_pprint = 
      (
      if String.length gvars_pprint > 0
      then gvars_pprint^";\n"
      else
	""
      )
    in
    let ret_string= ret_string^gvars_pprint
    in
    let ret_string= pprint_optional_init nt_system ret_string
    in
    let ret_string = pprint_optional_thread_list nt_system ret_string
    in
    (*
    let all_automata = pprint_all_cautomata  nt_system.nts_automata
    *)
    let all_automata = pprint_automata_lexico_sorted nt_system.nts_automata
    in
    ret_string^all_automata^"\n"
    
   

  (**

     One propose a set of functions that get rid of declared variables
     that don't appear in the transtions
     
  *)

  let locally_used_variables diary nt_aut =
    let log_var_in_transitions_iterator _ nts_translabel =
      Simplification.add_vars_of_trans_label_list_to_diary diary nts_translabel
    in
    let trans_table_iterator _ inner_table =
      Hashtbl.iter log_var_in_transitions_iterator inner_table
    in
    Hashtbl.iter trans_table_iterator nt_aut.transitions
    ;
    Format.printf "[locally_used_variables] \n"; (Simplification.pprint_diary diary)





 
  
  let update_local_list nt_aut loc_vars_list =
    {
      nts_automata_name=nt_aut.nts_automata_name;
      anot=nt_aut.anot;
      init_states=nt_aut.init_states;
      final_states=nt_aut.final_states;
      error_states=nt_aut.error_states;
      input_vars = nt_aut.input_vars; 
      output_vars = nt_aut.output_vars;
      local_vars = loc_vars_list; 
      transitions = nt_aut.transitions;
    }



  let nts_sys_with_update_cautomaton_table nt_sys ctable =
    {
      nts_system_name = nt_sys.nts_system_name ;
      nts_global_vars = nt_sys.nts_global_vars ;
      nts_automata = ctable ; 
      nts_gvars_init = nt_sys.nts_gvars_init ; 
      nts_system_threads = nt_sys.nts_system_threads;  
    }


  (**
     This functions removes the local variables of an automaton
     if if they are not listed in the diary.
  *)


  let clean_unlisted_local_vars  nt_aut =
   
    let local_list_folder diary glist gvar =
      if (contains_nts_genrel_var diary gvar) 
      then 
	gvar::glist
      else
	begin
	  Format.printf "eliminating variable %s \n" (Nts_generic.nts_pprint_genvar  gvar);
	  glist
	end
	
    in

    let diary = create_empty_var_diary () in 
    locally_used_variables diary nt_aut;
    
    let clean_local_list =
      (List.fold_left ( local_list_folder diary) []  nt_aut.local_vars )
    in
    update_local_list nt_aut clean_local_list

      
      
  let clean_unlisted_vars_on_all_system_table nt_system =    
    let cleaner_folder cname nt_aut n_table =
      let local_diary = create_empty_var_diary () 
      in
      locally_used_variables local_diary nt_aut; 
      (* Fills diarry with used
	 variables*)
      let clean_entry = clean_unlisted_local_vars  nt_aut
      in
      Hashtbl.add n_table cname clean_entry; n_table
    (* Modify each automaton
       within the hashtbl.*)
    in
    let new_table = Hashtbl.create 97 in
    Hashtbl.fold cleaner_folder nt_system.nts_automata new_table
    
      

  (** Adds the name of the called subsystems in cautomaton, in called_fun,
  if the former are not already referenced.*)

  let register_called_subsystem called_fun cautomaton =
   
    let called_fun_of_transition _ translist _ =
      (List.iter (fun s -> Simplification.register_called_subsystems called_fun s) translist)
    in
    iter_transitions_container  cautomaton.transitions called_fun_of_transition  
    


      
  let reference_called_nts nt_system =
    let called_fun = Simplification.create_fun_name_in_call_table () 
    in
    Simplification.add_fun_name_in_call_table called_fun "main";
    let register_callees_of_each_subsystems _ subs =
      register_called_subsystem called_fun subs 
    in
    Hashtbl.iter register_callees_of_each_subsystems nt_system.nts_automata;
    called_fun
    
    
    
    
    
  
(**
Returns as input a numerical transition system in which all local variables
list of each automaton has been cleared of non used varibles
*)
  let nt_system_var_cleaner nt_sys =
    let clean_system_table = clean_unlisted_vars_on_all_system_table nt_sys
    in
    nts_sys_with_update_cautomaton_table nt_sys clean_system_table
    
 




  let c_table_having_keys_in ctable called_fun =
    let new_table = Hashtbl.create 97 in
    let fill_clean_table name cautomaton =
      if Simplification.is_name_in_call_table called_fun name 
      then Hashtbl.add new_table  name cautomaton
      else ()
    in
    Hashtbl.iter fill_clean_table ctable;
    new_table


(**
 This function returns a numerical transition system where all the subsystem
are called at some point. Calls might be performed from the same subsystem.
*)

  let nt_system_uncalled_subsystem_cleaner nt_sys =
    let called_fun = reference_called_nts nt_sys in
    let cleaned_call_table =  
      c_table_having_keys_in nt_sys.nts_automata called_fun 
    in
    nts_sys_with_update_cautomaton_table nt_sys cleaned_call_table
    

    




(**

Typing and typechecking section.

*)

  exception UndefinedVariable of string * nts_automaton
  exception UndefinedSubsystem of string

  exception ArgumentTypesMissMatch of nts_gen_relation
  (** Wrong number of arguments or arguments types don't
  match the subsystem call signature.*)

  exception TypeErrorInVar of nts_genrel_var
  exception TypeErrorInArithmExpr of nts_genrel_arithm_exp
  exception TypeErrorInNtsGenRelation of nts_gen_relation


  

  (** Try to find a varible definition in the sybsystem c of
      the nts n*)
  exception MultipleDeclarationOfVar of nts_genrel_var * nts_automaton
      
  let get_definition_of_variable_by_name n c vname =
    
    let is_some t =
      match t with 
	  None -> true
	| Some(_) -> false
    in
    let come_get_some t =
      match t with
	  Some(t) -> t
	| None -> assert false
    in
    let check_unicity elem itemfound =
      match elem with 
	  None -> itemfound
	| Some(v) ->
	  begin
	    if is_some itemfound then
	      raise (MultipleDeclarationOfVar(v,c))
	    else 
	      elem
	  end
    in
    let rec find_var_by_name name vlist =
      match vlist with
	  (NtsGenVar(NtsVar(vname,_),_) as hl)::l ->
	    begin
	      if String.compare vname name = 0 then
		Some(hl)
	      else
		find_var_by_name name l
	    end
	| [] -> None
    in
    let res1 = check_unicity (find_var_by_name vname c.input_vars) 
      None
    in
    let res1 = check_unicity (find_var_by_name vname c.output_vars) 
      res1 in
    let res1 = check_unicity (find_var_by_name vname c.local_vars)
      res1 in
    if is_some res1 then
      come_get_some res1 
    else
      begin
	let res1 = find_var_by_name vname n.nts_global_vars in
	if  is_some res1 then
	  come_get_some res1 
	else
	  raise ( UndefinedVariable ( vname , c))
      end

	
	
  (** Need to be moved in module Nts*)
  let nts_var_of_nts_genvar gv =
    match gv with
	NtsGenVar(v,_)-> v
	    
  (** Need to be moved in Nts module*)
  let has_type v t =
    match v with
	NtsGenVar(NtsVar(vname,typedef),priminfo) ->
	  if typedef = t then
	    true
	  else
	    false
	      
    (** This function might raise an exception whenever v is not
	a defined variable within the current context of c. *)
  let type_ntsgen_var n c v =
    match v with
	NtsGenVar(NtsVar(vname,NtsUnTyped),priminfo) ->
	  begin
	      (** No type is yet associated to this variable, hence
		  we need to look forward to its definition in 
		  the automaton.*)
	    let typedvar_from_def = 
	      get_definition_of_variable_by_name n c vname
	    in
	    let v= nts_var_of_nts_genvar typedvar_from_def in
	    NtsGenVar(v,priminfo)
	  end
	    
      |  NtsGenVar(NtsVar(vname,typedef),priminfo) ->
	begin
	  let typedvar_from_def = 
	    get_definition_of_variable_by_name n c vname
	  in
	  if has_type typedvar_from_def typedef then v
	    (** If well typed, then one returns the same variable*)
	  else
	    raise (TypeErrorInVar(v))
	  (** Aborting upon type check failure*)
	  end
	      
	      
    (** Returns the type of constant *)
  let gentype_of_cst c =
    match c with
	CntGenICst (_) -> NtsIntType
      | CntGenFCst(_) -> NtsRealType
      | CntGenBCst (_) -> NtsBoolType	

  (** Bottom to typecheck : One shall make sure that each subtrees of
      an arithemtical expression is : typed and all subtree that are
      shared by an expression node have the same type.
  *)
	  


  let rec type_gen_arithm_expressions n c exp =
    CntGenCst( base_cst , btype) ->
    begin
      match type with
	  NtsUnTyped -> 
	    let btype = gentype_of_cst base_cst in
	    CntGenCst( base_cst , btype)
	| _ ->
	  begin
	    let t = gentype_of_cst base_cst in
	    if t = btype then
	      exp
	    else 
	      raise  (TypeErrorInNtsGenRelation (exp) )
	  end
    end
      
      | CntGenSymCst(symcst,btype) ->
	begin
	  
	end
	  
      | CntGenVar(v) -> 
	begin
	  let v = type_ntsgen_var n c v in
	  CntGenVar(v)
	end
      
      |  CntGenArithmBOp (bop,eg,ed,t) ->
	begin
	  
	end

      |  CntGenArithmUOp( uop , exp , t ) ->
	begin
	end

    



  (** Types and functions used to generate a control flow graph
      from the numerical transition system description*)
      

  type nts_basic_block = {
    mutable head_label : string ;
    mutable block : (control * nts_trans_label list) list; 
    (** Current control state,
	nts_trans_label_list corresponds
	to what changes/is called before
	transiting*)
    
    mutable block_succs : ( nts_basic_block ref * nts_trans_label list ) list option;
    (** transitions between blocks. Nexts blocks and the transisions being
	described.
	None is in the case the last control state is an error state.
	It's also a convenience for the buiding process.   
    *) 
  } 
    
    
  type nts_automaton_cfg = {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , unit ) Hashtbl.t;
    nts_cfg_final_block : (string , unit ) Hashtbl.t;
    nts_cfg_error_block : (string , unit ) Hashtbl.t;
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_block ) Hashtbl.t
  }




    

end
