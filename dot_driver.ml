open Nts_functor
open Nts_types
open Hashtbl
open Nts_generic


module Make = 
  functor ( Param  : Nts_functor.NTS_PARAM ) ->
    struct
      module NFParam=Nts_functor.Make(Param)
      type anotations = NFParam.anotations
      type control = NFParam.control
	  
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system

      open NFParam


      let pprint_control = NFParam.pprint_control
	
      let rec get_opt_transition ntlist =
	match ntlist with
	    (CntGenCall ( _, _, _) as h)::l -> Some(h)
	  | _::l -> get_opt_transition l
	  | [] -> None
	    
	       
	       
	    

      let dot_of_init_nodes (ca : nts_automaton) =
	let init_state_printer  prefix control =
	  Format.sprintf "%s %s_%s [style=filled,color=blue];\n" 
	    prefix ca.NFParam.nts_automata_name  (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.init_states init_state_printer  ""
	  



      let dot_of_final_nodes (ca : nts_automaton) =
	let final_state_printer prefix control =
	  Format.sprintf "%s%s_%s[style=filled,color=green];\n" 
	    prefix ca.NFParam.nts_automata_name (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.NFParam.final_states final_state_printer  ""
	  

	  
      let dot_of_error_nodes_reach_upb (ca : nts_automaton ) invtable =
	let error_printer  prefix control =
	  if NFParam.is_state_in_inv_relation invtable control 
	  then
	    begin
	      Format.sprintf "%s %s_%s [style=filled,color=red];\n" 
		prefix ca.NFParam.nts_automata_name  (pprint_control control)
	    end
	  else
	    prefix
	in
	NFParam.fold_states_containers 
	  ca.NFParam.error_states error_printer ""
	  
	  
      let dot_of_error_nodes (ca : nts_automaton) =
	let in_folder  prefix control =
	  Format.sprintf "%s%s_%s[label=\"error\",color=red];\n" 
	    prefix ca.NFParam.nts_automata_name (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.NFParam.final_states in_folder  ""
	  
	  
     

      let dot_of_transitions  (ca : nts_automaton ) prefix =
	let transition_printer prefix control_org l control_dest =
	  let opt_call = get_opt_transition l in
	  match opt_call with
	      None ->
		Format.sprintf "%s%s_%s->%s_%s;\n" prefix 
		  ca.NFParam.nts_automata_name 
		  (pprint_control control_org) 
		  ca.nts_automata_name (pprint_control control_dest)
	    | Some(CntGenCall ( sysname, _, _)) -> 
	      begin
		Format.sprintf "%s%s_%s->%s_%s[color=red,label=\"call to %s \"];\n" prefix 
		  ca.NFParam.nts_automata_name 
		  (pprint_control control_org) 
		  ca.nts_automata_name (pprint_control control_dest)
		  sysname
	      end
	      
	in 
	NFParam.fold_transitions_container 
	  ca.transitions transition_printer prefix
	  

      let dot_of_cautomaton ?(standalone_graph = false) (ca : nts_automaton )=
	
	let ret_str = ( if standalone_graph then
	    Format.sprintf " digraph %s {" ca.nts_automata_name 
	  else 
	    Format.sprintf " subgraph cluster_%s { \n color=blue; label=\"%s\";" ca.nts_automata_name ca.nts_automata_name
	)
	in
	
	let invtable = NFParam.compute_pred_relation ca 
	in
	
	let res = Format.sprintf "%s %s %s %s %s }" ret_str
	  (dot_of_init_nodes ca) (dot_of_error_nodes_reach_upb ca invtable)
	  (dot_of_final_nodes ca) (dot_of_transitions ca "")
	in 
	res
	
	

      let dot_of_nts (nt : nts_system ) =
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s }" nt.nts_system_name automata_dump
	in
	ret_string
	
	
	  
	  
end;;
