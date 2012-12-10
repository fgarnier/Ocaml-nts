open Nts_functor
open Nts_types
open Hashtbl
open Nts_generic
open Trace_types


module Make = 
  functor ( Param  : Nts_functor.NTS_PARAM ) ->
    struct
      module NFParam=Nts_functor.Make(Param)
      type anotations = NFParam.anotations
      type control = NFParam.control
	  
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system
	
      let control_out_of_string = Param.key_val_of_string
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
	  

      (** Prints error states in red.*)  
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
	  
	  
	  
(** subrel is a subset of the transition of the counter automaton ca.
 gv color is an optional string.
*)
      let dot_of_subgraph ?(color="greenyellow") prefix ca  subrel =
	let transition_printer prefix control_org l control_dest =   
	  Format.sprintf "%s%s_%s->%s_%s[color=%s];\n"
	    prefix 
	    ca.NFParam.nts_automata_name 
	    (pprint_control control_org) 
	    ca.nts_automata_name (pprint_control control_dest)
	     color
	in
	NFParam.fold_transitions_container 
	  subrel transition_printer prefix
	

    (** Transition printer folder*)
      let transition_printer ca prefix control_org l control_dest =
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
	    | Some(_) -> assert false
	      


      (** Interprocedural calls are represented using a red transition 
      label.*)
      let dot_of_transitions  (ca : nts_automaton ) prefix =
	(*let transition_printer prefix control_org l control_dest =
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
	    | Some(_) -> assert false
	      
	in*)
	let transition_printer = transition_printer ca in
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
	

      let pprint_sys_control s =
	match s with
	    Trace_types.Sys_control(sname,cname) ->
	      sname^"_"^cname 
		 

      let pprint_trace_tansitions tr =
	let pprint_trace_transitions_folder (prefix_printer,previous_state) 
	    curr_control =
	  match previous_state with 
	      None -> 
		("",Some(curr_control))
	    | Some(prev) -> 
	      begin
		let out_string = Format.sprintf "%s %s -> %s [color=gold]\n" prefix_printer 
		(pprint_sys_control prev) (pprint_sys_control curr_control) in
		(out_string,Some(curr_control))
	      end
	in
	let  (ret_string, _ ) = 
	  List.fold_left pprint_trace_transitions_folder ("",None) tr 
	in
	ret_string
	  
	
      let dot_of_trace_upon_nts (nt : nts_system ) (tr : Trace_types.trace ) =
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s" nt.nts_system_name automata_dump
	in
	let trace_transitions =  pprint_trace_tansitions tr in
	Format.sprintf "%s %s } " ret_string trace_transitions


      let control_of_syscontrol sysc =
	match sysc with 
	  Trace_types.Sys_control(_,s) ->
	    let s_val = control_out_of_string s 
	    in
	    NFParam.Nts_State(s_val)
     
	      
      let ca_name_of_syscontrol sysc =
	match sysc with 
	  Trace_types.Sys_control(caname,_) -> caname
	    
      let ca_of_syscontrol nt sysc =
	match sysc with 
	  Trace_types.Sys_control(sysname,_) ->
	    NFParam.get_cautomaton_by_name nt sysname


      let highlight_graph_between  (ca : nts_automaton) (max : control) 
	  ( min : control) =
	let subgraph = subgraph_between ca max min 
	in
	dot_of_subgraph "" ca subgraph.sub_transitions 


      (** Computes and then concatenates the sequence of arrows that
      belong to the different control points of a trace, whenever
      two or more belong to the same nts automaton. If  *)

      let pprint_subgraph_between_ctr_pair_folder 
	  nt (pre_str,pre_sysc) curr_sysc =
	
	match pre_sysc with
	  None -> (pre_str,Some(curr_sysc)) (*No previous state visted*)
	| Some(Sys_control(ca_name,_) as prev_sysc ) 
	  ->
	  begin
	    if (String.compare ca_name (ca_name_of_syscontrol curr_sysc))
	      <> 0 
	    then  (pre_str,Some(curr_sysc)) (* Current state and previous
					       one don't belong to the same 
					       subsystem*)
	    else
	      begin
		try
		  let max_c = control_of_syscontrol prev_sysc in 
		  let min_c = control_of_syscontrol curr_sysc in
		  let ca = ca_of_syscontrol nt curr_sysc in
		  if not (NFParam.is_successor_of ca max_c min_c) then
		  begin
		    let gprint_out = highlight_graph_between ca max_c min_c
		    in
		    let suffix = Format.sprintf "%s%s\n" pre_str gprint_out in
		    (suffix,Some(curr_sysc))
		  end
		  else
		    begin
		      let transition_labels = NFParam.get_transition_from
			ca max_c min_c in
		      let print_out = 
			(
			  match transition_labels
			  with 
			    Some(ll) ->
			      let label = List.hd ll in
			      transition_printer ca pre_str max_c label min_c
			
			  | None -> ""
			) 
		      in
		      (print_out,Some(curr_sysc))
		    end
		    
		with
		  No_such_counter_automata_in_nts_system(_,_) ->
		    (pre_str,None)
		  | other_ex -> raise other_ex    
	      end
	  end


      let dot_of_subcfg_of_nts (nt : nts_system ) ( tr : Trace_types.trace ) =
	
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s" nt.nts_system_name automata_dump
	in
	let (printout_hgraph,_) = 
	  List.fold_left (pprint_subgraph_between_ctr_pair_folder nt ) 
	    ("",None) tr
	in
	Format.sprintf "%s%s}" ret_string printout_hgraph

	
	
end;;
