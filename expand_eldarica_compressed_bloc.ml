open Ntsint
open Nts_functor
open Nts_generic
open Trace_types

(* Dotofintnts.dot_of_subcfg_of_nts nt_system trace_l *)


module Make =
  functor (Param : Nts_functor.NTS_PARAM ) ->
struct 



  module NFParam=Nts_functor.Make(Param)
  type anotations = NFParam.anotations
  type control = NFParam.control
    
  type nts_automaton = NFParam.nts_automaton
  type nts_system = NFParam.nts_system
    
  let control_out_of_string = Param.key_val_of_string
  

  type subsystem_call_count = (nts_automaton , int ref) Hashtbl.t
    

  open NFParam

  
  (**
     function nts_out_of_subrealtion :

      Pretty prints the collection of the transitions rules 
      that describe the transition relation. No variables definition,
      and no information concerning the various states are provides --
      no info concerning wheter which one are initial, final or 
      error state.

     main use : Called by nts_out_of_subrelation.
  *)
  
  let nts_out_of_subrelation subrel =
    NFParam.pprint_subgraph_transitions subrel
 

  (**  *)
  let get_tran s_relation_max_min (ca : nts_automaton) ( max : control ) 
      ( min : control ) =
    let subgraph = NFParam.subgraph_between ca max min 
    in nts_out_of_subrelation subgraph
  
  
  





(*let pprint_subgraph_between_ctr_pair_folder 
	   nt (pre_str,pre_sysc) curr_sysc =
	
	match pre_sysc with
	  None -> (pre_str,Some(curr_sysc)) (*No previous state visted*)
	| Some(Sys_control(ca_name,_) as prev_sysc ) 


let nts_out_of_subrelation prefix ca subrel =
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

let get_trans_relation_max_min (ca : nts_automaton) ( max : control ) 
    ( min : control ) =
  let subgraph = subgraph_between ca max min 
  in 
*)
  





  let get_contextual_transitions_of_subgraph context subgraph =
    let contextual_folder ctlist corg label cdest =
      (context,(corg,label,cdest))::ctlist
    in
    let inv_list = NFParam.fold_transitions_container 
       subgraph.sub_transitions contextual_folder  []
    in
    List.rev inv_list



(** 

*)
 (* let get_single_instruction_interval_content  *) 

(**

*)




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



  let get_contextual_transition_list_from_pair_folder
      nts_lib nt  (pre_context_tlist,pre_sysc) curr_sysc =
    
    let _build_list_on_nts_param nts_param =
      match pre_sysc with
	None -> (pre_context_tlist,Some(curr_sysc)) (*No previous state visted*)
      | Some(Sys_control(ca_name,_) as prev_sysc ) 
	->
	begin
	  if (String.compare ca_name (ca_name_of_syscontrol curr_sysc))
	    <> 0 
	  then  
	    begin
	  
	      (pre_context_tlist,Some(curr_sysc)) (* Current state and previous
						  one don't belong to the same 
						     subsystem. It is a call to a subsystem.*)
		
	    end
	  else
	    begin
	      let max_c = control_of_syscontrol prev_sysc in 
	      let min_c = control_of_syscontrol curr_sysc in
	      let ca = ca_of_syscontrol nts_param curr_sysc in
	      if not (NFParam.is_successor_of ca max_c min_c) then
		begin
		  let subgraph = NFParam.subgraph_between ca max_c min_c
		  in
		  let interval_content = 
		    get_contextual_transitions_of_subgraph ca subgraph
		  in
		  (interval_content,Some(curr_sysc))
		end
	      else
		begin
		  let transition_labels = NFParam.get_transition_from
		    ca max_c min_c in
		  let interval_content = 
		    (
		      match transition_labels
		      with 
			Some(ll) ->
			  let label = List.hd ll in
			  ( ca , (max_c,label, min_c))::[]
			  
		      | None -> []
		    ) 
		  in
		  (interval_content,Some(curr_sysc))
		end
	    end

	end 
    in
    try
      _build_list_on_nts_param nt
    with
      No_such_counter_automata_in_nts_system(_,_) ->
	_build_list_on_nts_param nts_lib
    | other_ex -> raise other_ex    






(** In this function, the parameter nts_out nt_sytems_build_container 
    describes the generated transition system that will ultimately
    be exported for flatac.

 nts_lib is the set of functions that does not correspond to compiled
 C code and 
*)

(*
let get_nts_from_ctr_pair_mapper
    nts_lib nt  (  trace_system : (string, nts_automaton ) Hashtbl.t ) (pre_str,pre_sysc) curr_sysc =
	
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
    *)

(*
let rec compile_trace_into_nts nts_lib nt call_counts trace_system (pre_str,pre_sysc) = 
*)

end;;





(*
let flata_nts_of_eldarica_incomplete_trace  (opt_lib : nts_system option) (nt : nts_system ) ( tr : Trace_types.trace ) =
	
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
0	  Format.sprintf "digraph %s { %s" nt.nts_system_name automata_dump
	in
	let (printout_hgraph,_) = 
	  List.fold_left (pprint_subgraph_between_ctr_pair_folder nt ) 
	    ("",None) tr
	in
	Format.sprintf "%s%s}" ret_string printout_hgraph


end;;
*)
