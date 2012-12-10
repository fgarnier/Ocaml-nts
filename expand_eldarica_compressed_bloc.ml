open Ntsint
open Nts_functor
open Nts_generic
open Traces_types

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
  



(** In this function, the parameter nts_out nt_sytems_build_container 
    describes the generated transition system that will ultimately
    be exported for flatac.

 nts_lib is the set of functions that does not correspond to compiled
 C code and 
*)


let get_nts_from_ctr_pair_folder 
    nts_lib nt call_counts (  trace_system : (string, nts_automaton ) Hashtbl.t ) (pre_str,pre_sysc) curr_sysc =
	
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
	    let gprint_out = highlight_graph_between ca max_c min_c
	    in
	    let suffix = Format.sprintf "%s%s\n" pre_str gprint_out in
	    (suffix,Some(curr_sysc))
	  with
	    No_such_counter_automata_in_nts_system(_,_) ->
	      (pre_str,None)
	  | other_ex -> raise other_ex    
	end
    end



let rec compile_trace_into_nts nts_lib nt call_counts trace_system (pre_str,pre_sysc) = 
  





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
