open Ntsint
open Nts_functor
open Nts_generic
open Nts_types
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
  


  let get_contextual_transitions_of_subgraph context subgraph =
    let contextual_folder ctlist corg label cdest =
      (context,(corg,label,cdest))::ctlist
    in
    let inv_list = NFParam.fold_transitions_container 
       subgraph.sub_transitions contextual_folder  []
    in
    List.rev inv_list


  let debug_pprint_syscontrol sysc =
    match sysc with 
      Trace_types.Sys_control(c,s) ->
	Format.sprintf "Cautomaton :%s, state :%s" c s
	  
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
	  


  let rec is_transition_a_call l =
    match l with
      CntGenCall(sysname,opt_ret,params)::_ -> true
    | _::tl -> is_transition_a_call tl	
    | [] -> false
    

 (* let is_new_context ca_def (corg,_,_) =
     List.mem ca_def.NFParam.init_states cdest
 *)
  let is_a_return ca_def (_,_,cdest) =
   NFParam.is_final_state ca_def cdest  
  
 



  (*let get_one_transition_l nts_lib nt =*)
    

  (**

     nts_lib is collection of nts automaton that compose the nts
     library.

     nt is another library. It is reasonable to think that both
     libraries might be merged into a single one in a next version.
*)

  let get_contextual_transition_list_from_pair_folder
      nts_lib nt  (pre_context_tlist,pre_sysc) curr_sysc =
    
    let _build_list_on_nts_param nts_param =
      match pre_sysc with
	None -> (pre_context_tlist,Some(curr_sysc)) (*No previous state visted*)
      | Some(Sys_control(ca_name,_) as prev_sysc ) 
	->
	begin
	  if (String.compare ca_name (ca_name_of_syscontrol curr_sysc))
	    != 0 
	  then  
	    begin
	      Format.printf "[Debug] context name change detected : Old context %s, new context %s \n %!" (debug_pprint_syscontrol prev_sysc) (debug_pprint_syscontrol curr_sysc);
	      
	      
		  let max_c = control_of_syscontrol prev_sysc in
		  
		  let ca =
		    (
		      try
			ca_of_syscontrol nt prev_sysc
		      with
			Not_found -> ca_of_syscontrol nts_lib prev_sysc
		    )
		  in
		  if not (is_a_return ca ((),(),max_c))
		  then 
		    begin
		      let (dest,l) = get_one_transition ca max_c  
		      in
		      Format.printf "Label is %s \n" (nts_pprint_gen_trans_label_list l);
		  
		      (pre_context_tlist@((ca,(max_c,l,dest))::[]),Some(curr_sysc))
		    end
		  else
		    (pre_context_tlist,Some(curr_sysc)) (* Current state and previous
						  one don't belong to the same 
						     subsystem. It is call return to a subsystem.*)
				
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
		  (pre_context_tlist@interval_content,Some(curr_sysc))
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
		  (pre_context_tlist@interval_content,Some(curr_sysc))
		end
	    end

	end 
    in
    try
      _build_list_on_nts_param nt
    with
      No_such_counter_automata_in_nts_system(_,_) ->
	_build_list_on_nts_param nts_lib
    | other_ex -> 
      raise other_ex    



  let contextual_transition_list_of_trace nts_lib nt tr =
    let (res,_)=
      List.fold_left 
	(get_contextual_transition_list_from_pair_folder nts_lib nt) 
	([],None) tr
    in
    res
  


(**
 A context is defined as a nts_cautomaton and a context_id.
*)


  let initial_context_of_ctl_list tr =
    let (ca,_) = List.hd tr in
    (ca,0)


  let nts_subsystem_of_ca_cid ca_name cid =
    Format.sprintf "%s_%d" ca_name cid


(** Updates the name of the called subsystem so that it matches the
    subsystem generated from the trace itself.
*)

 
  let contextual_call_of_subsystem l (usid_counter : int ref) =
    Format.printf "contextual call of susbsystem id : %d \n%!" !usid_counter;
    match l with
      CntGenCall(sysname,opt_ret,params)::tl ->
	begin
	  Format.printf "[DEBUG] call function %s \n" sysname;
	  usid_counter := !usid_counter + 1;
	  let contextual_sysname = nts_subsystem_of_ca_cid sysname 
	    !usid_counter  
	  in
	  (CntGenCall(contextual_sysname,opt_ret,params)::tl)
	end
    
    | _ -> l

      
(** In this function, the parameter nts_out nt_sytems_build_container 
    describes the generated transition system that will ultimately
    be exported for flatac.

 nts_lib is the set of functions that does not correspond to compiled
 C code and 
*)



  let get_called_subsystem_name l =
    match l with
      CntGenCall(sysname,_,_)::_ ->
	begin
	  sysname
	end	  
    | _ ->  assert false
      
      
  let get_ca_by_name nts_lib nt name =
    try
      Hashtbl.find nt.nts_automata name
    with
      Not_found ->
	Hashtbl.find nts_lib.nts_automata name


  let definition_of_called_ca nts_lib nt l =
    let ca_name = get_called_subsystem_name l in
    get_ca_by_name nts_lib nt ca_name
	  
	  

  let new_context_table_entry catable uid ca_def=
    Hashtbl.add catable uid (ca_def,[])

  let is_context_switch_ahead curr_context_ca next_op_list =
    match next_op_list with
      (ca,_)::_ -> curr_context_ca == ca
    | [] -> assert false

  let empty_tail tl =
    match tl with
      [] -> true
    | _ -> false

  let add_transtion_in_contextual_trans_sys context_table ca_param uid trans =
    if Hashtbl.mem context_table uid 
    then
      begin
	let (ca,tlist) = Hashtbl.find context_table uid in
	Hashtbl.replace context_table uid (ca,tlist@(trans::[]))
      end
    else
      begin
	Hashtbl.add context_table uid (ca_param,(trans::[]))
      end
(**)


  let nts_of_transitions_rules_container tbl =
    let sys_table = Hashtbl.create 97 in
    let mapper (corg,l,cdest) =
      (corg,cdest,l)
    in
    let trans_table_iterator context_id (ca_def,tlist) =
      let tlist_map = List.map mapper tlist in
      let context_sysname = nts_subsystem_of_ca_cid ca_def.nts_automata_name 
	context_id in
      let subrel = transitions_container_of_trans_list tlist_map in
      let context_cautomaton = NFParam.cautomaton_of_transitions_container
	 context_sysname  ca_def subrel in
      Hashtbl.add sys_table context_sysname context_cautomaton
    in
    Hashtbl.iter trans_table_iterator tbl;
    sys_table
  
    

  let build_nts_table_from_contextual_trace nts_lib nt tr =
    
    Format.printf "[Debug] trace length : %d \n" (List.length tr) ;
    
    let context_uid = ref 0 in (* Add one to this variable each time
			       a call is performed.*)
    let context_table = Hashtbl.create 97 in (* (int  , ( ca, (control, trans list, control))) Hashtbl.t *)
    let contextual_transition_list = 
      contextual_transition_list_of_trace nts_lib nt tr in
    let current_context = initial_context_of_ctl_list 
      contextual_transition_list in 
    let context_stack = Stack.create () in
    Stack.push current_context context_stack;


    
    
    let rec build_ctl_iterator ctl = 
      Format.printf "[Debug] Iterating on ctl list, List length %d \n" (List.length ctl);
      let ( current_context_ca,current_cid) = 
	Stack.top context_stack 
      in
      (** 
	  Two transitions migth lead to a final state, that's why
	  I need to look ahead in the tail to be sure that the next
	  transition is not within the same context, before removing
	  the current context description from the stack.
      *)

      match ctl with 	    
	(ca,((corg,l,dest) as tlabel))::tl ->
	  begin
	    Format.printf "Label is %s \n" (nts_pprint_gen_trans_label_list l);
	    if ( is_transition_a_call l )
	    then
	      begin
		let called_subsystem_definition =  
		  definition_of_called_ca nts_lib nt l 
		in
		let l = contextual_call_of_subsystem l context_uid 
		in
		add_transtion_in_contextual_trans_sys 
		  context_table ca current_cid (corg,l,dest);
		let new_context = 
		  (called_subsystem_definition,!context_uid) in
		Stack.push new_context context_stack;
		Format.printf "[Debug]: Got a push \n"
	      (* Create a new context, and push it on the top of
		 the stack *)
	      end
	   
	    else
	      begin
		Format.printf "Transition is not a call \n";
		add_transtion_in_contextual_trans_sys 
		  context_table ca current_cid (corg,l,dest) ;
		if is_a_return ca tlabel 
		then  
		  begin
		    if ( empty_tail tl) then
		      (*let  _ = Stack.pop context_stack in *)
		      ()
 		    else 
		      begin
			if ( is_context_switch_ahead ca tl ) 
			then 
			(*let  _ = Stack.pop context_stack
			in*) Format.printf "[Debug] Got a pop \n"
			else 
			  ()
		      end
		  end
	      end
	  end;
	  build_ctl_iterator tl (*recursion upon tail list.*)
      | [] -> ()
    in
    build_ctl_iterator contextual_transition_list;
    let nts_sys_table = 
      nts_of_transitions_rules_container context_table
    in
    nts_sys_table




  let nts_out_trace nts_lib nt tr =
    let trans_table = build_nts_table_from_contextual_trace nts_lib nt tr
    in
    {
      NFParam.nts_system_name = "debug_trace_system";
      NFParam.nts_global_vars = nt.NFParam.nts_global_vars;
      NFParam.nts_automata = trans_table ;
      NFParam.nts_gvars_init = nt.NFParam.nts_gvars_init ;
      NFParam.nts_system_threads = None
    }
    

end;;







