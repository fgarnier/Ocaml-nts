open Trace_types
open Trace


type control_type = FC_ESID of int
  | FLATAC_SINTER_SID of int
  | FLATAC_SINIT

exception UnknownESidDescriptor of string
exception Unreferenced_esid_in of esid * (esid,sid) Hashtbl.t
exception NoSubsystem of string * tr_subsystem_table

(**  *)



let type_statename_1 name =
  let namelen = String.length name in
  let pref = String.sub name 0 1 in
  if (String.compare pref "s" ) = 0 then 
    begin 
      let suffix = String.sub name 1 (namelen-1) in
      let id = 
	( try int_of_string suffix 
	  with
	  | e -> 
	    begin 
	      Format.printf "name : %s, suffix: %s%! \n Fatal error." name suffix;
	      raise e
	    end
	)	    
      in
      FC_ESID(id)
    end
  else raise (UnknownESidDescriptor name)
  
let type_statename_5 name =
  let namelen = String.length name in
  if namelen < 5 then type_statename_1 name 
  else
    begin
      let pref = String.sub name 0 5 in
      if (String.compare pref "sinit")=0 then
	FLATAC_SINIT
		
      else type_statename_1 name
    end
  
  
let type_statename_6 name =
  let namelen = String.length name in
  if namelen < 6 then type_statename_5 name 
  else 
    begin
      let pref = String.sub name 0 6 in
      if (String.compare pref "sinter" )=0 then
	begin 
	  let suffix = String.sub name 6 (namelen-6) in
	  let id = int_of_string suffix in
	  FLATAC_SINTER_SID( id)
	end
      else type_statename_5 name
    end



let type_statename sname =
  type_statename_6 sname



let get_esid_of_statename name =
  let esid_kind = type_statename name in
  match esid_kind with
    FLATAC_SINIT ->Some(ESID(0))
  | FC_ESID(n) -> Some(ESID(n))
  | FLATAC_SINTER_SID(_) -> None


let get_sid_statement_of_esid esid subsyst =
  let sid = 
    (
      try
	Hashtbl.find subsyst.esid_to_sid_map 
	  esid
      with
	Not_found -> 
	  ( raise (Unreferenced_esid_in(esid,subsyst.esid_to_sid_map)))
    )
  in
  let annot=
    ( try Hashtbl.find subsyst.esid_to_statement_infos sid 
    with
    | Not_found -> 
      begin
	Format.printf "Can't find sid %s %! \n" (Trace.pprint_sid sid);
	("/*Dummy ecfg starting point; No Operation*/")
      end
    )
  in
  (sid,annot)


let sid_anot_info_of_opt_esid opt_esid tr_smap =
  match opt_esid with
    None -> (SID(-1),"/*flatac_intermediate_state;*/")
  | Some(e) -> get_sid_statement_of_esid e tr_smap


let sid_infos_of_syscontrol tr_map sysc =
  match sysc with
    Trace_types.Sys_control(sysname,statename) -> 
      begin
	let tr_smap = 
	  (
	    try
	      Hashtbl.find tr_map sysname 
	    with
	      Not_found -> ( raise (NoSubsystem(sysname,tr_map)))
	  )
	   in
	  let opt_esid = get_esid_of_statename statename in
	  sid_anot_info_of_opt_esid opt_esid tr_smap
      end
	

(*
let pprint_c_statement_list_of_trace sys_table tr =
*)
  

  (* Need to compute the stack position *)
