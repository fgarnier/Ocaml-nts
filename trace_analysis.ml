open Trace_types
open Trace


type control_type = FC_ESID of int
  | FLATAC_SINTER_SID of int
  | FLATAC_SINIT

exception UnknownESidDescriptor of string
exception Unreferenced_esid_in of esid * (esid,sid) Hashtbl.t
exception NoSubsystem of string * tr_subsystem_table

(**  *)

let type_statename sname =
  let namelen = String.length sname in
  
  let type_statename_1 name =
    let pref = String.sub name 0 1 in
    match pref with
      "s" ->
	begin 
	  let suffix = String.sub name 1 (namelen-1) in
	  let id = int_of_string suffix in
	  FC_ESID(id)
	end
    | _ -> raise (UnknownESidDescriptor sname)
  in
  let type_statename_5 name =
    if namelen < 5 then type_statename_1 name 
    else
      begin
	let pref = String.sub name 0 4 in
	match pref with
	  "sinit" ->
	      FLATAC_SINIT
	   
	| _ -> type_statename_1 name
      end
  in
  
  let type_statename_6 name =
    if namelen < 6 then type_statename_5 name 
    else 
      begin
	let pref = String.sub name 0 5 in
	match pref with
	  "sinter" ->
	    begin 
	      let suffix = String.sub name 6 (namelen-1) in
	      let id = int_of_string suffix in
	      FLATAC_SINTER_SID( id)
	    end
	| _ -> type_statename_5 name
      end
  in
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
  let annot=Hashtbl.find subsyst.esid_to_statement_infos sid in
  (sid,annot)


let sid_anot_info_of_opt_esid opt_esid tr_smap =
  match opt_esid with
    None -> (SID(-1),"/*flatac_intermediate_state;*/")
  | Some(e) -> get_sid_statement_of_esid e tr_smap


let sid_infos_of_syscontrol tr_map sysc =
  match sysc with
    (sysname,statename) -> 
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
