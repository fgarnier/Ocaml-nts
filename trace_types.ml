type sys_control = Sys_control of string * string (* Name of subsystem
							name of the control
						   state*)

type trace = sys_control list

type esid = ESID of int
type sid = SID of int
type esidtosidrel =  esid * sid

type map_2_fcinfos = 
	{
	tr_sysname : string ;
	 esid_to_sid_map : (esid,sid) Hashtbl.t;	
	esid_to_statement_infos : (sid, string) Hashtbl.t;		t
	}	

type subsystem_map = 
	{ tr_subsystem_	name : string ;
	 tr_map : map_2_fcinfos ;	 	
	}

	


