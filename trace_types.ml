type sys_control = Sys_control of string * string (* Name of subsystem
							name of the control
						   state*)

type trace = sys_control list

type esid = ESID of int
type sid = SID of int
type esidtosidrel =  esid * sid
type map_2_fcinfos = 
	{
	 esid_to_sid_map : (esid,sid) Hashtbl.t;	
	esid_to_statement_infos : (sid, string) Hashtbl.t;		t
	}	

	


