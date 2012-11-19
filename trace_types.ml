type sys_control = Sys_control of string * string (* Name of subsystem
							name of the control
						   state*)

type trace = sys_control list
 
