(*type sys_control = Sys_control of string * string (* Name of subsystem
							name of the control
						   state*)

type trace = sys_control list
*)

open Trace_types



let print_sys_control s =
  match s with 
    Sys_control(sname,cname) -> sname^":"^cname

let print_trace_l_folder prefix s =
  if String.length prefix = 0 then
    print_sys_control s
  else
    prefix^","^(print_sys_control s)

let print_trace t =
  List.fold_left print_trace_l_folder "" t
    
