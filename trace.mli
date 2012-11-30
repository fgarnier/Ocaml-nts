val print_sys_control : Trace_types.sys_control -> string
val print_trace_l_folder : string -> Trace_types.sys_control -> string
val print_trace : Trace_types.sys_control list -> string
val pprint_esid : Trace_types.esid -> string
val pprint_sid : Trace_types.sid -> string
val pprint_folder_esid_sid_map :
  Trace_types.esid -> Trace_types.sid -> string -> string
val pprint_esidsid_map :
  (Trace_types.esid, Trace_types.sid) Hashtbl.t -> string
val pprint_position : Lexing.position * Lexing.position -> string
val pprint_folder_sid_code_map :
  Trace_types.sid ->
  string * (Lexing.position * Lexing.position) option -> string -> string
val pprint_sid_to_code_info :
  (Trace_types.sid, string * (Lexing.position * Lexing.position) option)
  Hashtbl.t -> string
val pprint_map_2_fcinfo : Trace_types.map_2_fcinfos -> string
val pprint_tr_subsystem_table :
  ('a, Trace_types.map_2_fcinfos) Hashtbl.t -> string
