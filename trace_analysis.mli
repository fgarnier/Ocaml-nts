type control_type = FC_ESID of int | FLATAC_SINTER_SID of int | FLATAC_SINIT
exception UnknownESidDescriptor of string
exception Unreferenced_esid_in of Trace_types.esid *
            (Trace_types.esid, Trace_types.sid) Hashtbl.t
exception NoSubsystem of string * Trace_types.tr_subsystem_table
val type_statename : string -> control_type
val get_esid_of_statename : string -> Trace_types.esid option
val get_sid_statement_of_esid :
  Trace_types.esid -> Trace_types.map_2_fcinfos -> Trace_types.sid * string
val sid_anot_info_of_opt_esid :
  Trace_types.esid option ->
  Trace_types.map_2_fcinfos -> Trace_types.sid * string
val sid_infos_of_syscontrol :
  Trace_types.tr_subsystem_table ->
  string * string -> Trace_types.sid * string
