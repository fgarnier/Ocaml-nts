(** Parses the table exported by the flatac plugin to rebuild the
correspondance between the ecfg sid and frama-c sid as well as the
c language statement.*)



open Sid_table_info_parser
open Sid_table_info_lexer
open Trace_eldarica_parser
open Trace_eldarica_lexer
open Trace_analysis
open Nts_types
open Ntl_parser
open Ntsint
open Nts_generic



let openfile_with_guard fname =
  let input_channel =
    ( 
      try
	( open_in fname )
      with
	Sys_error(a) -> 
	  begin
	    Format.printf "Can't open file  %s, aborting \n %!" fname;
	    raise ( Sys_error(a) )
	  end
    ) 
  in
  input_channel






let get_trace_from_file fname =
  let input_channel = 
    openfile_with_guard fname
  in
  let buf = Lexing.from_channel input_channel
  in
  let tr =Trace_eldarica_parser.gettrace Trace_eldarica_lexer.token  buf in
    close_in input_channel; 
  tr

let nts_lib_standards_subsystems =
  let input_channel = 
    openfile_with_guard "base_fun.ca_lib"
  in
  let buf = Lexing.from_channel input_channel in
  let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
  let nt_system = Nts_int.nt_system_var_cleaner nt_system in
  close_in input_channel;
  nt_system

let fold_info_of_trace nts_std_lib tr_smap prefix sysc =
  let (sid,annot) = Trace_analysis.sid_infos_of_syscontrol ~annot_less_callee:(Some(nts_std_lib)) tr_smap sysc in
  match annot with
    (str_annot,None) ->
    Format.sprintf "%s %s:%s\n" prefix (Trace.pprint_sid sid) str_annot
  | (str_annot,Some(p)) 
    -> 
    begin
     Format.sprintf "%s %s:%s %s\n" prefix (Trace.pprint_sid sid) str_annot 
       (Trace.pprint_position p)
    end

let get_info_table_from_file fname =
  let input_channel =
    openfile_with_guard fname 
  in
  let buf = Lexing.from_channel input_channel in
  let itable = Sid_table_info_parser.mapextract Sid_table_info_lexer.token buf
  in
  close_in input_channel;
  itable
    
(** Main function of this utility program*)
let _ =
  if (Array.length Sys.argv ) != 3 then 
    begin
      Format.printf "trace_upon_statement_info sid_table_info_file trace_file \n";
      exit (1) ;
    end
  else ();

  let trace = get_trace_from_file Sys.argv.(2) in
  let trmap = get_info_table_from_file Sys.argv.(1) in
  let pprint_folder = fold_info_of_trace (nts_lib_standards_subsystems.Ntsint.Nts_int.nts_automata) trmap in
  let print_out = List.fold_left pprint_folder "" trace
  in

  Format.printf " trace is : \n %s%!" print_out;
  exit(0)
    

      
