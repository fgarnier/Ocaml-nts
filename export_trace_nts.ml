(** Parses the table exported by the flatac plugin to rebuild the
correspondance between the ecfg sid and frama-c sid as well as the
c language statement.*)


open Trace_eldarica_parser
open Trace_eldarica_lexer
open Trace_analysis
open Nts_types
open Ntl_parser
open Ntsint
open Nts_generic
open Expand_eldarica_compressed_bloc


module INtstrace = Expand_eldarica_compressed_bloc.Make(Ntsint.P)

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


let get_nts_from_file fname =
  let  ichannel = openfile_with_guard fname in
  let buf = Lexing.from_channel ichannel in
  let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
  let nt_system = Nts_int.nt_system_var_cleaner nt_system in
  close_in ichannel;
  nt_system
    
  
(** Main function of this utility program*)
let _ =
  if (Array.length Sys.argv ) != 3 then 
    begin
      Format.printf "trace_2_nts orig_nts trace_file \n";
      exit (1) ;
    end
  else ();

  let trace = get_trace_from_file Sys.argv.(2) in
  let nt = get_nts_from_file Sys.argv.(1) in
  let nts_out = 
    INtstrace.nts_out_trace nts_lib_standards_subsystems nt trace 
  in
  let printout = Nts_int.pprint_nts nts_out in 
    Format.printf "%s\n" printout;
  exit(0)
    

      
