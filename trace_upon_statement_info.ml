(** Parses the table exported by the flatac plugin to rebuild the
correspondance between the ecfg sid and frama-c sid as well as the
c language statement.*)



open Sid_table_info_parser
open Sid_table_info_lexer
open Trace_eldarica_parser
open Trace_eldarica_lexer


let openfile_with_guard fname =
  let input_channel =
    ( 
      try
	( open_in filename )
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
    open_file_with_guard fname
  in
  let buf = Lexing.from_channel input_channel
  in
  let tr =Trace_eldarica_parser.gettrace Trace_eldarica_lexer.token  buf in
    close_in input_channel; 
  tr


let get_info_table_from_file fname =
  let input_channel =
    open_file_with_guard fname 
  in
  let buf = Lexing.from_channel input_channel in
  let itable = Sid_table_info_parser.mapextract Sid_table_info_lexer.token buf
  in
  close_in input_channel;
  itable
    
      
