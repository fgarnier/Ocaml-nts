open Lexing
open Trace_types
open Sid_table_info_parser 
open Sid_table_info_lexer
open Trace


let _ = 
  if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf "  A file name is expected as argument \n Aborting \n %!"
    end
  else
    begin
      let filename = Sys.argv.(1) in
      let input_channel =
	( 
	  try
	    ( open_in filename )
	  with
	      Sys_error(a) -> 
		begin
		  Format.printf "Can't open file  %s, aborting \n %!" filename;
		  raise ( Sys_error(a) )
		end
	)
      in
      
      try
	let buf = Lexing.from_channel input_channel in
	let sid_infos =  Sid_table_info_parser.mapextract buf in
	let print_out = Trace.pprint ( pprint_tr_subsystem_table sid_infos)
	in
	Format.printf "%s" print_out 


      with
	| _ -> 
	  begin
	    prerr_string "Parse error \n";
	    exit 1
	  end
    end
   


