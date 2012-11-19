open Lexing
open Trace_types
open Trace
open Trace_eldarica_lexer
open Trace_eldarica_parser


let print_str_l_folder prefix s =
  if String.length prefix = 0 then
     s
  else
    prefix^","^s


let _ = if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf " You need to provide a trace filename as an argument \n Aborting \n %!"
    end
  else ();
   
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
  
      
  let buf = Lexing.from_channel input_channel 
  in
  (*
  let trace_l = Trace_eldarica_parser.gettrace Trace_eldarica_lexer.token  buf *)
   let trace_l = Trace_eldarica_parser.gettrace Trace_eldarica_lexer.token  buf
   in
  Format.printf "Trace is %s \n" (List.fold_left  print_str_l_folder "" trace_l)
      
      
