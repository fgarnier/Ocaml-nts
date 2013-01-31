
open Lexing
open Ntl_lexer
open Nts_parser
open Nts_generic
open Nts_functor
open Dot_driver


module Dotofintnts = Dot_Driver.Make(Ntsint.P)


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

let create_output_file dump_name =
  let dump_file_descr = ( 
    try
      ( open_out dump_name )
    with
      Sys_error(a) -> 
	begin
	  Format.printf "Can't create file  %s, aborting \n %!" dump_name;
	  raise ( Sys_error(a) )
	end
  )
  in
  dump_file_descr
      




(** Main programm *)

let _ =
  let num_args =  Array.length  Sys.argv in
  if ( num_args != 2 &&  num_args!=4 ) 
  then
    begin
      Format.printf "Syntax : %s File.nts \n  
 Aborting \n %!" Sys.argv.(0)
    end
  else
    
    begin
      let filename = Sys.argv.(1) in
      let input_channel = openfile_with_guard filename in
      let base_file_name = Filename.basename filename in
      let dir_name = Filename.dirname filename in
      let dump_name = dir_name^"/"^base_file_name^".dot" in
      let dump_channel = create_output_file dump_name in
      let buf = Lexing.from_channel input_channel in
      let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
      let nt_system = Nts_int.nt_system_var_cleaner nt_system in
      let output_string = Dotofintnts.dot_of_all_subsystem_of_nts 
	nt_system in
      Format.fprintf dump_chanel "%s" output_string;
      close_out dump_channel;
      close_in input_channel;
      
    end


