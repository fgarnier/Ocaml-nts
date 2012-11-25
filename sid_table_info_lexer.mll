{
  open Sid_table_info_parser
  open Lexing

  
  exception UNdefinedLexem of string

      
  module KWD: sig val register_kwd : string -> token -> unit val _KWD_or_IDENT : string -> token end =
  struct
    let kwds = Hashtbl.create 17
    
    let register_kwd k sort = Hashtbl.add kwds k sort
      
    let _KWD_or_IDENT str = try Hashtbl.find kwds str with Not_found -> IDENT(str)
  end;;


  open KWD;;


  register_kwd "FUNCTION" FUNDECL;;
  register_kwd "ESID_TO_SID_MAP" MAPDECL;;
  register_kwd "SID_TO_CODE_MAP" CODEMAP;;



  let new_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum }

}

let lletter = ['a' - 'z']
let uletter = ['A' - 'Z']
let number =  ['0' - '9']
let intval = number +
let identifier = ( '_' | uletter | lletter)+ ( uletter | lletter | '_' | number )*
let dotted_identifier = (  '_' | uletter | lletter)+ ( '/' |'.' | '_' | uletter | lletter | number )+ (uletter | lletter |number)+

    
  rule token = parse
  | ">>" {MAPESIDTOSID}
  | "{{" {OPENGROUP}
  | "}}" {CLOSEGROUP}
  | "@{{@" {CODEBLOCKOPEN}
  | "@}}@" {CODEBLOCKCLOSE}

  | ";;" {ENDLINE}

  | eof {EOF}



  | ['\n'] {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t' '\r' '\000'] {token lexbuf}

  | intval { 
    let num =  Big_int.big_int_of_string( 
    Lexing.lexeme lexbuf) in
    INT(num)
  }

  | _ { 
    let error_msg = Lexing.lexeme lexbuf in 
    raise (UndefinedLexem(error_msg))
  }
      
      
{}
