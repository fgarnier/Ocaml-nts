{
  open Trace_eldarica_parser
  open Lexing

  exception UndefinedLexem of string

  module KWD: sig val register_kwd : string -> token -> unit val _KWD_or_IDENT : string -> token end =
  struct
  let kwds = Hashtbl.create 17
    
  let register_kwd k sort = Hashtbl.add kwds k sort
    
  let _KWD_or_IDENT str = try Hashtbl.find kwds str with Not_found -> IDENT(str)
  end;;
  
  open KWD;;      


 register_kwd "List" TRACEDECL;;


}


let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let number = ['0'-'9']
let intval = number +

let sys_state_name = ('_'| uletter | lletter )+ (uletter|lletter|'_'|number)*

rule token = parse
| "," {COMMA}
| "(" {LBRACE}
| ")" {RBRACE}
| ":" {COLON}

 
| sys_state_name {STATE( Lexing.lexeme lexbuf)}
| eof {EOF}


