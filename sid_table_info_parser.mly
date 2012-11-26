%{
  open Lexing
  open Trace_types 
   
  
  let esid_sid_tbl_builder tbl ((l,r) : (esid * sid)) =
    Hashtbl.add tbl l r
      
  let sid_to_code_tbl_builder tbl ((l,r) : (sid * string)) =
    Hashtbl.add tbl l r
      
      
  let tr_subsystem_builder_iterator tbl 
      ((l,r) : (string *  map_2_fcinfogs)) =
    Hashtbl.add tbl l r
      
%}


%type <Trace_types.tr_subsystem_table> mapextract 
%token <int> INT
%token <string> IDENT
%token <string> ANNOT
%token MAPESIDTOSID OPENGROUP CLOSEGROUP CODEBLOCKOPEN CODEBLOCKCLOSE
%token ENDLINE EQ EOF CCODE SID COLON SEMICOLON FUNDECL CODEMAP DECLMAPESIDTOSID
%token DECLARECODEMAP
%start mapextract
%%


mapextract : nts_map_list EOF {
  let tbl = Hashtbl.create 97 in
  List.iter ( tr_subsystem_builder_iterator tbl ) $1;
  tbl
    
};


nts_map_list : extract_subsystable_map {[$1]}
 | extract_subsystable_map nts_map_list {$1 :: $2 };


extract_subsystable_map : OPENGROUP FUNDECL EQ IDENT ENDLINE extract_esid_sid_map ENDLINE extract_sid_code_map ENDLINE CLOSEGROUP {

  let mp = {
    tr_sysname=$4;
    esid_to_sid_map = $6;
    esid_to_statement_infos=$8;
  } in
  {tr_subsystem_name = $4;
   tr_map = mp;
  }

};


extract_esid_sid_map : DECLMAPESIDTOSID OPENGROUP esidlist CLOSEGROUP
{
  let esidsidtbl = Hashtbl.create 97 in
  List.iter (fun s -> esid_sid_tbl_builder esidsidtbl s) $3;
  esidsidtbl
};


esidlist : esidtosidrel ENDLINE {[$1]}
| esidtosidrel ENDLINE esidlist {$1::$3};


esidtosidrel : INT MAPESIDTOSID INT {(Esid($1), Sid($3))};


extract_sid_code_map : CODEMAP OPENGROUP sidtocodelist CLOSEGROUP 
  {
    let tbl = Hashtbl.create 97 in
    Lisp.map (fun s -> sid_to_code_tbl_builder tbl s) $3;
    tbl
  };



sidtocodelist : sidtocoderel { [$1] }
| sidtocoderel sidtocodelist {$1::$2};

sidtocoderel :  SID COLON INT SEMICOLON CCODE ANNOT {
  let sid = $3 in
  let code = $6 in
  (Sid(sid),code)						 
};

