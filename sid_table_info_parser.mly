%{
  open Lexing
  open Trace_types 
   

  let esid_sid_tbl_builder tbl ((l,r) : (esid * sid)) =
    Hashtbl.add tbl l r

  let sid_to_code_tbl_builder tbl ((l,r) : (sid * string)) =
    Hastbl.add tbl l r
%}



%token <int> INT
%token <string> IDENT
%token <string> ANNOT
%token MAPESIDTOSID OPENGROUP CLOSEGROUP CODEBLOCKOPEN CODEBLOCKCLOSE
%token ENDLINE EQ EOF CCODE SID COLON SEMICOLON FUNDECL

%type <Table_types.sid_to_code_tbl_builder>
%entry mapextract

%%



nts_map_list : extract_subsystable_map {[$1]}
 | extract_subsystable_map nts_map_list {$1 :: $2 };


extract_subsystable_map : OPENGROUP FUNDECL EQ IDENT extract_esid_sid_map extract_sid_code_map CLOSEGROUP {

  {
    tr_sysname=$4;
    esid_to_sid_map = $5;
    esid_to_statement_infos=$6;
  }

};


extract_esid_sid_map : MAPESIDTOSID OPENGROUP esidlist CLOSEGROUP
{
  let esidsidtbl = Hashtbl.create 97 in
  List.iter (fun s -> esid_sid_tbl_builder esidsidtbl s) $3;
  esidsidtbl
};


esidlist : esidtosidrel ENDLINE {[$1]}
| esdidlist ENDLINE esidlist {$1::$3};


esidtosidrel : INT MAPSIDTOSID INT {(Esid($1), Sid($3))};


extract_sid_code_map : CODEMAP OPENGROUP sidtocoldelist CLOSEGROUP 
  {
    let tbl = Hashtbl.create 97 in
    Lisp.map (fun s -> sid_to_code_tbl_builder tbl s) $3;
    tbl
  };



sidtocodelist : sidtocoderel { [$1] }
| sidtocoderel sidtocodelist {$1::$2};

sidtocoderel : SID COLON INT SEMICOLON CCODE CODEBLOCKOPEN CODEBLOCKCLOSE 
  {
    (Sid(sid),"")
  };

| SID COLON INT SEMICOLON CCODE CODEBLOCKOPEN something CODEBLOCKCLOSE {
  let sid = $3 in
  let code = $7 in
  (Sid(sid),code)						 
};

something : IDENT something {$1^($2)}
| IDENT {$1};


