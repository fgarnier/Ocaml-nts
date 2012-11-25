%{
  open Lexing
  open Trace_types 
   

  let esid_sid_tbl_builder tbl ((l,r) : (esid * sid)) =
    Hashtbl.add tbl l r

  let sid_to_code_tbl_builder tbl ((l,r) : (sid * string)) =
    Hastbl.add tbl l r
}



%token <int> INT
%token MAPESIDTOSID OPENGROUP CLOSEGROUP CODEBLOCKOPEN CODEBLOCKCLOSE
%token ENDLINE EOF CCODE SID COLON SEMICOLON


%%


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


