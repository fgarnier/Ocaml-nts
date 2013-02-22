(*
Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA

*)


open Nts_types
open Nts

exception Found_a_primed_var
exception Type_mismatch_in_arithm_expression of 
    Nts_types.nts_genrel_arithm_exp * Nts_types.nts_genrel_arithm_exp
exception Arithmetic_modulo_on_non_integer_type


let nts_pprint_genvar var =
  match var with
      NtsGenVar(v,NtsPrimed) -> Format.sprintf "%s'" (nts_pprint_nts_var v)
    | NtsGenVar(v,NtsUnPrimed) -> (nts_pprint_nts_var v)
  

let pprint_ntsgen_var_list l =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold (nts_pprint_genvar h) l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^(nts_pprint_genvar h)) l' 
  in
  (pprint_nts_var_list_fold "" l)

let pprint_nts_quantifier  q =
  match q with 
      NtsExists -> "exists"
    | NtsForall -> "forall"

let rec nts_pprint_nts_typeinfo_genvar ( x : nts_genrel_var) =
  match x with 
      NtsGenVar(NtsVar( vname,vtype ),_) -> vname^(Nts.nts_pprint_btype vtype )
    
      

let is_int_var v =
  match v with 
      NtsGenVar(NtsVar( _, NtsIntType ),_)-> true
    |_ -> false
      
let is_real_var v =
  match v with
    NtsGenVar(NtsVar( _,NtsRealType),_)-> true
    |_ -> false
      


 
      
let concat_if_first_arg_nonzero s1 s2 =
  if String.length s1 != 0
  then s1^s2
  else ""
	
let concat_comma_both_arg_non_empty s1 s2 =
  if String.length s1 != 0 then
    begin
      if  String.length s2 != 0 then
	s1^","^s2
      else
	s1
    end
  else
    s2
      

let pprint_typeinfo_nts_genvar_list l  =
  let get_curr_type_of_var v =
    match v with
        NtsGenVar(NtsVar(_,t),_) -> t
	  
  in
  let string_of_type ctype =
    match ctype with
        Some(NtsIntType) -> ": int"
      | Some(NtsRealType) -> ": real"
      | Some(NtsBoolType) -> ": bool"
      | Some(NtsUnTyped) -> ": untyped"
      | None -> ""
  in
  let outputstring_folder ( prefix , ctype ) nvar  =
    match ctype, nvar with
        (None, _) ->
          begin
            let vname = Nts.nts_get_nts_gen_var_name nvar in
            ((Format.sprintf "%s"  vname),
             Some(get_curr_type_of_var nvar) )
          end
      | (Some(NtsIntType), NtsGenVar(NtsVar(vname,NtsIntType),_)) ->
        ((Format.sprintf "%s,%s" prefix vname),ctype )

      | (Some(NtsRealType), NtsGenVar(NtsVar(vname,NtsRealType),_)) ->
        ((Format.sprintf "%s,%s" prefix vname), ctype)
      | (Some(NtsBoolType), NtsGenVar(NtsVar(vname,NtsBoolType),_)) ->
        ((Format.sprintf "%s,%s" prefix vname), ctype)
      (*Need to add handling for various array types*)
      | (Some(NtsUnTyped), NtsGenVar(NtsVar(vname,NtsUnTyped),_)) ->
        ((Format.sprintf "%s,%s" prefix vname), ctype)	  
      | (Some(NtsIntType),_) ->
        ((Format.sprintf "%s : int, %s" prefix
            (Nts.nts_get_nts_gen_var_name nvar)), Some(get_curr_type_of_var nvar) )
      | (Some(NtsRealType),_) ->
        ((Format.sprintf "%s :real, %s" prefix
            (Nts.nts_get_nts_gen_var_name nvar)), Some (get_curr_type_of_var nvar) )
      | (Some(NtsBoolType),_) ->
        ((Format.sprintf "%s :bool, %s" prefix
            (Nts.nts_get_nts_gen_var_name nvar)), Some (get_curr_type_of_var nvar) )

      | (Some(NtsUnTyped),_) ->
        ((Format.sprintf "%s :bool, %s" prefix
            (Nts.nts_get_nts_gen_var_name nvar)), Some (get_curr_type_of_var nvar) ) 
  in
  let (ret_s,ctype_s) = List.fold_left outputstring_folder ("", None) l in
  Format.sprintf "%s%s" ret_s (string_of_type ctype_s)


  



let rec size_genrel_arithm_deeper_than 
    (barithm : nts_genrel_arithm_exp ) (depth : int ) =

  if depth <= 0 then true
  else 
    let depth' = depth - 1 in
    match barithm with 
	CntGenCst(_,_)
      | CntGenSymCst (_ )
      | CntGenVar (_) -> false
      | CntGenArithmUOp( _, exp',_ ) ->  
	size_genrel_arithm_deeper_than exp' depth'
	  
      | CntGenArithmBOp (_ , eg ,  ed , _ ) ->
	(size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' )
      
  

(* This function is used to parse subtrees in a more human
readable fashion. *)

let rec size_genrel_deeper_than  (bexp : nts_gen_relation ) (depth : int ) =
  if depth <= 0 then
    true
  else
    let deep' = depth - 1 in
    match bexp with
	CntGenTrue -> false
      | CntGenFalse -> false
      | CntGenNot ( exp' ) -> size_genrel_deeper_than exp' deep'
      | CntGenRelComp(_ , eg ,  ed ) ->
	(size_genrel_deeper_than eg deep' ) || (size_genrel_deeper_than ed deep' )
      | CntGenRel ( _ , _ , _ ) -> false   
      |  CntQVarsGenRel(_,_,exp') -> size_genrel_deeper_than exp' deep'


let rec nts_pprint_genrel_arithm_exp ( exp : nts_genrel_arithm_exp ) =
  match exp with
      CntGenCst(CntGenICst(i),_) -> Big_int.string_of_big_int i
    | CntGenCst(CntGenFCst(f),_) -> Format.sprintf "%f" f
    | CntGenCst(CntGenBCst(CntBTrue),_) -> "true"
    | CntGenCst(CntGenBCst(CntBFalse),_) -> "false" 
    | CntGenSymCst(CntSymCst(vname,_),_ ) -> vname
    | CntGenVar ( ntsgenvar ) -> nts_pprint_genvar ntsgenvar
   
    
    
    | CntGenArithmUOp(CntGenUMinus, e ,_ ) ->
      begin
	match e with
	    CntGenArithmUOp(CntGenUMinus, subtree,_ ) -> nts_pprint_genrel_arithm_exp subtree
	  | _  -> "-"^(nts_pprint_genrel_arithm_exp e)
      end

    | CntGenArithmBOp(CntGenSum , eg , ed ,_) ->
      (nts_pprint_genrel_arithm_exp eg ) ^"+" ^(nts_pprint_genrel_arithm_exp ed)

    | CntGenArithmBOp( CntGenMinus , eg , ed, _ )
      -> 
      begin
	if size_genrel_arithm_deeper_than ed 2 then
	  let pprint_output = 
	    (nts_pprint_genrel_arithm_exp eg)^"-("^(nts_pprint_genrel_arithm_exp ed)^")"
	  in
	  pprint_output
	else
	  (nts_pprint_genrel_arithm_exp eg)^"-"^(nts_pprint_genrel_arithm_exp ed)
      end
	
    | CntGenArithmBOp(  CntGenDiv,  eg , ed, _ )
      -> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"-"^(!pprint_outputd)
      end
       
   | CntGenArithmBOp(  CntGenProd , eg , ed, _ )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 1 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd :=nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 1 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"*"^(!pprint_outputd)
     end
       
    
   | CntGenArithmBOp( CntGenMod ,  eg , ed, _ )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"%"^(!pprint_outputd)
     end 

  (* | CntGenInvalidExp -> raise Invalid_nts_expression *)
     




let pprint_gen_rel_arithm_list larithm =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold (nts_pprint_genrel_arithm_exp h) l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^(nts_pprint_genrel_arithm_exp h)) l' 
  in
  (pprint_nts_var_list_fold "" larithm)
  
  


let nts_pprint_arithm_unop uop =
  match uop with
    CntGenUMinus -> "-"

(* Print the conjunction or the disjunction of lhs/rhs *)
let nts_pprint_bool_binop ( lhs : string) bop (rhs : string) =
  match bop with
      CntGenBAnd -> lhs ^ " and " ^ rhs
    | CntGenBOr -> lhs ^ " or " ^ rhs


let nts_pprint_aritm_binop  bop  =
  match bop with
      CntGenSum -> "+"
    | CntGenMinus -> "-"
    | CntGenProd -> "*"
    | CntGenDiv -> "/"
    | CntGenMod -> "%"

(* reduce code size using cnt_gen_bool_binop type*)
let rec nts_pprint_genrel (bexp : nts_gen_relation ) =
  match bexp with 
      CntGenTrue -> "true"
    | CntGenFalse-> "false"
    | CntGenNot ( exp ) ->
      if size_genrel_deeper_than exp 0 then
	"not ("^(nts_pprint_genrel exp)^")"
      else 
	"not "^(nts_pprint_genrel exp)
	  
    |  CntGenRelComp(CntGenBAnd, eg , ed )
      -> 
      begin
	let pprint_outputd = ref ""
	    in
	let pprint_outputg = ref "" 
	in
	begin
	  if size_genrel_deeper_than ed 1 then
	    begin
	      pprint_outputd := "("^(nts_pprint_genrel ed)^")";
	    end
	  else
	    pprint_outputd := nts_pprint_genrel ed;
	end;
	begin
	  if size_genrel_deeper_than eg 1 then
	    begin
	      pprint_outputg := "("^(nts_pprint_genrel eg)^")";
	    end
	  else
	    pprint_outputg := nts_pprint_genrel eg;
	end;
	(!pprint_outputg)^" and "^(!pprint_outputd)
      end

    |  CntGenRelComp ( CntGenBOr , eg , ed )
      -> 
      begin
	let pprint_outputd = nts_pprint_genrel ed 
	in
	let pprint_outputg = nts_pprint_genrel eg
	in
	(pprint_outputg)^" or "^(pprint_outputd)
      end
	
    | CntGenRel ( bop , expg , expd ) ->
      begin
	let expg = nts_pprint_genrel_arithm_exp expg 
	in
	let expd =  nts_pprint_genrel_arithm_exp expd 
	in
	match bop with
	    CntEq ->  expg^" = "^expd
	  | CntNeq ->  expg^" != "^expd
	  | CntLeq -> expg^" <=  "^expd
	  | CntLt -> expg^" < "^expd
	  | CntGt -> expg^" > "^expd
	  | CntGeq -> expg^" >= "^expd
      end
    | CntQVarsGenRel (vlist,quantifier, subformula) ->
      begin
	let qvars_list_print =  pprint_typeinfo_nts_genvar_list vlist in
	let q_print = pprint_nts_quantifier quantifier in
	let pprint_sformula = nts_pprint_genrel subformula in
	Format.sprintf "%s %s . %s" q_print qvars_list_print pprint_sformula
      end
	


(* This function answers yes whenever r is a boolean relation,
i.e. when all variables are unprimed*)

let boolean_relation r =
  let unprimed_var_checker x =
    match x with
        NtsGenVar(_,NtsPrimed) -> raise Found_a_primed_var
      | NtsGenVar(_,NtsUnPrimed) -> ()
  in
  let rec primeless_arithm_express p =
    match p with
	CntGenVar(n) ->  unprimed_var_checker n
      (*| CntGenNdet | CntGenNdetVar(_)*) 
      | CntGenSymCst(_) -> ()
      | CntGenCst(_,_) -> ()
      | CntGenArithmBOp(_,a,b,_) -> 
	( primeless_arithm_express a);
	( primeless_arithm_express b) 
      | CntGenArithmUOp(_,a,_) ->  primeless_arithm_express a 
      
  in
  try
    primeless_arithm_express r; true
  with
      Found_a_primed_var -> false
    
    
(* Syntactic simplification of boolean expressions *)
  let simplify_genrel_bottom_top (e : nts_gen_relation ) = 
    match e with
      | CntGenRelComp(CntGenBAnd,CntGenFalse,_) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,_,CntGenFalse) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,CntGenTrue,a) ->  a
      | CntGenRelComp(CntGenBAnd,a,CntGenTrue) ->  a
      | CntGenNot(CntGenTrue) -> CntGenFalse
      | CntGenNot(CntGenFalse) -> CntGenTrue
      | CntGenNot(CntGenNot(a)) -> a
      | CntGenRelComp(CntGenBOr,_,CntGenTrue) -> CntGenTrue
      | CntGenRelComp(CntGenBOr,CntGenTrue,_) -> CntGenTrue
      | CntGenNot(CntGenRel(CntEq,a,b)) -> (CntGenRel(CntNeq,a,b))
      | CntGenNot(CntGenRel(CntNeq,a,b)) -> (CntGenRel(CntEq,a,b))
      | CntGenNot(CntGenRel(CntLt,a,b)) -> (CntGenRel(CntGeq,a,b))
      | CntGenNot(CntGenRel(CntGt,a,b)) -> (CntGenRel(CntLeq,a,b))
      | CntGenNot(CntGenRel(CntLeq,a,b)) -> (CntGenRel(CntGt,a,b))
      | CntGenNot(CntGenRel(CntGeq,a,b)) -> (CntGenRel(CntLt,a,b))	
      | _ -> e

	
  let rec simplify_gen_rel ( e : nts_gen_relation ) =
    match e with
      | CntGenRelComp(CntGenBAnd,CntGenFalse,_) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,_,CntGenFalse) -> CntGenFalse
      
      | CntGenRelComp(CntGenBOr,CntGenTrue,_) -> CntGenTrue
      | CntGenRelComp(CntGenBOr,_,CntGenTrue) -> CntGenTrue	
      
      
      | CntGenNot(CntGenNot(a)) -> 
	simplify_gen_rel a

      | CntGenRelComp(CntGenBAnd,a,b) -> 
	let fg = simplify_gen_rel a in
	let fd = simplify_gen_rel b in
	simplify_genrel_bottom_top (CntGenRelComp(CntGenBAnd,fg,fd))
	
	  
      | CntGenRelComp(CntGenBOr,a,b) -> 
	let fg = simplify_gen_rel a in
	let fd = simplify_gen_rel b in
	simplify_genrel_bottom_top (CntGenRelComp(CntGenBOr,fg,fd))		  
      | CntGenNot(a) -> 
	let a = simplify_gen_rel a in
	simplify_genrel_bottom_top (CntGenNot(a))
			 
      | CntGenTrue -> CntGenTrue
      | CntGenFalse -> CntGenFalse
	
      | _ -> e



(**********************)
(* Those mutually recursive functions answers yes whenever the bool, res. the 
arithmetic val, only depends on the variable evaluation --and not of 
non-determinitic variables. In this implementation, the Non deterministic
variables and values are removed, hence, the answere is always true. One
need to update that function or delete it*)

let rec is_gen_bool_det ( b : nts_gen_relation ) =
  match b with
    | CntGenTrue -> true
    | CntGenFalse -> true
    
    | CntGenRelComp (_,a,b) ->  
      let ndet_fg = is_gen_bool_det a in
      let ndet_fd = is_gen_bool_det b in
      ndet_fg && ndet_fd
	
 
    | CntGenNot (a) -> is_gen_bool_det a
      
    | CntGenRel(_,a,b) -> 
      let det_fg = is_gen_arithm_exp_a_function a in
      let det_fd = is_gen_arithm_exp_a_function b in
      det_fg && det_fd

(* Does the arithmetic expression have a deterministic evalution, i.e.
contains no CntNdet constructor *)	
and is_gen_arithm_exp_a_function (e : nts_genrel_arithm_exp ) =
  match e with
   (*| CntGenNdet -> false
   | CntGenNdetVar(_) -> false *)
   | CntGenArithmBOp(_,fg,fd,_) ->   
     let det_fg = is_gen_arithm_exp_a_function fg in
     let det_fd = is_gen_arithm_exp_a_function fd in
     det_fg && det_fd
   | CntGenArithmUOp (_,a,_) -> is_gen_arithm_exp_a_function a
   
   | _-> true

(**********************)


 (* Answers true if the expression can be sytacticaly evalutated to false.
 An answers to false means that e shall be evaluated at runtime, and might
be equal CntBFalse.
*)

let static_check_if_gen_relation_false ( e : nts_gen_relation  ) =
  let es =  simplify_gen_rel e in
  match es with
      CntGenFalse -> true
    | _ -> false


let static_check_if_gen_translist_unsat ( l : nts_trans_label list) =
  let decide_folder unsat_previous current_label =
    if unsat_previous then true
    else
      match current_label with 
	  CntGenGuard(cond) ->
	    (static_check_if_gen_relation_false cond)
	| _ -> false (* Non guard transition are not unsat*)
  in
  List.fold_left decide_folder false l 
    

(*       *)
	    
let nts_pprint_gen_trans_label ( tlabel : nts_trans_label ) =
  match tlabel with
      CntGenGuard ( nts_rel ) ->
	begin
	  nts_pprint_genrel nts_rel
	end
	  
    | CntGenHavoc(ntslist ) ->
      begin
	let strl = pprint_ntsgen_var_list ntslist in
	Format.sprintf "havoc(%s)" strl
      end
	
    | CntGenCall(nts_sys_name,lvals_opt_varlist,param_list) ->
      begin
	match lvals_opt_varlist with
	    None ->
	      Format.sprintf "%s(%s)" nts_sys_name (pprint_gen_rel_arithm_list param_list) 
	  | Some(lhslist) -> 
	    begin
	      let str_ret_var_list = 
		pprint_ntsgen_var_list lhslist in
	      let str_param = pprint_gen_rel_arithm_list 
		param_list in
	      Format.sprintf "(%s)=%s(%s)" str_ret_var_list 
		nts_sys_name str_param
	    end
      end
	

(*Is the guard a single true ? *)
let is_label_true t =
  match t with
      CntGenGuard(CntGenTrue) -> true
    | _ -> false
      


let nts_pprint_gen_trans_label_list ( tlabellist : nts_trans_label list ) =
  let folder_of_the_day s tlabel =
    match s with
	"" ->
	  begin
	    if  not ( is_label_true tlabel )
	     then
	      nts_pprint_gen_trans_label tlabel
	    else
	      ""
	  end
      | _ ->
	begin
	  if  not ( is_label_true tlabel ) 
	  then
	    s^" and "^( nts_pprint_gen_trans_label tlabel)
	  else 
	    s
	end
  in
  List.fold_left folder_of_the_day "" tlabellist




 (** 
this function aims at splitting one transition list in two parts :
_ The guards  
_ The operations.

 *)



(*
let split_folder (pre_guards,pre_op) curr_label =
  match curr_label with
*)  


(**
Getting the type of arithemtic subtrees for typing the current node current 
*)
exception Untyped_constant of nts_base_type_cst
exception InconsistantTypingofGenVar of nts_genrel_var
exception CantTypeArithmExpression of nts_genrel_arithm_exp

let type_of_nts_var nv = 
  match nv with
    NtsVar(_,rettype) -> rettype

let type_of_genvar ngvar = 
  match ngvar with
    NtsGenVar(v,_) -> type_of_nts_var v
    
let type_of_cnt_base_cst gcst =
  match gcst with 
    CntGenICst _ -> NtsIntType
  | CntGenFCst _ -> NtsRealType
  | CntGenBCst _ -> NtsBoolType
  

let type_of_nts_sym_cst symc =
  match symc with 
    CntSymCst(_,t) -> t

(** 
Check that an arithmetic term is well typed and retunrs the
type of the root node of the term.
*)
let rec type_of_gen_arithmetic_expr aop =
  match aop with
    CntGenCst(cs,ct) -> 
      begin
	let typ_bot = type_of_cnt_base_cst cs in
	if typ_bot = ct then ct
	else raise (CantTypeArithmExpression(aop))
      end
  | CntGenSymCst(symcst,ct) -> 
    begin
      let typ_bot = type_of_nts_sym_cst symcst in
      if typ_bot = ct then ct
      else raise (CantTypeArithmExpression(aop))
    end
  
  | CntGenVar gvar -> type_of_genvar gvar
  
  | CntGenArithmBOp (_,tg,td,tup) ->
    begin
      let typ_tg = type_of_gen_arithmetic_expr tg in
      let typ_td = type_of_gen_arithmetic_expr td in
      if (typ_tg=typ_td && typ_td=tup) then
	tup
      else
	raise (CantTypeArithmExpression(aop))
    end

  | CntGenArithmUOp (_,expr,tup) ->
    begin
      let typ_exp = type_of_gen_arithmetic_expr expr in
      if ( typ_exp=tup) then
	tup
      else
	raise (CantTypeArithmExpression(aop))
    end


(** Compares the type of two arithemtic expressions*)
let arithm_exp_same_type l r =
  let tl = type_of_gen_arithmetic_expr l in
  let tg = type_of_gen_arithmetic_expr r in
  if tl=tg then Some(tl)
  else None





(**  The function below aim at building arithemtical terms without 
having to write all those verbose constructors ... *)




let make_nts_int_cst i =
  CntGenCst(CntGenICst(i),Nts_types.NtsIntType)

let make_nts_int_cst_of_int i = 
  let big_i = Big_int.big_int_of_int i in
  make_nts_int_cst big_i

let make_nts_real_cst f =
   CntGenCst(CntGenFCst(f),Nts_types.NtsRealType)

let make_nts_bool_cst b =
  if b then
    CntGenCst(CntGenBCst(CntBTrue),Nts_types.NtsBoolType)
  else
     CntGenCst(CntGenBCst(CntBFalse),Nts_types.NtsBoolType)

let make_nts_genvar name typ =
  NtsGenVar(NtsVar(name,typ),NtsUnPrimed)

let primerized_nts_var nts_var =
  match nts_var with
    NtsGenVar(ntv,_) -> NtsGenVar(ntv, NtsPrimed)
      

let affect_aexpr_to_nts_var nts_var arithm_exp =
  let primed_var = primerized_nts_var nts_var in
  CntGenRel(CntEq,CntGenVar(primed_var),arithm_exp)
  
let add_arithm_expr aeg aed =
  let tg = arithm_exp_same_type aed aed 
  in
  match tg  with 
    Some(tg) ->
      CntGenArithmBOp(CntGenSum,aed,aeg,tg)
  | None -> raise (Type_mismatch_in_arithm_expression(aeg,aed))

let sub_arithm_expr aeg aed =
  let tg = arithm_exp_same_type aed aed 
  in
  match tg  with 
    Some(tg) ->
      CntGenArithmBOp(CntGenMinus,aed,aeg,tg)
  | _ -> raise (Type_mismatch_in_arithm_expression(aeg,aed))
    
let mul_arithm_expr aeg aed =
  let tg = arithm_exp_same_type aed aed 
  in
  match tg  with 
    Some(tg) ->
      CntGenArithmBOp(CntGenProd,aed,aeg,tg)
  | _ -> raise (Type_mismatch_in_arithm_expression(aeg,aed))
    

let div_arithm_expr aeg aed =
  let tg = arithm_exp_same_type aed aed 
  in
  match tg  with 
    Some(tg) ->
      CntGenArithmBOp(CntGenDiv,aed,aeg,tg)
  | _ -> raise (Type_mismatch_in_arithm_expression(aeg,aed))

let mod_arith_expr aeg aed = 
  let tg = arithm_exp_same_type aed aed 
  in
  match tg  with 
    Some(tg) ->
      if tg=NtsIntType then 
	CntGenArithmBOp(CntGenMod,aed,aeg,tg)
      else
	raise Arithmetic_modulo_on_non_integer_type
	  
  | None -> raise (Type_mismatch_in_arithm_expression(aeg,aed))


let inv_sign_of_aexp aexp =
  let taexp = type_of_gen_arithmetic_expr aexp in
  match aexp with
    CntGenArithmUOp(CntGenUMinus,retv,_) -> retv
  | _ -> CntGenArithmUOp(CntGenUMinus,aexp,taexp)



let guard_lt_aexpr aexpg aexpd =
  CntGenRel(CntLt,aexpg,aexpd)

let guard_gt_aexpr aexpg aexpd =
  CntGenRel(CntGt,aexpg,aexpd)

let guard_leq_aexpr aexpg aexpd =
  CntGenRel(CntLeq,aexpg,aexpd)

let guard_geq_aexpr aexpg aexpd =
  CntGenRel(CntGeq,aexpg,aexpd)

let guard_eq_aexpr aexpg aexpd =
  CntGenRel(CntEq,aexpg,aexpd)

let make_transition_of_translabel (tl : nts_trans_label ) =
  tl::[]

let add_to_transition pre ( op : nts_trans_label) =
  op::pre

