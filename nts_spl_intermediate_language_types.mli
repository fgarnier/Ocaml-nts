type ntl_spl_il_label = string
type point = ntl_spl_il_label
type ntl_spl_il_instruction =
    NS_Skip
  | NS_Halt
  | NS_Fail
  | NS_Assume
  | NS_If of Nts_types.nts_gen_relation * ntl_spl_il_label *
      ntl_spl_il_label option
  | NS_Goto of ntl_spl_il_label
  | NS_Call of Nts_types.nts_var list option * string *
      Nts_types.nts_genrel_arithm_exp list
  | NS_local of bool * Nts_types.nts_var list * ntl_spl_block
and ntl_spl_il_instr = {
  ns_insturction : ntl_spl_il_instruction option;
  ns_ipoint : point;
}
and ntl_spl_block = { ns_bpoint : point; ns_instrs : ntl_spl_il_instr list; }
type nts_spl_il_procedure = {
  ns_pname : string;
  ns_pinput : Nts_types.nts_var list;
  ns_poutput : Nts_types.nts_var list;
  ns_pcode : ntl_spl_block;
}
type nts_spl_il_program = {
  ns_global : Nts_types.nts_var list;
  ns_initial : Nts_types.nts_genrel_arithm_exp;
  ns_final : Nts_types.nts_genrel_arithm_exp;
  ns_procedures : (string * nts_spl_il_procedure) list;
  ns_threads : string list;
}
