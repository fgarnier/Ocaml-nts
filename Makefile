#
TYPES_INCLUDES := ${FRAMA_C_INCLUDES} 
GENSOURCE := ntl_parser.ml ntl_parser.mli ntl_lexer.ml trace_eldarica_lexer.ml trace_eldarica_parser.ml trace_eldarica_parser.mli sid_table_info_parser.ml sid_table_info_lexer.ml
 
SOURCE_FILES := nts_types.ml nts.ml nts_functor.ml ntsint.ml nts_generic.ml\
	     dot_driver.ml trace_types.ml trace.ml trace_analysis.ml parsing_error.ml

OCAMLDOC := ocamldoc
DOCEXPORTDIR := ./api
DOCFLAGS := -html -d $(DOCEXPORTDIR) *.ml *.mli

CFLAGS = -g -annot -c
INTERFACES = ntl_parser.mli
OBJECTS = nts_types.cmo trace_types.cmo trace.cmo nts.cmo simplification.cmo nts_generic.cmo nts_functor.cmo ntsint.cmo \
	trace_analysis.cmo \
	dot_driver.cmo \
	trace_eldarica_parser.cmo \
	trace_eldarica_lexer.cmo \
	sid_table_info_parser.cmo \
	sid_table_info_lexer.cmo \
	nts_spl_intermediate_language_types.cmo \
	interproc_driver.cmo \
	ntl_lexer.cmo parsing_error.cmo \
	ntl_parser.cmo \
	expand_eldarica_compressed_bloc.cmo  

XOBJECTS = nts_types.cmx trace_types.cmx trace.cmx nts.cmx simplification.cmx nts_generic.cmx nts_functor.cmx ntsint.cmx \
	dot_driver.cmx \
	trace_analysis.cmx \
	trace_eldarica_parser.cmx \
	trace_eldarica_lexer.cmx \
	sid_table_info_parser.cmx \
        sid_table_info_lexer.cmx \
	nts_spl_intermediate_language_types.cmx \
	interproc_driver.cmx \
	ntl_lexer.cmx parsing_error.cmx \
	ntl_parser.cmx \
	expand_eldarica_compressed_bloc.cmx 

LIBNUM = nums.cma # Contains Big_int module
LIBXNUM = nums.cmxa
   
INTERFACES = $(OBJECTS:.cmo=.cmi)
TEST = parse_n_print
DATE = `eval date +%Y%m%d`
DIST_TAR_NAME = ocaml_ntlib$(DATE).tgz
 
clean : 
	rm -f *.cmo *.o *.cmx *.cmxa *.cmi *.cma *.annot parse_n_print xparse_n_print $(GENSOURCE)

all : .depend $(INTERFACES) $(OBJECTS) $(XOBJECTS) $(TEST) parse_n_print nts2dot nts2dot_subgraph test_parse_sidinfo
	@echo "Build successfull"

dist : all clean
	@echo "Building distribution tarball"
	
	cd .. && echo `pwd` && tar czvf $(DIST_TAR_NAME) ntl_lib/ && cd ntl_lib/
	cp ../$(DIST_TAR_NAME) ./

dist_no_fixpoint : .depend $(INTERFACES) $(OBJECTS) $(XOBJECTS) $(TEST) clean
	 cd .. && echo `pwd` && tar czvf $(DIST_TAR_NAME) ntl_lib/ && cd ntl_lib/
	cp ../$(DIST_TAR_NAME) ./
%.cmo : %.ml
	@echo  "Compiling"  $<
	@ocamlc ${CFLAGS}  $< -o $@ ${TYPES_INCLUDES}

%.cmi : %.mli
	@echo  "Compiling interface " $<
	@ocamlc ${CFLAGS}  $< -o $@ $(TYPES_INCLUDES)

%.cmx : %.ml
	@echo  "OPT Compiling"  $<
	@ocamlopt ${CFLAGS}  $< -o $@ ${TYPES_INCLUDES}

ntl_lexer.cmi : ntl_parser.mli

ntl_parser.mli : ntl_parser.ml


ntl_parser.ml : ntl_parser.mly
	@echo  "Generating Parser" $<
	@ocamlyacc $<

ntl_lexer.ml : ntl_lexer.mll
	@echo  "Generating Lexer" $<
	@ocamllex $<



trace_eldarica_lexer.cmi : trace_eldarica_parser.cmi

trace_eldarica_parser.cmi : trace_eldarica_parser.ml


trace_eldarica_parser.ml : trace_eldarica_parser.mly
	@echo  "Generating Parser" $<
	@ocamlyacc $<

trace_eldarica_lexer.ml : trace_eldarica_lexer.mll
	@echo  "Generating Lexer" $<
	@ocamllex $<


sid_table_info_parser.cmi : sid_table_info_parser.mly


sid_table_info_lexer.cmi : sid_table_info_parser.cmi

sid_table_info_parser.cmi : sid_table_info_parser.ml


sid_table_info_lexer.ml : sid_table_info_lexer.mll sid_table_info_parser.cmi 
	@echo  "Generating Lexer" $<
	@ocamllex $<	


sid_table_info_parser.ml :  sid_table_info_parser.mly trace_types.cmi trace_types.cmx
	@echo  "Generating Parser" $<
	@ocamlyacc $<
		

parse_n_print : parse_n_print.cmo parse_n_print.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling parse_n_print test"
	ocamlc -g -o parse_n_print $(LIBNUM) $(OBJECTS) parse_n_print.cmo 
	ocamlopt -g -o xparse_n_print  $(LIBXNUM) $(XOBJECTS) parse_n_print.cmx 

nts2dot	: nts2dot.cmo nts2dot.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling nts2dot"
	ocamlc -g -o nts2dot $(LIBNUM) $(OBJECTS) nts2dot.cmo 
	ocamlopt -g -o xnts2dot  $(LIBXNUM) $(XOBJECTS) nts2dot.cmx 


nts2dot_subgraph : nts2dot_subgraph_info.cmo nts2dot_subgraph_info.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling nts2dot"
	ocamlc -g -o nts2dot_subgraph $(LIBNUM) $(OBJECTS) nts2dot_subgraph_info.cmo 
	ocamlopt -g -o xnts2dot_subgraph  $(LIBXNUM) $(XOBJECTS) nts2dot_subgraph_info.cmx 

test_parse_trace : test_trace_parsing.cmo test_trace_parsing.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling test parse trace"
	ocamlc -g -o print_trace $(LIBNUM) $(OBJECTS) test_trace_parsing.cmo 
	ocamlopt -g -o xprint_trace  $(LIBXNUM) $(XOBJECTS) test_trace_parsing.cmx 

test_parse_sidinfo : test_parse_sid_table.cmo test_parse_sid_table.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling test_parse_sid_table"
	ocamlc -g -o test_parse_sid_table $(LIBNUM) $(OBJECTS) test_parse_sid_table.cmo 
	ocamlopt -g -o xtest_parse_sid_table  $(LIBXNUM) $(XOBJECTS) test_parse_sid_table.cmx 

trace_upon_statement_info : trace_upon_statement_info.cmo trace_upon_statement_info.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling trace_upon_statemen_info"
	ocamlc -g -o trace_upon_statement_info $(LIBNUM) $(OBJECTS) trace_upon_statement_info.cmo 
	ocamlopt -g -o xtrace_upon_statment_info  $(LIBXNUM) $(XOBJECTS) trace_upon_statement_info.cmx


trace2nts : export_trace_nts.cmo export_trace_nts.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling trace compiler"
	ocamlc -g -o trace2nts $(LIBNUM) $(OBJECTS) export_trace_nts.cmo 
	ocamlopt -g -o xtrace2nts $(LIBXNUM) $(XOBJECTS)  export_trace_nts.cmx 

test_block_compression : test_block_compression.cmo test_block_compression.cmx $(OBJECTS) $(XOBJECTS)
	@echo "Compiling test block comression"
	ocamlc -g -o test_block_compress $(LIBNUM) $(OBJECTS) test_block_compression.cmo
	ocamlopt -g -o xtest_block_compress $(LIBXNUM) $(XOBJECTS) test_block_compression.cmx
include .depend

.depend: $(GENSOURCE)  
	ocamldep *.ml *.mli > .depend

fixpointvalid :
	@echo "Fixpoint test for parse_n_print"
	@python test_passes.py

docgen : $(OBJECTS)
	@echo "Generating API"
	@$(OCAMLDOC) $(DOCFLAGS) 


