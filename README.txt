



This folder contains the files that define the OCAML version
of the Numerical Transition Systel Library, which sepecification
is available here : http://richmodels.epfl.ch/ntscomp.


I)  Requirements :
------------------


 _ Ocaml compiler v3.12 or over.
 _ Python interpreter. 


II) Compilation :
-----------------

_ make all : Builds all objects modules and utilities tools.
_ make docgen : Builds the html api using ocamldoc.


III) General Description :
-------------------------

The files nts_functor.ml and nts_functor.mli define a fonctorial 
interface which allows to deal with a wide range of numerical 
transitions systems. 

In its current state of developpement, this library contains :

 _ A parser : (Provides a function that returns a numerical transition
	system from a text input, developped using ocamllex and ocamlyacc.
	The current parser is defined for the nts which control states 
	are labelled using integers.
	)

 _ A pretty printer.

 _ A set of functions that allow to extract a GraphViz .dot visual
 representation of a Numerical Transition System.

 _ A set of functions and types that allows to analyse traces of numerical
 transitions systems : 
  We define some functions that allow to map traces back to the nts :
 Full traces, as well as uncomplete traces -- traces where only calls,
 branching control flow operations and subsystem return are present, print
 trace enveloppe on .dot format.

 We provide as well some utilities that allow to generate a Numerical
 transition system from a trace. This is usefull to check whether a trace or
 a counter example trace is satifiable using thirg party tool --e.g. flata
 or eldarica.   



IV) Utilities tools :
-------------------

Tools generated during the compilation : 


parse_n_print : 
Used to parse a .nts file, and print it using a noramlization procedure. All
non used variables are removed from subsystems.

nts2dot :
Takes as input a numerical transition system and prints the collection of
subsytems in the Graphviz .dot format for visualizing the control structure
of numerical transition systems. Initial control states are printed in blue,
final state in green and error stated are printed in red. Transitions that
corresponds to interprocedural calls are printed in red.

To compile the .dot file into the ps resp. pdf format, use the command below :

 dot -Tps out.dot -o out.ps
 dot -Tpdf out.dot -o out.pdf


nts2dot_subgraph : 


nts2dot_subgraph file.nts : Does the same thing as nts2dot.
nts2dot_subgraph file.nts -interval trace_file.txt :

 Draws an enveloppe of the trace described with trace_file.txt on the
Graphviz representation the nts described in file.nts .


An execution trace is represented a comma separated list of control state :

List((subsystem_name,control_state_id),...,(subsystem,last_control_state))

All sysbsystems in the trace name must either be defined in file.nts or in the 
base_fun.ca_lib file, that contains a collection of subsystems that are supposed
to be defined as a standard. 


trace2nts :

 This program takes as input a nts file and a trace description, an computes
a numerical transition system that contains the trace given as input.
Each time the execution trance enters a subsystems, one define a new
subsystem that contains the trace in the corresponding context.


This program is useful for checking whether a counter example trace provided
by one tool is satifiable using an analysis technique provided by another one.
E.g : The satisfiability of traces provided using eldarica were checked using
    flata.


 
V) Modules Decription :
----------------------


nts_functor(.ml/.mli) : Types definition for numerical transitions subsystems
		       and numerical transition systems, as well as functions
		       used for : Pretty printing, parsin and normalizing 
		       subsystems.


nts_types(.ml/mli) : Type definition for nts arithmetic and
		    nts subsystems transitions.
nts(.ml/mli) and
nts_generic(.ml/mi) : Operations used to handle arithmetical expressions and
		      numerical systems transitions. E.g: Pretty printing,
		      some basic simplifications and other operations on
		      syntactic trees.


ntl_lexer.mll/ntl_parser.mly : lexer and parser generator file for nts parser.


dot_driver(.ml/.mli) : functor used to generate a graphical representation
		     of nts files into the .dot graphviz format. Handle
		     subgraph representations and drawing trace enveloppe
		     from trace.

simplification(.ml/.mli) : function used to remove unused defined variable
			 in nts and some unsat transitions.   




(c) Verimag 2012-2013.
 For any question or remark, please write to florent dot garnier at gmail dot com 


