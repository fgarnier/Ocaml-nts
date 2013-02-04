



This folder contains the files that define the OCAML version
of the Numerical Transition Systel Library, which sepecification
is available here : http://richmodels.epfl.ch/ntscomp.

Requirements :
 _ Ocaml compiler v3.12 or over.
 _ Python interpreter. 

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


 _ Syntactic and type checking : Developpment in progress.

To build and test the library :

 make  all

The api can be generated using the docgen targe of the aforementionned
makefile.

make docgen
 


Tools generated during the compilation time 




(c) Verimag 2012.
 For any question or remark, please write to florent dot garnier at imag dot fr 


