:- module(stubs, [
    on_signal/3,
    use_foreign_library/1,
    opt_arguments/3,
    opt_help/2,
    writeln/1,
    exists_file/1,
    compound_name_arguments/3,
    nb_setval/2,
    nb_getval/2
]).

:- use_module(library(files)).
:- use_module(library(error)).

on_signal(_,_,_).

use_foreign_library(_).

opt_arguments(_,[config("./config/config.pl")],_).

opt_help(_,_).

writeln(S) :- write(S), nl.

exists_file(Path) :- file_exists(Path).

compound_name_arguments(Compound, Name, Args) :- Compound =.. [Name | Args].

:- dynamic(globals/2).

nb_setval(Var, Value) :-
    must_be(atom, Var),
    retractall(globals(Var, _)),
    copy_term(Value, Copy),
    assertz(globals(Var, Copy)).

nb_getval(Var, Copy) :-
    must_be(atom, Var),
    globals(Var, Value),
    copy_term(Value, Copy).
