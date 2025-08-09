:- module(stubs, [
    on_signal/3,
    use_foreign_library/1,
    opt_arguments/3,
    opt_help/2,
    writeln/1,
    exists_file/1,
    compound_name_arguments/3,
    nb_setval/2,
    nb_getval/2,
    atom_string/2,
    ignore/1
]).

:- use_module(library(files)).
:- use_module(library(error)).
:- use_module(library(si)).

ignore(Goal) :- (once(Goal) -> true ; true).

on_signal(_,_,_).

use_foreign_library(_).

opt_arguments(_,[config("./config/config.pl")],_).

opt_help(_,_).

writeln(S) :- format("~s~n", [S]).

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
    ( globals(Var, Value) -> true
    ; existens_error(variable, Var)
    ),
    copy_term(Value, Copy).

atom_string(Atom, Chars) :-
    ( var(Chars) ->
        ( atom(Atom) ->  atom_chars(Atom, Chars)
        ; chars_si(Atom) -> Atom = Chars
        ; number_chars(Atom, Chars)
        )
    ; var(Atom) -> (
        (atom(Chars) -> Chars = Atom
        ; chars_si(Chars) -> atom_chars(Atom, Chars)
        ; number_chars(Chars, C), atom_chars(Atom, C)
        )
    )).
