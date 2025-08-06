:- module(stubs, [
    on_signal/3,
    use_foreign_library/1,
    opt_arguments/3,
    opt_help/2,
    writeln/1
]).

on_signal(_,_,_).

use_foreign_library(_).

opt_arguments(_,[config("./config/config.pl")],_).

opt_help(_,_).

writeln(S) :- write(S), nl.
