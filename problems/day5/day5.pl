:- module(day5, _, [assertions,dynamic]).

:- use_module(library(stream_utils), [get_line/2]). 
:- use_module(library(streams), [open/3]).
:- use_module('../../utils/parse_file.pl', [separate_row/5]).
    
:- export(_).

:- dynamic(app/2).
:- dynamic(twice/2).


part1(File, Sol) :-
    open(File, read, Streams),
    get_global_cover(Streams),
    count_twice(Sol).
    
part2(File, Sol) :-
    open(File, read, Streams),
    get_global_cover2(Streams),
    count_twice(Sol).
    
add_app(X, Y) :-
    twice(X, Y), !.
add_app(X, Y) :-
    app(X, Y),!,
    assert(twice(X, Y)).
add_app(X, Y) :-
    assert(app(X, Y)).

count_twice(Counter) :-
    twice(X, Y), !,
    retract(twice(X, Y)),
    count_twice(Count),
    Counter is Count + 1.
count_twice(0).
    
update_count(Y, Count, NCount) :-
    Y >= 2, !,
    NCount is Count + 1.
update_count(_, Count, Count).

    
get_global_cover2(Streams) :-
    get_line(Streams, Line),
    \+ Line = end_of_file, !,
    get_cords(Line, Cord1, Cord2),
    get_cover2(Cord1, Cord2),
    get_global_cover2(Streams).
get_global_cover2(_).

get_global_cover(Streams) :-
    get_line(Streams, Line),
    \+ Line = end_of_file, !,
    get_cords(Line, Cord1, Cord2),
    get_cover(Cord1, Cord2),
    get_global_cover(Streams).
get_global_cover(_).

get_cords(Line, Cord1, Cord2) :-
    separate_row(Line, Part1, [], RestLine, 32), %% atom_codes(' ', 32).
    separate_row(RestLine, _Arrow, [], Part2, 32),
    get_cord(Part1, Cord1),
    get_cord(Part2, Cord2).
    
get_cover((X1, Y1), (X2, Y2)) :-
    X1 = X2, !,
    get_hor_cover(X1, Y1, Y2).
get_cover((X1, Y1), (X2, Y2)) :-
    Y1 = Y2, !,
    get_ver_cover(Y1, X1, X2).
get_cover(_, _). %% Does nothing

get_cover2((X1, Y1), (X2, Y2)) :-
    X1 = X2, !,
    get_hor_cover(X1, Y1, Y2).
get_cover2((X1, Y1), (X2, Y2)) :-
    Y1 = Y2, !,
    get_ver_cover(Y1, X1, X2).
get_cover2(X, Y) :-
    !, 
    get_diag_cover(X, Y).

get_diag_cover((X1, Y1), (X2, Y2)) :-
    X1 = X2,
    Y1 = Y2, !, 
    add_app(X1, Y1).
get_diag_cover((X1, Y1), (X2, Y2)) :-
    X1 < X2,
    Y1 < Y2, !,
    add_app(X1, Y1),
    X1N is X1 + 1,
    Y1N is Y1 + 1,
    get_diag_cover((X1N, Y1N), (X2, Y2)).
get_diag_cover((X1, Y1), (X2, Y2)) :-
    X1 < X2,
    Y1 > Y2, !,
    add_app(X1, Y1),
    X1N is X1 + 1,
    Y1N is Y1 - 1,
    get_diag_cover((X1N, Y1N), (X2, Y2)).
get_diag_cover((X1, Y1), (X2, Y2)) :-
    X1 > X2,
    Y1 < Y2,
    !,
    add_app(X1, Y1),
    X1N is X1 - 1,
    Y1N is Y1 + 1, 
    get_diag_cover((X1N, Y1N), (X2, Y2)).
get_diag_cover((X1, Y1), (X2, Y2)) :-
    X1 > X2,
    Y1 > Y2,
    !,
    add_app(X1, Y1),
    X1N is X1 - 1,
    Y1N is Y1 - 1, 
    get_diag_cover((X1N, Y1N), (X2, Y2)).

get_hor_cover(X, Y1, Y2) :-
    Y1 = Y2, !,
    add_app(X, Y1).
get_hor_cover(X, Y1, Y2) :-
    Y1 < Y2, !,
    add_app(X, Y1),
    Y1N is Y1 + 1,
    get_hor_cover(X, Y1N, Y2).
get_hor_cover(X, Y1, Y2) :-
    Y1 > Y2, !,
    add_app(X, Y1), 
    Y1N is Y1 - 1, 
    get_hor_cover(X, Y1N, Y2).

get_ver_cover(Y, X1, X2) :-
    X1 = X2, !,
    add_app(X1, Y).
get_ver_cover(Y, X1, X2) :-
    X1 > X2, !,
    add_app(X1, Y),
    X1N is X1 - 1,
    get_ver_cover(Y, X1N, X2).
get_ver_cover(Y, X1, X2) :-
    X1 < X2, !,
    add_app(X1, Y),
    X1N is X1 + 1,
    get_ver_cover(Y, X1N, X2).

get_cord(PointStr, (X, Y)) :-
    separate_row(PointStr, XStr, [], YStr, 44),
    number_codes(X, XStr),
    number_codes(Y, YStr). 
