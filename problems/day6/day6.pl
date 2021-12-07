:- module(day6, [solve_part1/2, solve_part2/2], [assertions,dynamic]).

:- use_module(library(stream_utils), [get_line/2]). 
:- use_module(library(streams), [open/3]).
:- use_module('../../utils/parse_file.pl', [separate_row/5]).
    
:- dynamic(lant_fish/2).


lant_fish(0, 0).
lant_fish(0, 1).
lant_fish(0, 2).
lant_fish(0, 3).
lant_fish(0, 4).
lant_fish(0, 5).
lant_fish(0, 6).
lant_fish(0, 7).
lant_fish(0, 8).

%%% lant_fish(N, T) -> number of lanternfish with internal timer T 


solve_part1(File, Sol) :-
    get_data(File),
    solve(80, Sol).

solve_part2(File, Sol) :-
    get_data(File),
    solve(256, Sol).
    
solve(0, Sol) :-
    count_fish(Sol).
solve(Days, Sol) :-
    NDays is Days - 1,
    update_fish,
    solve(NDays, Sol).
    

add_fish(Fish) :-
    lant_fish(N, Fish), !,
    M is N + 1,
    retract(lant_fish(N, Fish)),
    assert(lant_fish(M, Fish)).

get_data(File) :-
    open(File, read,Streams),
    get_line(Streams, Line),
    initial_pop(Line).

initial_pop([]).
initial_pop(Line) :-
    separate_row(Line, Fish_code, [], Rest, 44),
    number_codes(Fish, Fish_code),
    add_fish(Fish),
    initial_pop(Rest).

count_fish(C) :-
    lant_fish(X0, 0),
    lant_fish(X1, 1),
    lant_fish(X2, 2),
    lant_fish(X3, 3),
    lant_fish(X4, 4),
    lant_fish(X5, 5),
    lant_fish(X6, 6),
    lant_fish(X7, 7),
    lant_fish(X8, 8), 
    C is X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8.

update_fish :-
    lant_fish(X0, 0),
    lant_fish(X1, 1),
    lant_fish(X2, 2),
    lant_fish(X3, 3),
    lant_fish(X4, 4),
    lant_fish(X5, 5),
    lant_fish(X6, 6),
    lant_fish(X7, 7),
    lant_fish(X8, 8),
    retract(lant_fish(X0, 0)),
    retract(lant_fish(X1, 1)),
    retract(lant_fish(X2, 2)),
    retract(lant_fish(X3, 3)),
    retract(lant_fish(X4, 4)),
    retract(lant_fish(X5, 5)),
    retract(lant_fish(X6, 6)),
    retract(lant_fish(X7, 7)),
    retract(lant_fish(X8, 8)),

    assert(lant_fish(X1, 0)),
    assert(lant_fish(X2, 1)),
    assert(lant_fish(X3, 2)),
    assert(lant_fish(X4, 3)),
    assert(lant_fish(X5, 4)),
    assert(lant_fish(X6, 5)),
    NX is X7 + X0, 
    assert(lant_fish(NX, 6)),
    assert(lant_fish(X8, 7)),
    assert(lant_fish(X0, 8)).
    