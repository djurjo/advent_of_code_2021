:- module(day2, [main_1/2, main_2/2], _).

:- use_module(library(stream_utils), [get_line/2]).
:- use_module(library(streams), [open/3]).

%%%% Less time consumption
:- use_module('../../utils/parse_file.pl', [separate_row/5]).

%%% Part 1
main_1(File, Solution) :-
    open(File, read, Stream),
    solve_puzzle1(Stream, (0, 0), (Hor, Depth)),
    Solution is Hor*Depth.

%%% Part 2
main_2(File, Solution) :-
    open(File, read, Stream),
    solve_puzzle2(Stream, (0, 0, 0), (Hor, Depth, _Aim)),
    Solution is Hor*Depth.


%%% Begin -Auxiliar predicates to part 1
solve_puzzle1(Stream, Position, SolPosition) :-
    get_line(Stream, Line),
    Position = (Hor, Depth),
    get_line_info(Line, none, Move,Value),
    \+ (Value =  stop), !, 
    move_submarine1(Hor, Depth, Move, Value, NHor, NDepth),
    TempPosition = (NHor, NDepth),
    solve_puzzle1(Stream,TempPosition, SolPosition).
solve_puzzle1(_, Position, Position).

move_submarine1(CurrHor, Depth, '$forward', Value, NewHor, Depth) :-
    !, NewHor is CurrHor + Value.
move_submarine1(Hor, CurrDepth, '$down', Value, Hor, NewDepth) :-
    !, NewDepth is CurrDepth + Value.
move_submarine1(Hor, CurrDepth, '$up', Value, Hor, NewDepth) :-
    !, NewDepth is CurrDepth - Value.
%%% End -Auxiliar predicates to part 1

%%% Begin -Auxiliar predicates to part 2
solve_puzzle2(Stream, Position, SolPosition) :-
    get_line(Stream, Line),
    Position = (Hor, Depth, Aim),
    get_line_info(Line, none, Move,Value),
    \+ (Value =  stop), !, 
    move_submarine2(Hor, Depth, Aim, Move, Value, NHor, NDepth, NAim),
    TempPosition = (NHor, NDepth, NAim),
    solve_puzzle2(Stream,TempPosition, SolPosition).
solve_puzzle2(_, Position, Position).

move_submarine2(Hor, Depth,Aim, '$forward', Value, NewHor, NewDepth, Aim) :-
    !,
    NewHor is Hor + Value,
    NewDepth is Depth + Aim*Value.
move_submarine2(Hor, Depth, Aim, '$down', Value, Hor, Depth, NewAim) :-
    !, NewAim is Aim + Value.
move_submarine2(Hor, Depth, Aim, '$up', Value, Hor, Depth, NewAim) :-
    !, NewAim is Aim - Value.
%%% End -Auxiliar predicates to part 2

%%% Begin -Auxiliar predicates both parts

%%%% Move:
%%%%                 last
%%%% forward -> f -> d
%%%% down    -> d -> n
%%%% up      -> u -> p
move(100, '$forward').
move(110, '$down').
move(112, '$up').

get_line_info(end_of_file, _, _, stop). % Ugly hack, avoid calling get_line twice.
get_line_info([C|Line], Prev, Move, Value) :-
    C = 32, !,%% Blank
    move(Prev, Move),
    number_codes(Value, Line).
get_line_info([C|Line], _Prev, Move, Value) :-
    get_line_info(Line, C, Move, Value).
    
