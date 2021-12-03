:- module(day3, [solve_part1/2], _).

:- use_module(library(stream_utils), [get_line/2]). 
%%%% Less time consumption
:- use_module(library(streams), [open/3]).

%%:- use_module(library(llists), [transpose/2]). 
%%% With less code. Put stream as rows and transpose it. Then calculate.

solve_part1(File, Solution) :-
    open(File, read, Streams),
    get_score(Streams, [], Score),
    gamma_epsilon(Score, Gamma, Epsilon),
    binary_to_decimal(Gamma, GammaD, _),
    binary_to_decimal(Epsilon, EpsilonD, _),
    Solution is GammaD * EpsilonD.

get_score(Streams, CurrScore, Score) :-
    get_line(Streams, Line),
    \+ Line = end_of_file, !,
    get_aparitions(Line, CurrScore, TempScore),
    get_score(Streams, TempScore, Score).
get_score(_, Score, Score).

gamma_epsilon([], [], []).
gamma_epsilon([(Zero, One)|Score], Gamma, Epsilon) :-
    translate((Zero, One), (GamVal, EpsVal)),
    Gamma = [GamVal|GamTemp],
    Epsilon = [EpsVal|EpsTemp],
    gamma_epsilon(Score, GamTemp, EpsTemp).
   
get_aparitions([], [], []).
get_aparitions([Value|Line], [], Scores) :-
    update_score(Value, (0, 0), NScore),
    Scores = [NScore|TScores],
    get_aparitions(Line, [], TScores).
get_aparitions([Value|Line], [CScore|CurrScores], Scores) :-
    update_score(Value, CScore, NScore),
    Scores = [NScore|TScores],
    get_aparitions(Line, CurrScores, TScores).

update_score(48, (Zero, One), (NZero, One)) :- NZero is Zero + 1.
update_score(49, (Zero, One), (Zero, NOne)) :- NOne is One + 1.

translate((Zero, One), (0, 1)) :-
    Zero > One, !.
translate(_, (1, 0)).

binary_to_decimal([D], D, 1).
binary_to_decimal([D|Bin], Dec, NVal) :-
    binary_to_decimal(Bin, TDec, Val),
    NVal is Val + 1,
    Dec is D*2**Val + TDec.