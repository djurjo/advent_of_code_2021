:- module(day3, [solve_part1/2, solve_part2/2], _).

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

solve_part2(File, Solution) :-
    open(File, read, Streams),
    get_score_2(Streams, [], Score, Lines),
    get_sol(Score, Lines, Ox, CO2),
    Solution is Ox*CO2.

%%%% To solve the second part we have to get the score first and then
%%%% filter the list consisting in all the values. To do so we
%%%% modified the predicate get_scores.


%%% Predicates used in part 2.

get_sol_rec([Ox], [CO2], [Ox], [CO2]).
get_sol_rec(Ox, [CO2], SolOx, [CO2]) :-
    !,
    new_score(Ox, (0, 0), OxScore),
    most_least(OxScore, (OxV, _)),
    filter(OxV, Ox, OutOxs),
    get_sol_rec(OutOxs, [CO2], SolOx, [CO2]). 
get_sol_rec([Ox], CO2, [Ox], SolCO2) :-
    !,
    new_score(CO2, (0, 0), CO2Score),
    most_least(CO2Score, (_, CO2V)),
    filter(CO2V, CO2, OutCO2s),
    get_sol_rec([Ox], OutCO2s, [Ox], SolCO2).
get_sol_rec(Ox, CO2, SolOx, SolCO2) :-
    !,
    new_score(CO2, (0, 0), CO2Score),
    most_least(CO2Score, (_, CO2V)),
    new_score(Ox, (0, 0), OxScore),
    most_least(OxScore, (OxV, _)),
    filter(OxV, Ox, OutOxs),
    filter(CO2V, CO2, OutCO2s),
    get_sol_rec(OutOxs, OutCO2s, SolOx, SolCO2).

:- export(get_sol/4).
get_sol([Sc|_], Lines, Ox, CO2) :-
    most_least(Sc, (OxV, CO2V)),
    filter1(OxV, CO2V, Lines, OxFilt, CO2Filt),
    get_sol_rec(OxFilt, CO2Filt, [SolOx], [SolCO2]),
    get_value(SolOx, Ox),
    get_value(SolCO2, CO2).

get_value(([], Body), Value) :-
    binary_to_decimal_(Body, Value, _). 
get_value(([D|Head], Body) , Value) :-
    get_value((Head, [D|Body]), Value).
    

    
new_score([], Score, Score).
new_score([Cand|Cands], CurrScore, Score) :- 
    Cand = (_Head, Body),
    Body = [D|_],
    update_score(D, CurrScore, TempScore),
    new_score(Cands, TempScore, Score).

filter(_, [], []).
filter(Val, [CurrCand|RestCand], NewCand) :-
    CurrCand = (CurrHead, CurrBody),
    CurrBody = [Dig|RestBody],
    Dig = Val,
    !,
    NewHead = [Val|CurrHead], %% Reversed
    NewBody = RestBody,
    NewCand_ = (NewHead, NewBody),
    NewCand = [NewCand_|TempCand],
    filter(Val, RestCand, TempCand).
filter(Val, [_|RestCand], NewCand) :-
    !, filter(Val, RestCand, NewCand).
    
filter1(_, _, [], [], []).
filter1(OxV, CO2V,[[D|Line]|Data], Ox,CO2) :-
    OxV  = D, !,
    Ox = [([D], Line)|OxT],
    filter1(OxV, CO2V, Data, OxT, CO2).
filter1(OxV, CO2V,[[D|Line]|Data], Ox,CO2) :-
    CO2V  = D, !,
    CO2 = [([D], Line)|CO2T],
    filter1(OxV, CO2V, Data, Ox, CO2T).

most_least((Zero, One), (48, 49)) :-
    Zero > One, !.
most_least((Zero, One), (49, 48)).

get_score_2(Streams, CurrScore, Score, Lines) :-
    get_line(Streams, Line),
    \+ Line = end_of_file, !,
    Lines = [Line|TempLines],
    get_aparitions(Line, CurrScore, TempScore),
    get_score_2(Streams, TempScore, Score, TempLines).
get_score_2(_, Score, Score, []).


%%% Predicates used to part 1.
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

%%% Predicates used in both parts.

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

binary_to_decimal_([D], Dec, 1) :-
    number_codes(Dec, [D]).
binary_to_decimal_([D|Bin], Dec, NVal) :-
    binary_to_decimal_(Bin, TDec, Val),
    NVal is Val + 1,
    number_codes(D_, [D]),
    Dec is D_*2**Val + TDec.