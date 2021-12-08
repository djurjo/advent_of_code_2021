:- module(day7, _, [assertions,dynamic]).

:- use_module(library(stream_utils), [get_line/2]). 
:- use_module(library(streams), [open/3]).
:- use_module('../../utils/parse_file.pl', [separate_row/5]).
    
solvepart1(File, Sol) :-
    open(File, read, Streams),
    get_line(Streams, Line),
    get_data(Line, Positions, Length),
    quick_sort(Positions, Sorted),
    median(Sorted, Length, Pos),
    fuel_consumption(Sorted, Pos, Sol).

solvepart2(File, Sol) :-
    open(File, read, Streams),
    get_line(Streams, Line),
    get_data2(Line, List, Length, Sum),
    X is Sum/Length,
    round(X, Pos, low),
    fuel_consumption2(List, Pos, Sol).

get_data2([], [], 0, 0).
get_data2(Line, Positions, Length, Sum) :-
    separate_row(Line, Value, [], Rest, 44),
    number_codes(Position, Value), 
    Positions = [Position|PositionTemp],
    get_data2(Rest, PositionTemp, TLength, TSum),
    Length is TLength + 1,
    Sum is TSum + Position.

fuel_consumption2([], _, 0).
fuel_consumption2([P|Rest], Target, Fuel) :-
    Dist is abs(Target - P),
    fuel_consumption2(Rest, Target, FuelTemp),
    Fuel_ is (Dist*(1 + Dist))/2,
    Fuel is FuelTemp + Fuel_.


get_data([], [], 0).
get_data(Line, Positions, Length) :-
    separate_row(Line, Value, [], Rest, 44),
    number_codes(Position, Value), ! ,
    Positions = [Position|PositionTemp],
    %% assert(crabs(1, Position)),
    get_data(Rest, PositionTemp, TLength),
    Length is TLength + 1.



fuel_consumption([], _, 0).
fuel_consumption([P|Rest], Target, Fuel) :-
    Dist is abs(Target - P),
    fuel_consumption(Rest, Target, FuelTemp),
    Fuel is FuelTemp + Dist.
    
median(List, Length, Sol) :-
    X is mod(Length, 2),
    X = 1,
    !,
    T is Length // 2,
    takeNth(List, T, Sol).
median(List, Length, Sol) :-
    !,
    T is Length // 2,
    takeNth2(List, T, (S1, S2)),
    Sol_ is (S1 + S2)/2,
    round(Sol_, Sol, up).

round(Sol_, Sol, up) :-
    float(Sol_), !, 
    Sol is floor(Sol_) + 1.
round(Sol_, Sol, low) :-
    float(Sol_), !, 
    Sol is floor(Sol_).
round(Sol, Sol, _).
%%% Quick sort from http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html

quick_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
    pivoting(H,T,L1,L2),
    q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-
    X=<H,!,
    pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-
    X>H, !,
    pivoting(H,T,L,G).

takeNth([H|_Tail], Pos, Nth) :-
    Pos = 0, !,
    Nth = H.
takeNth([_|Tail], Pos, Nth) :-
    !,
    NPos is Pos - 1,
    takeNth(Tail, NPos, Nth).

takeNth2([H1, H2|_], Pos, Nth) :-
    Pos = 0, !,
    Nth = (H1, H2).
takeNth2([_|Tail], Pos, Nth) :-
    !,
    NPos is Pos - 1,
    takeNth2(Tail, NPos, Nth).
