:- module(day4, _, _).

:- use_module(library(stream_utils), [get_line/2]). 
:- use_module(library(streams), [open/3]).
:- use_module('../../utils/parse_file.pl', [separate_row/5]).
    
:- export(_).

%%%%% LET'S PLAY BINGO


solve_bingoP1(File, Sol) :-
    open(File, read, Streams),
    get_line(Streams, Numbers_),
    get_num(Numbers_,[],Numbers,44),
    get_line(Streams, _),
    get_winner(Streams, Numbers, (0.Inf, [], 0), Best),
    Best = (_,Marked , SumCard),
    Marked = [(LastNumber, _, _)|_],
    sum_marked(Marked, Sum),
    Sol is (SumCard - Sum)*LastNumber.


solve_bingoP2(File, Sol) :-
    open(File, read, Streams),
    get_line(Streams, Numbers_),
    get_num(Numbers_,[],Numbers,44),
    get_line(Streams, _),
    get_last_winner(Streams, Numbers, (-0.Inf, [], 0), Best),
    Best = (_,Marked , SumCard),
    Marked = [(LastNumber, _, _)|_],
    sum_marked(Marked, Sum),
    Sol is (SumCard - Sum)*LastNumber.

sum_marked([], 0).
sum_marked([(LastNumber, _, _)|R], Sum) :-
    sum_marked(R, TempSum),
    Sum is LastNumber + TempSum.


get_winner(Streams, Numbers, CurrBest, Best) :-
    get_card(Streams, Card, 1, SumC),
    SumC > 0, 
    !,
    check_card(Numbers, Card, [], Marked, 0, _Size, Count),
    isBest(CurrBest, (Count, Marked, SumC), NBest),
    get_winner(Streams, Numbers, NBest, Best).
get_winner(_, _, Best, Best) :-
    !.

get_last_winner(Streams, Numbers, CurrBest, Best) :-
    get_card(Streams, Card, 1, SumC),
    SumC > 0, 
    !,
    check_card(Numbers, Card, [], Marked, 0, _Size, Count),
    isworse(CurrBest, (Count, Marked, SumC), NBest),
    get_last_winner(Streams, Numbers, NBest, Best).
get_last_winner(_, _, Best, Best) :-
    !.


isworse((CurrBest,_CurrM, _CurrSum), (CandBest, Marked, Sum), (CandBest, Marked, Sum)) :-
    CurrBest =< CandBest, !.
isworse((CurrBest,CurrM, CurrSum), (_CandBest, _Marked, _Sum),(CurrBest, CurrM, CurrSum)).

isBest((CurrBest,_CurrM, _CurrSum), (CandBest, Marked, Sum), (CandBest, Marked, Sum)) :-
    CurrBest >= CandBest, !.
isBest((CurrBest,CurrM, CurrSum), (_CandBest, _Marked, _Sum),(CurrBest, CurrM, CurrSum)).

flag_line(end_of_file) :-
    !, false.
flag_line(_).

get_line_(Streams, Line) :-
    get_line(Streams, Line),
    flag_line(Line).
    
get_card(Streams, Card, N, Sum) :-
    get_line_(Streams, Line),
    \+ Line = [], !, 
    get_row(Line, [], Row, 32, (N, 1), RowSum),
    Card = [Row|TempCard],
    M is N + 1,
    get_card(Streams, TempCard, M, TSum),
    Sum is RowSum + TSum.
get_card(_, [], _, 0).

get_num([], Row, Row,  _).
get_num(Line, Row, RowC, Sep) :-
    Line = [Sep|Rest], !,
    get_num(Rest, Row, RowC, Sep).
get_num(Line, Row, RowC, Sep) :-
    separate_row(Line, Number_code, [], RestLine, Sep),
    number_codes(Number, Number_code),
    RowC = [Number|RowT],
    get_num(RestLine, Row, RowT, Sep).

get_row([], Row, Row, _, _, 0).
get_row(Line, Row, RowC, Sep, Pos, Sum) :-
    Line = [Sep|Rest], !,
    get_row(Rest, Row, RowC, Sep, Pos, Sum).
get_row(Line, Row, RowC, Sep, (R, C), Sum) :-
    separate_row(Line, Number_code, [], RestLine, Sep),
    number_codes(Number, Number_code),
    RowC = [(Number, R, C)|RowT],
    NC is C + 1,
    get_row(RestLine, Row, RowT, Sep, (R, NC), PSum),
    Sum is Number + PSum.
    
%% check_card( +, +, +, -,  +, -, -).
check_card([Val|_], Card, Marked, NMarked, MSize, NMSize, Counter) :-
    check_number_card(Val, Card, Marked, TempMarked),     
    %%%If the number doesn't exists in the card the predicate fails
    TempMSize is MSize + 1,
    win(TempMarked, TempMSize), 
    !,
    NMSize = TempMSize,
    Counter = 0,
    NMarked = TempMarked.
check_card([Val|Numbers], Card, Marked, NMarked, MSize, NMSize, Counter) :-
    check_number_card(Val, Card, Marked, TempMarked),     
    %%%If the number doesn't exists in the card the predicate fails
    %%% Does not win
    !,
    TempMSize is MSize + 1,
    check_card(Numbers, Card, TempMarked, NMarked, TempMSize, NMSize, TempCount),
    Counter is TempCount + 1.
check_card([_|Numbers], Card, Marked, NMarked, MSize, NMSize, Counter) :-
    !, %% Number not in Card
    check_card(Numbers, Card, Marked, NMarked, MSize, NMSize, Counter).
check_card([], _, _, [], _, -0.Inf, 0.Inf).


win(_, Size) :-
    Size < 5, !, false.
win([M|Marked], _) :-
    winCol(M, Marked), !.
win([M|Marked], _) :-
    winRow(M, Marked), !.
win([_M|Marked], MSize) :-
    !, NMsize is MSize - 1,
    win(Marked, NMsize).

winCol(Target, Marked) :-
    winCol_(Target, Marked, Count),
    Count = 5, !.
    
winCol_(_, [], 1).
winCol_((_V, _R, C), [(_, _, C)|Marks], Count) :-
    winCol_((_V, _R, C), Marks, NCount),
    Count is NCount + 1.
winCol_(T, [_|Mark], Count) :-
    winCol_(T, Mark, Count).

winRow(Target, Marked) :-
    winRow_(Target, Marked, Count),
    Count = 5, !.

winRow_(_, [], 1).
winRow_((_V, R, _C), [(_, R, _)|Marks], Count) :-
    winRow_((_V, R, _C), Marks, NCount),
    Count is NCount + 1.
winRow_(T, [_|Mark], Count) :-
    winRow_(T, Mark, Count).

check_number_card(Int, [Row|_Card], CurrMarked, Marked) :-
    check_number_row(Int, Row, Number), !, 
    Marked = [Number|CurrMarked].
check_number_card(Int, [_|Card], CurrMarked, Marked) :-
    !,
    check_number_card(Int, Card, CurrMarked, Marked).

check_number_row(Int, [Curr|_Rest], Curr) :-
    (Int, _RowCord, _ColCord) = Curr, !.
check_number_row(Int, [_|Rest], Num) :-
    !, check_number_row(Int, Rest, Num).
