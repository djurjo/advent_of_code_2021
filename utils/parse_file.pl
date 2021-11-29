:- module(read_file, [extract_data/3], _).

%% :- use_module(library(stream_utils), [
%%     file_to_string/2
%% ]).


:- use_module(library(write)).
:- use_module(library(streams)).

extract_data(Str, Sep, Out) :-
    get_numbers(Str, Out,  [], Sep).

get_numbers([], Out, Out, _).
get_numbers(Str, OutC, Out, Sep) :-
    get_row(Str, Row, [], Rest),
    %write(row(Row)), nl,
    get_numbers_row(Row, Value, [], Sep),
    %write(valuue(Value)), nl,
    OutC = [Value|OutT],
    get_numbers(Rest, OutT, Out, Sep).

get_numbers_row(Row, [Out],  _, none) :-
    number_codes(Out, Row).
get_numbers_row([], OutR, OutR, _).
get_numbers_row(Row, Out, Out, Sep) :-
    separate_row(Row,  [], [], _RRow,Sep),
    !.
get_numbers_row(Row, OutC, OutR, Sep) :-
    separate_row(Row,  Num, [], RRow,Sep),
    %write(num(Num)), nl, 
    number_codes(Value, Num),
    %write(val(Value)), nl, 
    OutC = [Value|OutT],
    get_numbers_row(RRow, OutT, OutR, Sep).


separate_row([], Vals, Vals, [], _Sep).
separate_row([Cod|Row], Vals, Vals, Row, Sep) :-
    Cod = Sep, !.
separate_row([Cod|Row], ValsC, Vals, RestRow, Sep) :-
    !,
    %write(cod(Cod)), nl, 
    ValsC = [Cod|ValsT],
    separate_row(Row, ValsT, Vals, RestRow, Sep).

get_row([], Row, Row, []). 
get_row([Cod|Str],Row, Row, Str) :-
    Cod = 10, !.
get_row([Cod|Str], RowC, Row, Stream) :-
    !,
    RowC =  [Cod|RowT],
    get_row(Str, RowT, Row, Stream).

