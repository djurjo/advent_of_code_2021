:- module(puzzle1, [main_1/1, main_2/1], _).

:- use_module('/Users/danieljurjo/jurjo/advent/advent_of_code_2021/utils/parse_file.pl', [extract_data/3]).

:- use_module(library(stream_utils), [
    file_to_string/2
]).
%%%%% Part 1 %%%%
main_1(Output) :-
    p(Inputs),
    get_increases(Inputs, 0, Output, 0.Inf).

p(Out) :-
    file_to_string('input1.txt', Str),
    extract_data(Str, none, Out).

get_increases([], NumberIncreases, NumberIncreases, _).
get_increases([In|Inputs], NumberIncreasesC, NumberIncreases, PrevValue) :-
    In > PrevValue,
    !,
    NumberIncreasesT is NumberIncreasesC + 1,
    get_increases(Inputs, NumberIncreasesT, NumberIncreases, In).
get_increases([In|Inputs], NumberIncreasesC, NumberIncreases,_PrevValue) :-
    get_increases(Inputs, NumberIncreasesC, NumberIncreases, In).

%%%% Part 2 %%%%%
main_2(Output) :-
    p(Inputs),
    get_window_inc(Inputs, 0,0,0,0, Output ).

get_window_inc([In|Inputs], 0, 0, 0, COut, Out) :- %Step1
    get_window_inc(Inputs, In, 0, 0, COut, Out).
get_window_inc([In|Inputs], WinA, 0, 0, COut, Out) :- %Step2
    WinNA is WinA + In,
    get_window_inc(Inputs, WinNA, In, 0, COut, Out).
get_window_inc([In|Inputs], WinA, WinB, 0, COut,  Out) :- %Step3
    WinNA is WinA + In,
    WinNB is WinB + In, 
    get_window_inc(Inputs, WinNA, WinNB, In, COut, Out).
get_window_inc([In|Inputs], WinA, WinB, WinC, COut, Out) :-
    WinNA is WinB + In,
    WinNA > WinA, !,
    NCout is COut + 1,
    WinNB is WinC + In,
    WinNC = In,
    get_window_inc(Inputs, WinNA, WinNB, WinNC, NCout, Out).
get_window_inc([In|Inputs], _WinA, WinB, WinC, COut, Out) :-
    !,
    WinNA is WinB + In,
    WinNB is WinC + In,
    WinNC = In,
    get_window_inc(Inputs, WinNA, WinNB, WinNC, COut, Out).
get_window_inc([], _WinA, _WinB, _WinC, Out, Out).
