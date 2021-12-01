:- use_module(library(dcgs)).
:- use_module(library(pio)).

seq([]) --> [].
seq([C | Cs ]) --> [C], seq(Cs).

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

uint(I, 1) --> digit(I).
uint(I, E) --> digit(Hd), uint(Tl, E0), { E is E0 * 10, I is E * Hd + Tl }.
uint(I) --> uint(I, _).

lines([]) --> "\n".
lines([ N | Ns ]) --> uint(N), "\n", lines(Ns).

count_increase([], 0).
count_increase([_], 0).
count_increase([ A, B | T ], N) :- A >= B, count_increase([B|T], N).
count_increase([ A, B | T ], N) :- A < B, count_increase([B|T], M), N is M + 1.

answer1(N) :- phrase_from_file(lines(Nums), 'input.txt'), count_increase(Nums, N).

sum_window([], []).
sum_window([_], []).
sum_window([_,_], []).
sum_window([A,B,C | T], [N | Ns]) :- sum_window([B,C | T], Ns), N is A + B + C.

answer2(N) :- phrase_from_file(lines(Nums), 'input.txt'), sum_window(Nums, Sums), count_increase(Sums, N).
