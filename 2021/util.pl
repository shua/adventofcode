:- module(util, [
	seq//1, digit//1, uint//1, ws//1, csints//1,
	phrase_from_input/1,
	sum/2, msort/2, clump/2, counts/2, eq_except/4,
	list_of/3, zip/3]).

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

uint(I, E) --> digit(Hd), uint(Tl, E0), { E is E0 * 10, I is E * Hd + Tl }.
uint(I, 1) --> digit(I).
uint(I) --> uint(I, _).

ws([]) --> [].
ws([C|Cs]) --> [C], { C = ' ' ; C = '\n' ; C = '\t' }, ws(Cs).

csints([I|Is]) --> uint(I), ",", csints(Is).
csints([I]) --> uint(I).

phrase_from_input(GR) :- phrase_from_file(GR, 'input.txt').

sum([], 0).
sum([N|Ns], Sum) :-
	sum(Ns, Sum0),
	Sum is N + Sum0.

msort(key, [], []).
msort(key, [V|Vs], [V-1|KVs]) :- msort(key, Vs, KVs).
msort(Vs, Sorted) :-
	msort(key, Vs, KVs),
	keysort(KVs, KVSorted),
	msort(key, Sorted, KVSorted).

clump(KVs, Clumped) :-
	keysort(KVs, KVSorted),
	clump_(KVSorted, Clumped).
clump_([], []).
clump_([K-V], [K-V]).
clump_([K1-V1,K1-V2|KVs], Out) :-
	V is V1 + V2,
	clump_([K1-V|KVs], Out).
clump_([K1-V1,K2-V2|KVs], [K1-V1|Out]) :-
	K1 @< K2, clump_([K2-V2|KVs], Out).

counts(Vs, Counts) :-
	msort(key, Vs, KVs),
	clump(KVs, Counts).

eq_except([], [], _, _).
eq_except([V1|L1], [V1|L2], V3, V4) :- V1 \== V3, eq_except(L1, L2, V3, V4).
eq_except([V1|L1], [V2|L1], V1, V2).

list_of(0, _, []).
list_of(N, V, [V|Vs]) :- N > 0, N1 is N - 1, list_of(N1, V, Vs).

zip([], [], []).
zip([K|Ks], [V|Vs], [K-V|KVs]) :- zip(Ks, Vs, KVs).

