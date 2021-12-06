:- module(util, [
	seq//1, digit//1, uint//1, ws//1, csints//1,
	phrase_from_input/1]).

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

ws([]) --> [].
ws([C|Cs]) --> C, ( C = " " ; C = "\n" ; C = "\t" ), ws(Cs).

csints([I|Is]) --> uint(I), ",", csints(Is).
csints([I]) --> uint(I).

phrase_from_input(GR) :- phrase_from_file(GR, 'input.txt').

