:- use_module('../util.pl').
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

line([]) --> [].
line([C|Cs]) --> [C], { member(C, "()[]{}<>") }, line(Cs).
lines([]) --> [].
lines([L-V|Ls]) -->  line(L), "\n", { validate(L, [], V) }, lines(Ls).

input(Ls) :- phrase_from_input((lines(Ls), "\n")).

sample(Ls) :- phrase_from_file((lines(Ls), "\n"), 'sample.txt').

validate([], Exp, incomplete(Exp)) :- Exp = [_|_].
validate([], [], valid).
validate([C|Cs], [C|Expecting], Result) :-
	validate(Cs, Expecting, Result).
validate([C|Cs], Expecting, Result) :-
	(	C = '[', E = ']'
	;	C = '(', E = ')'
	;	[C] = "<", [E] = ">"
	;	C = '{', E = '}'
	),
	validate(Cs, [E|Expecting], Result)
	% for some reason, '>' throws a prolog parsing error in scryer?
;	( C = ']' ; C = ')' ; [C] = ">" ; C = '}' ),
	Expecting \= [C|_],
	Result = corrupt(C).

score1([], 0).
score1([_-valid|KVs], Score) :- score1(KVs, Score).
score1([_-incomplete(_)|KVs], Score) :- score1(KVs, Score).
score1([_-corrupt(C)|KVs], Score) :-
	(	C = ')', V = 3
	;	C = ']', V = 57
	;	C = '}', V = 1197
	;	[C] = ">", V = 25137
	),
	score1(KVs, Score0),
	Score is Score0 + V.

answer1(N) :-
	input(Ls),
	score1(Ls, N).

score2([], []).
score2([_-corrupt(_)|KVs], Scores) :- score2(KVs, Scores).
score2([_-valid|KVs], Scores) :- score2(KVs, Scores).
score2([_-incomplete(Exp)|KVs], [S|Scores]) :-
	score2(Exp, 0, S),
	score2(KVs, Scores).
score2([], Score, Score).
score2([C|Cs], Cur, Score) :-
	(	C = ')', V = 1
	;	C = ']', V = 2
	;	C = '}', V = 3
	;	[C] = ">", V = 4
	),
	Next is Cur * 5 + V,
	score2(Cs, Next, Score).

answer2(N) :-
	input(Ls),
	score2(Ls, Scores),
	msort(Scores, Sorted),
	length(Sorted, Sn),
	HSn is Sn div 2,
	append(Pre, [N|_], Sorted),
	length(Pre, HSn).

