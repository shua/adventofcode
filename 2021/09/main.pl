:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).

line([]) --> "\n".
line([D|Ds]) --> digit(D), line(Ds).

matrix([], _) --> [].
matrix(M, Stride) --> line(L), { length(L, Stride) }, matrix(M0, Stride), { append(L, M0, M) }.

input(M, Stride) :- phrase_from_input((matrix(M, Stride), "\n")).

sample(M, Stride) :- phrase_from_file((matrix(M, Stride), "\n"), 'sample.txt').

% keep buffer 1 line back
% scan first horizontal, then vertical
%    (1,1,1) U
% R0 [2,1,3] -> (0,1,0) H
% R  [3,2,1] -> (0,0,1) H1
%    (1,1,0) D
% row 1 lowpoints -> U&H&D = (0,1,0) -> [1]
% repeat with next row, H = H1, U = not(D)
scan(M, Stride, LowPts) :-
	append(R, M1, M), length(R, Stride),
	list_of(Stride, 1, Trues),
	scan_horz(R, H),
	scan(M1, Stride, R, Trues, H, LowPts).

scan([], _, R, U, H, LowPts) :-
	and([U,H], Mask),
	select(R, Mask, LowPts).
scan(M, Stride, R, U, H, LowPts) :-
	append(R1, M1, M), length(R1, Stride),
	mask_vert(R, R1, U, H, D, LowPtsCur),
	not(D, U1),
	scan_horz(R1, H1),
	scan(M1, Stride, R1, U1, H1, LowPtsRest),
	append(LowPtsCur, LowPtsRest, LowPts).

mask_vert(R0, R, U, H, D, LowPts) :-
	lt(R0, R, D),
	and([U,H,D], Mask),
	select(R0, Mask, LowPts).

lt([], [], []).
lt([A|As], [B|Bs], [M|Mask]) :-
	A < B, M = 1, lt(As, Bs, Mask)
;	A >= B, M = 0, lt(As, Bs, Mask).

list_of(0, _, []).
list_of(N, V, [V|Vs]) :-
	N > 0, N1 is N - 1,
	list_of(N1, V, Vs).

and([], [], []).
and([A|As], [B|Bs], [M|Mask]) :-
	(	A=1, B=1, M=1
	;	A=0, B=_, M=0
	;	A=_, B=0, M=0
	),
	and(As, Bs, Mask).

and([Mask], Mask).
and([V1,V2|Vs], Mask) :-
	and(V1, V2, Cur),
	and([Cur|Vs], Mask).

not([], []).
not([1|As], [0|Mask]) :- not(As, Mask).
not([0|As], [1|Mask]) :- not(As, Mask).

scan_horz(Ds, Mask) :- scan_horz(Ds, [1], Mask).
scan_horz([_], LtPrev, LtPrev).
scan_horz([D0,D1|Ds], LtPrev, [B0|Mask]) :-
	lt([D0], [D1], LtNext),
	and(LtPrev, LtNext, [B0]),
	not(LtNext, LtPrev1),
	scan_horz([D1|Ds], LtPrev1, Mask).

select([], [], []).
select([_|Ds], [0|Mask], Out) :- select(Ds, Mask, Out).
select([D|Ds], [1|Mask], [D|Out]) :- select(Ds, Mask, Out).

answer1(N, M, Stride, LowPts) :-
	input(M, Stride),
	scan(M, Stride, LowPts),
	sum(LowPts, Sum),
	length(LowPts, LN),
	N is Sum + LN.


