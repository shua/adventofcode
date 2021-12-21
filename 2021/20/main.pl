:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(format)).

iea([1|Bs]) --> "#", iea(Bs).
iea([0|Bs]) --> ".", iea(Bs).
iea([]) --> [].

gridline(Acc, 0, Acc) --> "\n".
gridline([0|Bs], Stride, Acc) --> ".", gridline(Bs, S0, Acc), { Stride is S0 + 1 }.
gridline([1|Bs], Stride, Acc) --> "#", gridline(Bs, S0, Acc), { Stride is S0 + 1 }.

grid([], _) --> [].
grid(G, Stride) --> gridline(G, Stride, Gn), grid(Gn, Stride).

input(Alg, m(G, Stride, 0)) :- phrase_from_input((iea(Alg), "\n\n", grid(G, Stride), "\n")).

bits(Bs, I) :- var(I), bits(Bs, _, I).
bits(Bs, I) :- var(Bs), tobits(I, [], Bs).
bits([B], 1, B).
bits([B|Bs], E, I) :- bits(Bs, E0, I0), E is E0*2, I is B*E+I0.
tobits(0, Bs, Bs).
tobits(I, Bs0, Bs) :-
	I > 0,
	B is I mod 2,
	I2 is I div 2,
	tobits(I2, [B|Bs0], Bs).
truncate(Bs, N, BOut) :-
	length(Bs, Bn),
	length(BOut, N),
	(	Bn >= N, append(_, BOut, Bs)
	;	Bn < N, Pn is N - Bn, list_of(Pn, 0, Pad), append(Pad, Bs, BOut)
	).

lshft([_,_,_,B1,B2,B3,B4,B5,B6], [B7,B8,B9], [B1,B2,B3,B4,B5,B6,B7,B8,B9]).

% list with integer, vs bitree with bits
nth([Hn|_], 0, Hn).
nth([_|T], N, Hn) :- N > 0, N1 is N - 1, nth(T, N1, Hn).
nth(l(N), [], N).
nth(n(T1,T2), [], n(T1,T2)).
nth(n(_,T2), [1|Bs], N) :- nth(T2, Bs, N).
nth(n(T1,_), [0|Bs], N) :- nth(T1, Bs, N).

mval(m(Bs, S, F), R-C, F) :-
	C < 0 ; C >= S
;	R < 0 ; length(Bs, N), R*S >= N.
mval(m(Bs, S, _), R-C, B) :-
	C >= 0, C < S,
	length(Bs, N),
	R >= 0, R*S < N,
	I is R*S + C,
	nth(Bs, I, B).
msize(m(Bs, S, _), R-S) :-
	length(Bs, N),
	(	var(N), N is R*S
	;	var(R), R is N div S
	).
mfill(m(_, _, F), F).

process(m(Bs, Stride, Fill), Alg, m(BOut, St2, FillP)) :-
	% M1 fill from M fill and alg
	list_of(9, Fill, Fbs), bits(Fbs, Fn), nth(Alg, Fn, FillP),
	% priming the bits for the first row of data
	St2 is Stride + 2,
	list_of(St2, Fbs, RBuf),
	time(prows(Bs, Stride, Fill, RBuf, Bits0)),
	time(append(Bits0, Bits)),
	length(Bits, Bitn), format("~w~n", [Bitn]),
	time(mapbits(Bits, Ints)),
	time(mapalg(Alg, Ints, BOut)).

prow_(Fill, [B1,B2], [P1,P2], [N1,N2]) :-
	lshft(P1, [B1,B2,Fill], N1),
	lshft(P2, [B2,Fill,Fill], N2).
prow_(Fill, [B1,B2,B3|Bs], [Pbs|Prev], [Nbs|Next]) :-
	lshft(Pbs, [B1,B2,B3], Nbs),
	prow_(Fill, [B2,B3|Bs], Prev, Next).
prow(Fill, [B1,B2|Bs], [P1,P2|Prev], [N1,N2|Next]) :-
	lshft(P1, [Fill,Fill,B1], N1),
	lshft(P2, [Fill,B1,B2], N2),
	prow_(Fill, [B1,B2|Bs], Prev, Next).

prows([], Stride, Fill, Prev, [Bitsn1,Bitsn0]) :-
	list_of(Stride, Fill, Fbs),
	prow(Fill, Fbs, Prev, Bitsn1),
	prow(Fill, Fbs, Bitsn1, Bitsn0).
prows(Bs, Stride, Fill, Prev, [Bits|BOut]) :-
	append(Row, Rest, Bs), length(Row, Stride),
	prow(Fill, Row, Prev, Bits),
	prows(Rest, Stride, Fill, Bits, BOut).

mapbits([], []).
mapbits([BStr|BSs], [I|Is]) :-
	bits(BStr, I),
	mapbits(BSs, Is).

mapalg(_, [], []).
mapalg(Alg, [I|Is], [B|Bs]) :-
	nth(Alg, I, B),
	mapalg(Alg, Is, Bs).

count_bits(m(Bs, _, Fill), Zs, Os) :-
	Fill = 1, Os = inf, count_bits(Bs, 0, Zs)
;	Fill = 0, Zs = inf, count_bits(Bs, 1, Os).
count_bits([], _, 0).
count_bits([0|Bs], 1, N) :- count_bits(Bs, 1, N).
count_bits([1|Bs], 1, N) :- count_bits(Bs, 1, N1), N is N1 + 1.
count_bits([1|Bs], 0, N) :- count_bits(Bs, 0, N).
count_bits([0|Bs], 0, N) :- count_bits(Bs, 0, N1), N is N1 + 1.

answer1(N) :-
	input(A,M),
	process(M,A,M1),
	process(M1,A,M2),
	count_bits(M2, _, N).

bitree([N], l(N)).
bitree(L, n(T1,T2)) :-
	L = [_,_|_],
	length(L, N), Nh is N div 2,
	append(L1, L2, L), length(L1, Nh),
	bitree(L1, T1),
	bitree(L2, T2).

process2(m(Bs, Stride, Fill), Alg, m(BOut, St2, FillP)) :-
	% M1 fill from M fill and alg
	bitree(Alg, ATr),
	list_of(9, Fill, Fbs), nth(ATr, Fbs, FillP),
	% priming the bits for the first row of data
	St2 is Stride + 2,
	list_of(St2, Fbs, RBuf),
	prows(Bs, Stride, Fill, RBuf, Bits0),
	append(Bits0, Bits),
	mapalg2(ATr, Bits, BOut).

mapalg2(_, [], []).
mapalg2(Alg, [Bits|Rest], [B|Bs]) :-
	nth(Alg, Bits, B),
	mapalg2(Alg, Rest, Bs).

process2n(0, M, _, M).
process2n(N, M, A, Mn) :-
	N > 0, N1 is N - 1,
	process2(M, A, M1),
	process2n(N1, M1, A, Mn).

answer2(N) :-
	input(A, M),
	time(process2n(50, M, A, Mn)),
	count_bits(Mn, _, N).

