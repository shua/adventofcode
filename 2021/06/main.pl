:- use_module('../util.pl').
:- use_module(library(lists)).

input(Is) :- phrase_from_input((csints(Is), "\n\n")).

% can do cyclic data: young: [8,7] cur: [6,5,4,3,2,1,0]
% yi = gen mod 2, ci = gen mod 7
% cur[ci] += young[yi]
% young[yi] = cur[ci]

at(L, I, V) :-
	length(L, N), I >= 0, I < N, at_(L, I, V).
at_([V|_], 0, V).
at_([_|L], I, V) :- I > 0, II is I-1, at_(L, II, V).

eq_except(L1, L2, I, V1, V2) :-
	append(Pre, [V1|Post], L1),
	append(Pre, [V2|Post], L2),
	length(Pre, I).

sim(step(I), (Young, Wheel, I), (Young, Wheel, I)).
sim(step(In), (Young, Wheel, I), (Yn, Wn, In)) :-
	I < In, II is I + 1,
	Yi is I mod 2, Ci is I mod 7,
	eq_except(Wp, Wheel, Ci, Wgraduated, Wval),
	eq_except(Yp, Young, Yi, Wval, Yval),
	Wgraduated is Yval + Wval,
	sim(step(In), (Yp, Wp, II), (Yn, Wn, In)).

sim(init, [], W, W).
sim(init, [I|Is], Acc, Out) :-
	eq_except(Acc, Cur, I, V0, V),
	V is V0 + 1,
	sim(init, Is, Cur, Out).
sim(init(Is), ([0,0], Wheel, 0)) :-
	sim(init, Is, [0,0,0,0,0,0,0], Wheel).

answer1(N) :- answer1(N, _, _).
answer1(N, Is, S0) :-
	input(Is),
	sim(init(Is), S0),
	sim(step(80), S0, SN),
	SN = ([Y0, Y1], [W0, W1, W2, W3, W4, W5, W6], _),
	N is Y0 + Y1 + W0 + W1 + W2 + W3 + W4 + W5 + W6.

answer2(N) :-
	input(Is),
	sim(init(Is), S0),
	sim(step(256), S0, SN),
	SN = ([Y0, Y1], [W0, W1, W2, W3, W4, W5, W6], _),
	N is Y0 + Y1 + W0 + W1 + W2 + W3 + W4 + W5 + W6.

