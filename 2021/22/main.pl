:- use_module('../util.pl').
:- use_module(library(format)).
:- use_module(library(lists)).

onoff(1) --> "on".
onoff(0) --> "off".

int(I) --> "-", uint(In), { I is -In }.
int(I) --> uint(I).

bound([Lo,Hi]) --> int(Lo), "..", int(Hi).

line(IO, [X,Y,Z]) --> onoff(IO), " x=", bound(X), ",y=", bound(Y), ",z=", bound(Z). 

lines([XYZ-IO|Ls]) --> line(IO, XYZ), "\n", lines(Ls).
lines([]) --> [].

input(Ls) :- phrase_from_input((lines(Ls), "\n")).

xyz_lt([X,_,_], [X1,_,_]) :- X < X1.
xyz_lt([X,Y,_], [X,Y1,_]) :- Y < Y1.
xyz_lt([X,Y,Z], [X,Y,Z1]) :- Z < Z1.

set([Pt-_|Pts], Pt-1, [Pt-1|Pts]).
set([Pt-_|Pts], Pt-0, Pts).
set([Pt0-V0|Pts0], Pt-V, [Pt0-V0|Pts]) :- xyz_lt(Pt0, Pt), set(Pts0, Pt-V, Pts).
set([Pt0-V0|Pts], Pt-1, [Pt-1,Pt0-V0|Pts]) :- xyz_lt(Pt, Pt0).
set([Pt0-V0|Pts], Pt-0, [Pt0-V0|Pts]) :- xyz_lt(Pt, Pt0).
set([], Pt-1, [Pt-1]).
set([], _-0, []).

set1(Pts, Pt-1, PtsOut) :-
	append(_, [Pt-1|_], Pts)
->	PtsOut = Pts
;	PtsOut = [Pt-1|Pts].
set1(Pts, Pt-0, PtsOut) :-
	append(Pre, [Pt-1|Post], Pts)
-> append(Pre, Post, PtsOut)
;	PtsOut = Pts.


range_set(Pts, [[XM1,XM],_,_]-_, Pts) :- XM1 > XM.
range_set(Pts, [[Xm,XM],[Ym,YM],[Zm,ZM]]-IO, PtsOut) :-
	Xm =< XM,
	format("range_set [~w,~w,~w]~n", [Xm, [Ym,YM], [Zm,ZM]]),
	range_set1(Pts, Xm, [Ym,YM], [Zm,ZM], IO, PtsCur),
	X1 is Xm + 1,
	range_set(PtsCur, [[X1,XM],[Ym,YM],[Zm,ZM]]-IO, PtsOut).
range_set1(Pts, _, [YM1,YM], _, _, Pts) :- YM1 > YM.
range_set1(Pts, X, [Ym,YM], [Zm,ZM], IO, PtsOut) :-
	integer(X), Ym =< YM,
	format("range_set [~w,~w,~w]~n", [X, Ym, [Zm,ZM]]),
	range_set2(Pts, X, Ym, [Zm,ZM], IO, PtsCur),
	Y1 is Ym + 1,
	range_set1(PtsCur, X, [Y1,YM], [Zm,ZM], IO, PtsOut).
range_set2(Pts, _, _, [ZM1,ZM], _, Pts) :- ZM1 > ZM.
range_set2(Pts, X, Y, [Zm,ZM], IO, PtsOut) :-
	integer(X), integer(Y), Zm =< ZM,
	set1(Pts, [X,Y,Zm]-IO, PtsCur),
	Z1 is Zm + 1,
	range_set2(PtsCur, X, Y, [Z1,ZM], IO, PtsOut).

% too slow, both set and set1
% can we store intervals as a tree?
% ^  ##
% | ###  2..3,2..3  t(2..3,n(2..3))
% | ##   3..4,3..4  t(2..2,n(2..3)),t(3..3,n(2..4)),t(4..4,n(3..4))
% |   #  4..4,1..1  t(2..2,n(2..3)),t(3..3,n(2..4)),t(4..4,n(1..1),n(3..4))
% 0--->

%  []           range contains
%      []       range does not contain
%  [...]        range intersects 1 min
%      [...]    range intersects 1 max
%   [......]    range intersects 2
% [..]    [..]
in_range(N, Min, Max) :- N >= Min, N =< Max.
nin_range(N, Min, Max) :- N < Min ; N > Max.

compare(A, B, lt) :- A < B.
compare(A, B, gt) :- compare(B, A, lt).
compare(A, A, eq).
compare(A, B, le) :- compare(A, B, lt) ; compare(A, B, le).
compare(A, B, ge) :- compare(A, B, gt) ; compare(A, B, le).

bounds_tree([], []).
bounds_tree([B0|Bs], [B0-T0]) :-
	bounds_tree(Bs, T0).

merge(T0, T1, Tn) :-
	format("merge(~w, ~w, ?)~n", [T0, T1]),
	merge_(T0, T1, To),
	format("norm(~w, ?)~n", [To]),
	norm(To, Tn).

merge_([], T2, T2).
merge_(T1, [], T1) :- T1 = [_|_].
merge_([[L0,R0]-S0|T0], [[L1,R1]-S1|T1], Tn) :-
	R1 < L0,
	% advance right
	merge([[L0,R0]-S0|T0], T1, T2),
	Tn = [[L1,R1]-S1|T2]
;	R0 < L1,
	% advance left
	merge(T0, [[L1,R1]-S1|T1], T2),
	Tn = [[L0,R0]-S0|T2]
;	R0 = R1, L1 = L0,
	% subtree merge, advance both
	merge(T0, T1, T2),
	merge(S0, S1, S2),
	Tn = [[L0,R0]-S2|T2]
;	R1 >= L0, R0 >= L1,
	% split and retry
	(L1 > L0 ; L1 < L0 ; L1=L0, R1 > R0 ; L1=L0, R1 < R0),
	split([L0,R0], [L1,R1], S0, S1, T0, T1, T0b, T1b),
	merge(T0b, T1b, Tn).

split(B0, B1, S0, S1, T0, T1, T0o, T1o) :-
	format("split(~w, ~w, ?)~n", [B0, B1]),
	split_(B0, B1, S0, S1, T0, T1, T0o, T1o).
split_(B, B, S0, S1, T0, T1, [B-S0|T0], [B-S1|T1]).
split_(
	[L0,R0], [L0,R1], S0, S1, T0, T1,
	[[L0,R0]-S0|T0], [[L0,R0]-S1,[R0p,R1]-S1|T1]
) :- R0 < R1, R0p is R0 + 1.
split_(
	[L0,R0], [L0,R1], S0, S1, T0, T1,
	[[L0,R1]-S0,[R1p,R0]-S0|T0], [[L0,R0]-S1|T1]
) :- R0 > R1, R1p is R1 + 1.
split_(
	[L0,R0], [L1,R1], S0, S1, T0, T1,
	[[L0,L1m]-S0|T0b], T1b
) :-
	L0 < L1, L1m is L1 - 1,
	split([L1,R0], [L1,R1], S0, S1, T0, T1, T0b, T1b).
split_(
	[L0,R0], [L1,R1], S0, S1, T0, T1,
	T0b, [[L1,L0m]-S1|T1b]
) :-
	L0 > L1, L0m is L0 - 1,
	split([L0,R0], [L0,R1], S0, S1, T0, T1, T0b, T1b).

insert([], [R|Rngs], [R-TOut1]) :- insert2([], Rngs, TOut1).
insert(T, R, To) :-
	T = [Rt|_],
	format("~w <<- ~w~n", [Rt, R]),
	(	insert_(T, R, To),
		format("~w <<- ~w == ~w~n", [Rt, R, To])
	;	format("~w <<- ~w FAIL~n", [Rt, R]),
		fail
	).
insert_([[Min0,Max0]-Children|T], [[Min1,Max1]|Rngs], TOut) :-
	Max0 < Min1,
	insert(T, [[Min1,Max1]|Rngs], TOut1),
	TOut = [[Min0,Max0]-Children|TOut1]
;	Max1 < Min0,
	insert([], Rngs, [TOut1]),
	TOut = [TOut1, [Min0,Max0]-Children | T]

;	Max0 >= Min1, Max1 >= Min0,
	(	Min1 < Min0,
		MinL is Min0 - 1,
		insert([[Min1,MinL]-Children], [[Min1,MinL]|Rngs], TOut1),
		insert([[Min0,Max0]-Children|T], [[Min0,Max1]|Rngs], TOut2),
		TOut = [[Min1,MinL]-TOut1 | TOut2]
	;	Min1 > Min0,
		insert([[Min1,Max0]-Children|T], [[Min1,Max1]|Rngs], TOut1),
		MinL is Min1 - 1,
		TOut = [[Min0,MinL]-Children|TOut1]

	;	Min1 = Min0,
		(	Max0 < Max1,
			MaxR is Max0 + 1,
			insert([[Min0,Max0]-Children], [[Min0,Max0]|Rngs], [TOut1]),
			insert([TOut1|T], [[MaxR,Max1]|Rngs], TOut)
		;	Max0 > Max1,
			MaxR is Max1 + 1,
			insert([[Min0,Max1]-Children], [[Min0,Max1]|Rngs], TOut1),
			TOut = [TOut1, [MaxR,Max0]-Children | T],
			format("Max0 > Max1 SUCCESS~n", [])
		;	Max0 = Max1,
			insert2(Children, Rngs, TOut1),
			TOut = [[Min0,Max0]-TOut1|T]
		)
	).

insert2(T, [], T).
insert2(T, R, TO) :- insert(T, R, T0), norm(T0, TO).

norm([N], [N]).
norm([], []).
norm([[Min0,Max0]-S0, [Min1,Max1]-S1 | T], TOut) :-
	Min1 =:= Max0+1, S0 = S1
->	norm([[Min0,Max1]-S0|T], TOut)
;	norm([[Min1,Max1]-S1|T], TOut1),
	TOut = [[Min0,Max0]-S0|TOut1].


remove([], _, []).
remove([[Min0,Max0]-Children|T], [[Min1,Max1]|Rngs], TOut) :-
	Max0 < Min1,
	remove(T, [[Min1,Max1]|Rngs], TOut1),
	TOut = [[Min0,Max0]-Children|TOut1]
;	Max1 < Min0,
	TOut = [[Min0,Max0]-Children|T]

;	Max0 >= Min1, Max1 >= Min0,
	(	Min1 < Min0,
		remove([[Min0,Max0]-Children|T], [[Min0,Max1]|Rngs], TOut)
	;	Min1 > Min0,
		remove([[Min1,Max0]-Children|T], [[Min1,Max1]|Rngs], TOut1),
		MinL is Min1 - 1,
		TOut = [[Min0,MinL]-Children|TOut1]

	;	Min1 = Min0,
		(	Max0 < Max1,
			MaxR is Max0 + 1,
			remove(T, [[MaxR,Max1]|Rngs], TOut2),
			remove2([Min0,Max0]-Children, Rngs, TOut2, TOut)
		;	Max0 > Max1,
			MaxR is Max1 + 1,
			TOut2 = [[MaxR,Max0]-Children|T],
			remove2([Min0,Max1]-Children, Rngs, TOut2, TOut)
		;	Max0 = Max1,
			remove2([Min0,Max0]-Children, Rngs, T, TOut)
		)
	).

remove2(_, [], Tail, Tail).
remove2(R-Children, [Rng|Rngs], Tail, TOut) :-
	remove(Children, [Rng|Rngs], Out0),
	(	Out0 = [_|_],
		TOut = [R-Out0|Tail]
	;	Out0 = [],
		TOut = Tail
	).

process([], T, T).
process([Rng-1|IOs], In, Out) :-
	insert(In, Rng, Cur),
	process(IOs, Cur, Out).
process([Rng-0|IOs], In, Out) :-
	remove(In, Rng, Cur),
	process(IOs, Cur, Out).
processn(IOs, N, Out) :-
	append(IOn, _, IOs), length(IOn, N),
	process(IOn, [], Out).

answer1(_) :- fail.

answer2(_) :- fail.

