:- use_module('../util.pl').
:- use_module(library(dcgs)).
:- use_module(library(lists)).

sint(I) --> uint(I).
sint(In) --> "-", uint(I), { In is -I }.
scanner(ID, Pts) --> "--- scanner ", uint(ID), " ---\n", points(Pts).
points([[X,Y,Z]|Pts]) --> sint(X), ",", sint(Y), ",", sint(Z), "\n", points(Pts).
points([]) --> [].

scanners([ID-Pts|Scnrs]) --> scanner(ID, Pts), "\n", scanners(Scnrs).
scanners([]) --> [].

input(Ss) :- phrase_from_input(scanners(Ss)).

pi(1, [[X,_,_]|Pts], [X|Xs]) :- pi(1, Pts, Xs).
pi(2, [[_,Y,_]|Pts], [Y|Ys]) :- pi(2, Pts, Ys).
pi(3, [[_,_,Z]|Pts], [Z|Zs]) :- pi(3, Pts, Zs).
pi(_, [], []).

min_max([E], E, E).
min_max(Ls, Min, Max) :-
	Ls = [_,_|_],
	sort(Ls, Lsrt),
	append([Min|_], [Max], Lsrt).

bounds(Pts, [Dx,Dy,Dz]) :-
	pi(1, Pts, Xs), pi(2, Pts, Ys), pi(3, Pts, Zs),
	min_max(Xs, Xm, XM), min_max(Ys, Ym, YM), min_max(Zs, Zm, ZM),
	(	XM >= 0, XM1 = XM
	;	XM < 0, XM1 is -1 * XM ),
	Dx is XM - Xm, Dy is YM - Ym, Dz is ZM - Zm.

sbounds([], []).
sbounds([K-V|KVs], [K-B|KBs]) :-
	bounds(V, B),
	sbounds(KVs, KBs).

