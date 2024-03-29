:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(time)).
:- use_module(library(format)).

line(D, D, 0) --> "\n".
line(D, [I|Is], N) --> digit(I), line(D, Is, Nn), { N is Nn + 1 }.
matrix(Stride, Data, m(Stride, Data)) --> "\n".
matrix(Stride, Data, M) -->
	line(Data, DataCur, Stride),
	matrix(Stride, DataCur, M).
matrix(M) -->
	line([], L, Stride),
	matrix(Stride, L, M).

input(M) :- phrase_from_input(matrix(M)).

mget(M, R-C, V) :- mget(M, R, C, V).
mget(m(S, Data), Row, Col, V) :-
	Col < S,
	I is Row * S + Col,
	length(Pre, I),
	append(Pre, [V|_], Data).

% https://en.wikipedia.org/wiki/A*_search_algorithm
% length(Data, Dn), Inf = Dn * 9 + 1,
astar(Self, Start, Goal, Path) :-
	Open = [Start],
	From = [],
	Gs = [Start-0],
	astar_h(Self, Start, HStart),
	Fs = [Start-HStart],
	astar_iter(Self, Goal, Open, From, Gs, Fs, Path).

astar_iter(Self, Goal, [], From, Gs, Fs, error(Self, Open, From, Gs, Fs)).
astar_iter(Self, Goal, Open, From, Gs, Fs, Path) :-
	Open = [_|_], popminfscore(Open, Fs, Open0, Cur),
	(	Cur == Goal, make_path(From, Cur, Path)
	;	Cur =\= Goal,
		astar_neigh(Self, Cur, CurNs),
		astar_gscores(Self, Cur, Gs, CurNs, GsNew),
		astar_update(Self, Cur, Open, From, Gs, Fs, GsNew, Open1, From1, Gs1, Fs1)
	).
astar_iter(Self, Goal, Open, From, Gs, Fs, error(Self, Open, From, Gs, Fs)).

memopt(K, [], none).
memopt(K, [K-V|_], some(V)).
memopt(K, [K2-_|KVs], Is) :- (K @> K2 ; K @< K2), memopt(K, KVs, Is).

optmin(none, none, none).
optmin(some(V), none, some(V)).
optmin(none, some(V), some(V)).
optmin(some(V1), some(V2), some(V3)) :- (V1 < V2, V3 = V1 ; V2 =< V1, V3 = V3).

popminfscore(Open, Fs, Open0, Cur) :-
	findminfscore(Open, Fs, Cur),
	append(Pre, [Cur|Post], Open),
	append(Pre, Post, Open0).
findminfscore([K], _, K).
findminfscore([K|Ks], Fs, Cur) :-
	findminfscore(Ks, Fs, Min),
	memopt(K, Fs, Vo), memopt(Min, Fs, VMin),
	optmin(Vo, VMin, V1),
	(	V1 = Vo, Cur = K
	;	V1 = VMin, Cur = Min
	).

	

astar_neigh(a(_, Rn-Cn), R-C, CurNs) :-
	C1 is C + 1, R1 is R + 1,
	( R > 0, Ru is R - 1, N0 = [Ru-C] ; R = 0, N0 = [] ),
	( C > 0, Cl is C - 1, N1 = [R-Cl|N0] ; C = 0, N1 = N0 ),
	( C1 < Cn, Cr is C + 1, N2 = [R-Cr|N1] ; C1 = Cn, N2 = N1 ),
	( R1 < Rn, Rd is R + 1, N3 = [Rd-C|N2] ; R1 = Rn, N3 = N2 ),
	!,
	CurNs = N3.

astar_gscores(_, _, _, [], []).
astar_gscores(Self, Cur, Gs, [N|CurNs], [N-Ng|GsNew]) :-
	member(Cur-Cg, Gs),
	astar_d(Self, Cur, N, Dcn),
	Ng is Cg + Dcn,
	astar_gscores(Self, Cur, Gs, CurNs, GsNew).

astar_d(a(M, _), RC1, RC2, D) :-
	taxi_dist(RC1, RC2, 1), % assert that this is a neighbor
	mget(M, RC2, D).

taxi_dist(R1-C1, R2-C2, D) :-
	( C1 >= C2, Cd is C1 - C2 ; C1 < C2, Cd is C2 - C1 ),
	( R1 >= R2, Rd is R1 - R2 ; R1 < R2, Rd is R2 - R1 ),
	D is Rd + Cd.

/*
if tentative_gScore < gScore[neighbor]
	// This path to neighbor is better than any previous one. Record it!
	cameFrom[neighbor] := current
	gScore[neighbor] := tentative_gScore
	fScore[neighbor] := tentative_gScore + h(neighbor)
	if neighbor not in openSet
		openSet.add(neighbor)
 */

astar_update(Self, Cur, Open, From, Gs, Fs, [], Open, From, Gs, Fs).
astar_update(Self, Cur, Open, From, Gs, Fs, [G1|GsNew], Open1, From1, Gs1, Fs1) :-
	update_min(Gs, G1, Gs0, Mut),
	(	Mut = 0,
		astar_update(Self, Cur, Open, From, Gs, Fs, GsNew, Open1, From1, Gs1, Fs1)
	;	Mut = 1,
		G1 = N-Ng,
		update(From, N, Cur, From0),
		astar_h(Self, N, Nh), Nf is Ng + Nh,
		update(Fs, N, Nf, Fs0),
		sort([N|Open], Open0),
		astar_update(Self, Cur, Open0, From0, Gs0, Fs0, GsNew, Open1, From1, Gs1, Fs1)
	).

update(In, K, V, Out) :-
	append(Pre, [K-_|Post], In)
->	append(Pre, [K-V|Post], Out)
;	sort([K-V|In], Out).

update_min(In, K-V, Out, Mut) :-
	append(Pre, [K-V1|Post], In)
->	( V < V1, append(Pre, [K-V|Post], Out), Mut = 1 ; V >= V1, Out = In, Mut = 0 )
;	sort([K-V|In], Out).

% assume every point in between is middle 5
astar_h(a(_, Rn-Cn), R-C, H) :-
	taxi_dist(R-C, Rn-Cn, H1),
	H is H1 * 5.

