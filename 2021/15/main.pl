:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).

mat_row([], 0) --> "\n".
mat_row([D|Ds], N) --> digit(D), mat_row(Ds, N0), { N is N0 + 1 }.
mat(m(N, D)) --> mat_row(D0, N), mat(m(N, Dn)), { append(D0, Dn, D) }.
mat(m(N, D)) --> mat_row(D, N).

input(M) :- phrase_from_file((mat(M), "\n"), 'input.txt').

sample(M) :- append([
	"1163751742\n",
	"1381373672\n",
	"2136511328\n",
	"3694931569\n",
	"7463417111\n",
	"1319128137\n",
	"1359912421\n",
	"3125421639\n",
	"1293138521\n",
	"2311944581\n"
], Src), phrase(mat(M), Src).

mat_get(m(Cs, D), [R,C], V) :-
	I is R * Cs + C,
	append(Pre, [V|_], D),
	length(Pre, I).
mat_eq_except(m(Cs, A), m(Cs, B), [R,C], V) :-
	I is R * Cs + C,
	append(Pre, [_|Post], A),
	length(Pre, I),
	append(Pre, [V|Post], B).
mat_verts(m(Cs, D), Verts) :-
	length(D, N), Rs is N div Cs,
	mat_verts(Rs, Cs, 0, 0, Verts).
mat_verts(Rs, _, Rs, 0, []).
mat_verts(Rs, Cs, R, Cs, Verts) :-
	R < Rs, R1 is R + 1,
	mat_verts(Rs, Cs, R1, 0, Verts).
mat_verts(Rs, Cs, R, C, [[R,C]|Verts]) :-
	R < Rs, C < Cs, C1 is C + 1,
	mat_verts(Rs, Cs, R, C1, Verts).

lt(A, B) :- integer(A), integer(B), A < B.
lt(A, infinity) :- integer(A).

djikstra(M, Src, Dst, Path) :-
	format("init~n", []),
	mat_verts(M, Unvisited),
	djikstra_init(M, Dist0, Prev),
	mat_eq_except(Dist0, Dist, Src, 0),
	format("start~n", []),
	remove_min(Unvisited, Dist, Q, U),
	djikstra_(M, Dst, U, Q-_, Dist-_, Prev-Prevp),
	reconstruct_path(Prevp, Src, Dst, Path).
djikstra_init(m(Cs, M), m(Cs, Dist), m(Cs, Prev)) :-
	length(M, N),
	list_of(N, infinity, Dist),
	list_of(N, undefined, Prev).
djikstra_(_, _, _, []-[], Dist-Dist, Prev-Prev).
% can stop early if U = Dst
djikstra_(M, Dst, Dst-_, Q-Q, Dist-Dist, Prev-Prev) :-
	show_state(M, Q).
djikstra_(M, Dst, U-DistU, Q-Qp, Dist-Distp, Prev-Prevp) :-
	format("choose ~w ~w~n", [U, DistU]),
	( random_integer(0, 10, X), X = 2 -> show_state(M, Q) ; true ),
	neighbors(U, UNeigbors),
	set_isect(UNeigbors, Q, QNeighbors),
	update_neighbors(QNeighbors, M, U-DistU, Dist-Distc, Prev-Prevc),
	remove_min(Q, Distc, Qc, NextU),
	djikstra_(M, Dst, NextU, Qc-Qp, Distc-Distp, Prevc-Prevp).

mat_val_key(_, [], []).
mat_val_key(Vs, [K|Ks], [V-K|VKs]) :-
	mat_get(Vs, K, V),
	mat_val_key(Vs, Ks, VKs).
remove_min(Ks, Vs, Kp, K-V) :-
	Ks = [_|_],
	mat_val_key(Vs, Ks, VKs),
	msort(VKs, [V-K|_]),
	append(Pre, [K|Post], Ks),
	append(Pre, Post, Kp).

neighbors([R,C], Ns) :-
	Lt is C - 1, Rt is C + 1,
	Up is R - 1, Dn is R + 1,
	Ns = [        [Up,C],
	       [R,Lt],       [R,Rt],
	              [Dn,C]        ].

set_isect(A, B, C) :-
	length(A, An), length(B, Bn),
	(	(An < Bn ; An = Bn),
		set_isect_(A, B, C)
	;	Bn < An,
		set_isect_(B, A, C)
	).
set_isect_([], _, []).
set_isect_([A|As], Bs, Cs) :-
	memberchk(A, Bs) ->
	set_isect_(As, Bs, Ds),
	Cs = [A|Ds]
;	set_isect_(As, Bs, Cs).

update_neighbors([], _, _, Dist-Dist, Prev-Prev).
update_neighbors([N|Ns], M, U-DistU, Dist-Distp, Prev-Prevp) :-
	mat_get(M, N, DistUN),
	Alt is DistU + DistUN,
	mat_get(Dist, N, DistN),
	(	lt(Alt, DistN),
		mat_eq_except(Dist, Distc, N, Alt),
		mat_eq_except(Prev, Prevc, N, U)
	;	( lt(DistN, Alt) ; Alt = DistN ),
		Distc = Dist, Prevc = Prev
	),
	update_neighbors(Ns, M, U-DistU, Distc-Distp, Prevc-Prevp).

reconstruct_path(Prev, Src, Dst, Path) :-
	reconstruct_path(Prev, Src, Dst, [Dst], Path).
reconstruct_path(_, Src, Src, Path, Path).
reconstruct_path(Prev, Src, Cur, Path, Pathp) :-
	Cur \== Src,
	mat_get(Prev, Cur, Next),
	reconstruct_path(Prev, Src, Next, [Next|Path], Pathp).

show_path(m(Cs, D), Path) :-
	show_path(D, Cs, [0,0], Path).
show_path([], _, _, _).
show_path(D, Cs, [R,Cs], Path) :-
	R1 is R + 1,
	format("~n", []),
	show_path(D, Cs, [R1,0], Path).
show_path([D|Ds], Cs, [R,C], Path) :-
	C < Cs,
	(	member([R,C], Path) ->
		format("\x1b\[31m~d\x1b\[0m", [D])
	;	format("~d", [D])
	),
	C1 is C + 1,
	show_path(Ds, Cs, [R,C1], Path).

show_state(m(Cs, D), Unvisited) :-
	show_state(D, Cs, [0,0], Unvisited).
show_state([], _, _, _) :- format("~n", []).
show_state(D, Cs, [R,Cs], Unvisited) :-
	D = [_|_],
	R1 is R + 1,
	format("~n", []),
	show_state(D, Cs, [R1,0], Unvisited).
show_state([D|Ds], Cs, [R,C], Unvisited) :-
	C < Cs,
	(	member([R,C], Unvisited) ->
		format("~d", [D])
	;	format("\x1b\[90m~d\x1b\[0m", [D])
	),
	C1 is C + 1,
	show_state(Ds, Cs, [R,C1], Unvisited).

