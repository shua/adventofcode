:- use_module('../util.pl').
:- use_module(library(lists)).

line([]) --> "\n".
line([D|Ds]) --> digit(D), line(Ds).

lines([]) --> "\n".
lines([L|Ls]) --> line(L), lines(Ls).

input(Ls) :- phrase_from_input(lines(Ls)).

sample(input(
	[[1,2,3],
	 [4,5,6],
	 [7,8,9]])).
sample(add_mat(
	[[1,8,0],
	 [0,0,3],
	 [2,5,1]])).

list_of(0, _, []).
list_of(N, V, [V|Vs]) :- N > 0, N1 is N - 1, list_of(N1, V, Vs).

sim(In, Out, Flashes) :-
	In = [L0|_], length(L0, N), length(In, M),
	list_of(N, 1, IL0), list_of(M, IL0, Incr0),
	sim(flash, In, Incr0, Result),
	sim(out, Result, Flashes, Out).

% todo: recurse
sim(flash, In, Add, Out) :-
	write('add'),
	sim(add, In, Add, Out0, [], FlashMap),
	write('added'),
	length(FlashMap, Fn),
	(	Fn = 0, Out = Out0
	;	Fn > 0,
		write('flashmap[]: '), write(Fn), write('\n'),
		length(In, M), In = [L0|_], length(L0, N),
		agg_flashes(M, N, FlashMap, Incr),
		sim(flash, Out0, Incr, Out)
	).

sim(add, [], [], [], FlashMap, FlashMap).
sim(add, [[]|In], [[]|Add], [[]|Out], FlashIn, FlashMap) :-
	sim(add, In, Add, Out, FlashIn, FlashMap).
sim(add, [[D|Ds]|In], [[A|As]|Add], [[DA|DOs]|Out], FlashIn, FlashMap) :-
	DA is D + A, 
	(	D < 10, DA >= 10,
		length([D|Ds], Dsn), length([[]|In], Inn),
		append_pts_around(Inn-Dsn, FlashIn, FlashNext)
	;	( D >= 10 ; DA < 10 ),
		FlashNext = FlashIn
	),
	sim(add, [Ds|In], [As|Add], [DOs|Out], FlashNext, FlashMap).

% don't worry about negatives, we filter them out later using filter2d
append_pts_around(X-Y, In, Out) :-
	Xl is X - 1, Xr is X + 1,
	Yl is Y - 1, Yr is Y + 1,
	Out = [
		Xl-Yl, X-Yl, Xr-Yl,
		Xl-Y,        Xr-Y,
		Xl-Yr, X-Yr, Xr-Yr  |In].

agg_flashes(M, N, FlashMap, Incr) :-
	key_init(FlashMap, 1, Init0),
	keysort(Init0, InitR),
	% the flashmap is using length of *remaining* for X,Y
	% so what we think of as 0,0 would be stored as M-1,N-1
	reverse(InitR, Init),
	filter2d(Init, 1-1, M-N, Filtered),
	reduce_add(Filtered, FlashOut),
	sparse_to_dense(M, N, FlashOut, Incr).

key_init([], _, []).
key_init([K|Ks], V, [K-V|KVs]) :- key_init(Ks, V, KVs).

filter2d([], _, _, []).
filter2d([X-Y-V|KVs], X0-Y0, Xn-Yn, Out) :-
	(	X >= X0, X =< Xn,
		(	Y >= Y0, Y =< Yn, Out = [X-Y-V|Out0]
		;	( Y < Y0 ; Y > Yn ), Out = Out0
		)
	;	( X < X0 ; X > Xn ), Out = Out0
	),
	filter2d(KVs, X0-Y0, Xn-Yn, Out0).

reduce_add([], []).
reduce_add([E], [E]).
reduce_add([K1-V1,K2-V2|KVs], [K1-V1|Out]) :-
	K1 @> K2,
	reduce_add([K2-V2|KVs], Out).
reduce_add([K-V1,K-V2|KVs], Out) :-
	V is V1 + V2,
	reduce_add([K-V|KVs], Out).

sparse_to_dense(M, N, Sparse, Dense) :-
	sparse_to_dense(M, N, N, Sparse, Dense).
sparse_to_dense(0, _, _, [], []).
sparse_to_dense(M, N, 0, KVs, [[]|Rows]) :-
	M > 0,
	M1 is M - 1, sparse_to_dense(M1, N, N, KVs, Rows).
sparse_to_dense(M, N, Ni, [], [[0|Cols]|Rows]) :-
	Ni > 0,
	N1 is Ni - 1, sparse_to_dense(M, N, N1, [], [Cols|Rows]).
sparse_to_dense(M, N, Ni, [M-Ni-V|KVs], [[V|Cols]|Rows]) :-
	M > 0, Ni > 0,
	N1 is Ni - 1, sparse_to_dense(M, N, N1, KVs, [Cols|Rows]).
sparse_to_dense(M, N, Ni, [M-Col-V|KVs], [[0|Cols]|Rows]) :-
	M > 0, Ni > 0, Col < Ni,
	N1 is Ni - 1, sparse_to_dense(M, N, N1, [M-Col-V|KVs], [Cols|Rows]).
sparse_to_dense(M, N, Ni, [Row-Col-V|KVs], [[0|Cols]|Rows]) :-
	M > 0, Ni > 0, Row < M,
	N1 is Ni - 1, sparse_to_dense(M, N, N1, [Row-Col-V|KVs], [Cols|Rows]).

sim(out, [], 0, []).
sim(out, [[]|Rows], F, [[]|Rowo]) :-
	sim(out, Rows, F, Rowo).
sim(out, [[D|Cols]|Rows], F, [[Do|Colo]|Rowo]) :-
	(	D < 10,
		F is F1 + 1, Do = D
	;	D >= 10,
		F = F1, Do = 0
	),
	sim(out, [Cols|Rows], F1, [Colo|Rowo]).

