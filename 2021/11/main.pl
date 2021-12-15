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

mat_incr([], []).
mat_incr([[]|Rows], [[]|R1]) :-
	mat_incr(Rows, R1).
mat_incr([[V|Cols]|Rows], [[V1|C1]|R1]) :-
	V1 is V + 1,
	mat_incr([Cols|Rows], [C1|R1]).

mat_add_sparse(Mat, Sparse, Out) :-
	length(Mat, Rn), Mat = [R1|_], length(R1, Cn),
	keysort(Sparse, S1), reverse(S1, S2),
	mat_add_sparse(Cn, Rn, Cn, Mat, S2, Out).
mat_add_sparse(_, 0, _, _, _, _, []).
mat_add_sparse(Cn, Ri, 0, [[]|Mat], Sp, Out) :-
	Ri > 0, R1 is Ri - 1,
	mat_add_sparse(Cn, R1, Cn, Mat, Sp, Out).
mat_add_sparse(Cn, Ri, Ci, [[V1|Cs]|Rs], [Ri-Ci-V2|Sparse], [Vo|Out]) :-
	Ri > 0, Ci > 0,
	Vo is V1 + V2,
	C1 is Ci - 1,
	mat_add_sparse(Cn, Ri, C1, [Cs|Rs], Sparse, Out).
mat_add_sparse(Cn, Ri, Ci, [[V1|Cs]|Rs], [R-C-V2|Sparse], [V1|Out]) :-
	Ri > 0, Ci > 0,
	( R > Ri ; R = Ri, C > Ci ),
	C1 is Ci - 1,
	mat_add_sparse(Cn, Ri, C1, [Cs|Rs], [R-C-V2|Sparse], Out).

mat_gteq_sp([], _, []).
mat_gteq_sp([[V|Cs]|Rs], N, Sp) :-
	V < N,
	mat_gteq_sp([Cs|Rs], N, Sp).
mat_gteq_sp([[V|Cs]|Rs], N, [Ri-Ci|Sp]) :-
	V >= N,
	length(Cs, Ci), length(Rs, Ri),
	mat_gteq_sp([Cs|Rs], N, Sp).

keys_with_val([], _, []).
keys_with_val([_-V1|KVs], V2, Ks) :-
	( V1 > V2 ; V1 < V2 ),
	keys_with_val(KVs, Ks).
keys_with_val([K-V|KVs], V, [K|Ks]) :-
	keys_with_val(KVs, Ks).
set_diff(S1, S2, Sd) :-
	append(S1, S2, S3), append(S3, S2, S),
	counts(S, SCts),
	keys_with_val(SCts, 1, Sd).

matrix(m(Stride, Data)) -->
	line(Data), "\n", { length(Data, Stride) }.
matrix(m(Stride, Data)) -->
	line(L), { length(L, Stride) },
	matrix(m(Stride, Data0)), { append(L, Data0, Data) }.

minc(m(S, Ds), m(S, Do)) :- minc(Ds, Do).
minc([], []).
minc([D|Ds], [Do|Dos]) :- Do is D + 1, minc(Ds, Dos).

msize(m(S, D), Rows, Cols) :-
	length(D, Dn), Rows is Dn div S, Cols = S.

mflash(m(S, D1), m(S, D2), msp(MSp)) :-
	length(D1, Dn),
	mflash(Dn, S, Dn, D1, D2, MSp).
mflash(N, S, Di, MSp1, MSp2) :-

mflash(N, S, Di, [D1|D1s], [D2|D2s], MSp) :-
	D2 > D1,
	(	D1 =< 9, D2 > 9, mflash(N, S, Di, MSp0, MSp)
	;	D2 =< 9, MSp = MSp0
	;	D1 > 9, MSp = MSp0
	),
	D1 is Di - 1,
	mflash(N, S, D1, D1s, D2s, MSp0).

