:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(dcgs)).

input(Is) :- phrase_from_input((csints(Is), "\n\n")).
sample(Is) :- phrase(csints(Is), "16,1,2,0,4,2,7,1,2,14").

add_crab([], X, []) :- X =< 0.
add_crab([], X, [X|NewWts]) :-
	XX is X - 1,
	add_crab([], XX, NewWts).
add_crab([Wt|Wts], X, [NewWt|NewWts]) :-
	( X > 0, NewWt is Wt + X ; X =< 0, NewWt is Wt - X ),
	XX is X - 1,
	add_crab(Wts, XX, NewWts).

calc_weights(Xs, Wts) :-
	calc_weights(Xs, [], Wts).
calc_weights([], Wts, Wts).
calc_weights([X|Xs], Wts, WtsOut) :-
	add_crab(Wts, X, WtsCur),
	calc_weights(Xs, WtsCur, WtsOut).

min([N|Ns], Min, Pos) :- min(Ns, N, Min, Pos0), (N = Min, Pos = 0 ; N > Min, Pos is Pos0 + 1 ).
min([], Min, Min, 0).
min([N|Ns], MinCur, Min, Pos) :-
	N < MinCur, min(Ns, N, Min, Pos0), (N = Min, Pos = 0 ; N > Min, Pos is Pos0 + 1 )
;	N >= MinCur, min(Ns, MinCur, Min, Pos0), Pos is Pos0+1.

msort_key([], []).
msort_key([K|Ks], [K-0|KVs]) :- msort_key(Ks, KVs).
msort(L, Sorted) :-
	msort_key(L, KVs),
	keysort(KVs, SortedKVs),
	msort_key(Sorted, SortedKVs).

median(Sorted, [M1,M2]) :-
	length(Sorted, N),
	append(Pre, [M1,M3|_], Sorted),
	length(Pre, PN),
	(	1 is N mod 2, N is PN * 2 + 1, M2 = M1
	;	0 is N mod 2, N is PN * 2 + 2, M2 = M3
	).
calc_weight([], _, 0).
calc_weight([X|Xs], At, Wt) :-
	calc_weight(Xs, At, Wt0),
	( X =< At, Wt is Wt0 + (At - X) ; X > At, Wt is Wt0 + (X - At) ).

answer1(N) :- answer1(N, _, _).
answer1(N, Xs, Median) :-
	input(Xs),
	msort(Xs, Sorted),
	median(Sorted, [Median,Median]),
	calc_weight(Sorted, Median, N).

% 1+2+3+...+n = n * (n+1) / 2
% f_n(x) =[x-n > 0] (x-n)*(x-n+1)/2 = ((x-n)^2+(x-n))/2
%        =[x-n > 0] (x^2-2xn+n^2+x-n)/2
%        =[x-n = 0] 0
%        =[x-n < 0] (x-n)*(x-n-1)/2 = ((x-n)^2-(x-n))/2
%        =[x-n < 0] (x^2-2xn+n^2-x+n)/2
% f_n'(x) =[x-n > 0] 1/2 * (2x - 2n + 1) = x - n + 1/2
%         =[x-n < 0] 1/2 * (2x - 2n - 1) = x - n - 1/2
% f(x) = Σ i ∈ I . f_i(x)
% f'(x) = Σ i ∈ I . f_i'(x) = |I|x - (Σ i ∈ I . i) + C
% the 0 point of f'(x) is the minimum of f(x)
add_crab2([], X, []) :- X =< 0.
add_crab2([], X, [Y|NewWts]) :-
	Y is X * (X+1) div 2,
	XX is X - 1,
	add_crab2([], XX, NewWts).
add_crab2([Wt|Wts], X, [NewWt|NewWts]) :-
	(	X > 0, NewWt is Wt + (X * (X+1) div 2)
	;	X = 0, NewWt is Wt
	;	X < 0, NewWt is Wt + (X * (X-1) div 2)
	),
	XX is X - 1,
	add_crab2(Wts, XX, NewWts).

% let's keep this in integers by 2x everything
% 2f_n'(x) = 2x - 2n +- 1
% Σ i ∈ I . 2f_i'(x) = 2 * Σ i ∈ I . f_i'(x)
% 2f'(x) = 0 -> f'(x) = 0
dxdfn2(N, X, Y) :-
	X > N, Y is 2*X - 2*N + 1
;	X = N, Y is 0
;	X < N, Y is 2*X - 2*N - 1.
dxdf2([], _, 0).
dxdf2([I|Is], X, Y) :-
	dxdfn2(I, X, Yi),
	dxdf2(Is, X, Yn),
	Y is Yi + Yn.

find_0(Is, X, [Xl, Xr]) :-
	dxdf2(Is, X, Y),
	(	Y = 0, Xl = X, Xr = X
	;	Y > 0, XX is X - 1,
		dxdf2(Is, XX, YY),
		( YY < 0, Xl = XX, Xr = X ; YY >= 0, find_0(Is, XX, [Xl, Xr]) )
	;	Y < 0, XX is X + 1,
		dxdf2(Is, XX, YY),
		( YY > 0, Xl = X, Xr = XX ; YY =< 0, find_0(Is, XX, [Xl, Xr]) )
	).
find_0(Is, X0) :-
	length(Is, In),
	sum(Is, Isum),
	XStart is Isum div In,
	find_0(Is, XStart, X0).

fn(N, X, Y) :-
	(	X > N, Y2 is ((X-N)*(X-N+1))
	;	X = N, Y2 = 0
	;	X < N, Y2 is ((X-N)*(X-N-1))
	), (integer(X), Y is Y2 div 2 ; float(X), Y is Y2 / 2).
f([], _, 0).
f([I|Is], X, Y) :-
	fn(I, X, Yi),
	f(Is, X, Yn),
	Y is Yi + Yn.

max([N|Ns], Max) :- max(Ns, N, Max).
max([], Max, Max).
max([N|Ns], Cmax, Max) :-
	N > Cmax, max(Ns, N, Max)
;	N =< Cmax, max(Ns, Cmax, Max).

calc_weight2(Is, Wts) :-
	max(Is, Xmax),
	calc_weight2(Is, Xmax, 0, Wts).
calc_weight2(_, X, X, []).
calc_weight2(Is, Xmax, X, [Wt|Wts]) :-
	f(Is, X, Wt),
	XX is X+1,
	calc_weight2(Is, Xmax, XX, Wts).


answer2(N) :- answer2(N, _, _).
answer2(N, Xs, [Xl, Xr]) :-
	input(Xs),
	find_0(Xs, [Xl, Xr]),
	f(Xs, Xl, Yl),
	f(Xs, Xr, Yr),
	min([Yl,Yr], N, _).
