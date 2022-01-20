:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(time)).

:- op(900, fx, *).
*(_) :- true.

/*
- every one is incremented
- each num > 9 flashes (causes all 8 neighbors to incr)
- can only flash once per round
*/

mat_row([], 0) --> "\n".
mat_row([D|Ds], N) --> digit(D), mat_row(Ds, N0), { N is N0 + 1 }.
mat(m(N, D)) --> mat_row(D0, N), mat(m(N, Dn)), { append(D0, Dn, D) }.
mat(m(N, D)) --> mat_row(D, N).

input(M) :- phrase_from_file((mat(M), "\n"), 'input.txt').

sample(0, m(10, D)) :- D = [
	5,4,8,3,1,4,3,2,2,3,
	2,7,4,5,8,5,4,7,1,1,
	5,2,6,4,5,5,6,1,7,3,
	6,1,4,1,3,3,6,1,4,6,
	6,3,5,7,3,8,5,4,7,8,
	4,1,6,7,5,2,4,6,4,5,
	2,1,7,6,8,4,1,7,2,1,
	6,8,8,2,8,8,1,1,3,4,
	4,8,4,6,8,4,8,5,5,4,
	5,2,8,3,7,5,1,5,2,6
].
sample(1, m(10, D)) :- D = [
	6,5,9,4,2,5,4,3,3,4,
	3,8,5,6,9,6,5,8,2,2,
	6,3,7,5,6,6,7,2,8,4,
	7,2,5,2,4,4,7,2,5,7,
	7,4,6,8,4,9,6,5,8,9,
	5,2,7,8,6,3,5,7,5,6,
	3,2,8,7,9,5,2,8,3,2,
	7,9,9,3,9,9,2,2,4,5,
	5,9,5,7,9,5,9,6,6,5,
	6,3,9,4,8,6,2,6,3,7
].
sample(2, m(10, D)) :- D = [
	8,8,0,7,4,7,6,5,5,5,
	5,0,8,9,0,8,7,0,5,4,
	8,5,9,7,8,8,9,6,0,8,
	8,4,8,5,7,6,9,6,0,0,
	8,7,0,0,9,0,8,8,0,0,
	6,6,0,0,0,8,8,9,8,9,
	6,8,0,0,0,0,5,9,4,3,
	0,0,0,0,0,0,7,4,5,6,
	9,0,0,0,0,0,0,8,7,6,
	8,7,0,0,0,0,6,8,4,8
].

mat_iter(m(Cs, D), [Rs,Cs,0,0]) :- length(D, N), Rs is N div Cs.
iter_next([Rs,Cs,R,C], [Rs,Cs,R,C1]) :- R < Rs, C1 is C + 1, C1 < Cs.
iter_next([Rs,Cs,R,C], [Rs,Cs,R1,0]) :- R < Rs, Cs is C + 1, R1 is R + 1.
iter_done([Rs,_,Rs,0]).

incr_all(m(Cols, Di), m(Cols, Do), WL) :-
	mat_iter(m(Cols, Di), It),
	incr_all(It, Di, Do, [], WL).
incr_all(It, [], [], WL, WL) :- iter_done(It).
incr_all(It, [Di|Dsi], [Do|Dso], WLi, WLo) :-
	iter_next(It, Next),
	Do is Di + 1,
	check_flash(Di, Do, It, WLi, WLc),
	incr_all(Next, Dsi, Dso, WLc, WLo).

check_flash(Pre, Post, It, WLi, WLo) :- Pre =< 9, Post > 9, flash(It, WLi, WLo).
check_flash(Pre, Post, _, WL, WL) :- Pre > 9; Post =< 9.

flash([Rs,Cs,R,C], WL, WLo) :-
	R > 0, R < Rs - 1,
	Up is R - 1, Down is R + 1,
	flash_row(Down, Cs, C, WL, WL1),
	flash_mid(R, Cs, C, WL1, WL2),
	flash_row(Up, Cs, C, WL2, WLo)
;	R = 0,
	Down is R + 1,
	flash_row(Down, Cs, C, WL, WL1),
	flash_mid(R, Cs, C, WL1, WLo)
;	R =:= Rs - 1,
	Up is R - 1,
	flash_mid(R, Cs, C, WL, WL1),
	flash_row(Up, Cs, C, WL1, WLo).
flash_row(R, Cs, C, WL, WLo) :-
	C > 0, C < Cs - 1,
	Left is C - 1, Right is C + 1,
	WLo = [[R,Left],[R,C],[R,Right]|WL]
;	C = 0,
	Right is C + 1,
	WLo = [[R,C],[R,Right]|WL]
;	C =:= Cs - 1,
	Left is C - 1,
	WLo = [[R,Left],[R,C]|WL].
flash_mid(R, Cs, C, WL, WLo) :-
	C > 0, C < Cs - 1,
	Left is C - 1, Right is C + 1,
	WLo = [[R,Left],[R,Right]|WL]
;	C = 0,
	Right is C + 1,
	WLo = [[R,Right]|WL]
;	C =:= Cs - 1,
	Left is C - 1,
	WLo = [[R,Left]|WL].

sim(Mi, Mo, Fs) :-
	incr_all(Mi, Mc, WL),
	sim(Mc, Mo, WL, Fs).
sim(Mi, Mo, [], Fs) :-
	norm9s(Mi, Mo, Fs).
sim(Mi, Mo, WLi, Fs) :-
	WLi = [_|_],
	counts(WLi, WLcts),
	sparse_add(Mi, WLcts, Mc, WLc),
	sim(Mc, Mo, WLc, Fs).

norm9s(m(Cols, Di), m(Cols, Do), Fs) :- norm9s_(Di, Do, Fs).
norm9s_([], [], 0).
norm9s_([D|Di], [D|Do], Fs) :- D =< 9, norm9s_(Di, Do, Fs).
norm9s_([D|Di], [0|Do], Fs1) :- D > 9, norm9s_(Di, Do, Fs), Fs1 is Fs + 1.

sparse_add(m(Cols, Di), Sp, m(Cols, Do), WL) :-
	mat_iter(m(Cols, Di), It),
	sparse_add(It, Sp, Di, Do, [], WL).
sparse_add(_, [], D, D, WL, WL).
sparse_add(It, [[Rp,Cp]-V|Sp], [D|Di], [D|Do], WLi, WLo) :-
	iter_next(It, Next), It = [_,_,R,C],
	( Rp > R ; Rp = R, Cp > C ),
	sparse_add(Next, [[Rp,Cp]-V|Sp], Di, Do, WLi, WLo).
sparse_add(It, [[R,C]-V|Sp], [Di|Dis], [Do|Dos], WLi, WLo) :-
	iter_next(It, Next), It = [_,_,R,C],
	Do is Di + V,
	check_flash(Di, Do, It, WLi, WL),
	sparse_add(Next, Sp, Dis, Dos, WL, WLo).

show_mat(m(Cols, D)) :- show_mat(Cols, 0, D).
show_mat(_, 0, []).
show_mat(Cs, Cs, D) :- format("~n", []), show_mat(Cs, 0, D).
show_mat(Cs, C, [D|Ds]) :- C < Cs, C1 is C + 1, format("~d", [D]), show_mat(Cs, C1, Ds).

simn(0, M, M, 0).
simn(N, Mi, Mo, Fs) :-
	N > 0,
	sim(Mi, Mc, Fc),
	N1 is N - 1,
	simn(N1, Mc, Mo, Fn),
	Fs is Fc + Fn.

sim_coord(m(_, D), 0) :- length(D, N), list_of(N, 0, D).
sim_coord(M, N) :-
	sim(M, M1, _),
	sim_coord(M1, Nn),
	N is Nn + 1.


answer1(N) :- input(M), time(simn(100, M, _, N)).

answer2(N) :- input(M), time(sim_coord(M, N)).

