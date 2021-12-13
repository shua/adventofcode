:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(pio)).

dots([]) --> "\n".
dots([X-Y|XYs]) --> csints([X,Y]), "\n", dots(XYs).

fold(A-N) --> "fold along ", [C], "=", uint(N), { atom_chars(A, [C]) }, "\n".
folds([]) --> "\n".
folds([F|Fs]) --> fold(F), folds(Fs).

input(Ds, Fs) :-
	phrase_from_input((dots(Ds), folds(Fs))).
sample(Ds, Fs) :-
	phrase_from_file((dots(Ds), folds(Fs)), 'sample.txt').

fold1([], _, []).
fold1([X-Y|Dots], y-Fy, [Do|DotsOut]) :-
	(	Y < Fy, Do = X-Y
	;	Y > Fy, Dy is Fy - (Y - Fy), Do = X-Dy
	),
	fold1(Dots, y-Fy, DotsOut).
fold1([X-Y|Dots], x-Fx, [Do|DotsOut]) :-
	(	X < Fx, Do = X-Y
	;	X > Fx, Dx is Fx - (X - Fx), Do = Dx-Y
	),
	fold1(Dots, x-Fx, DotsOut).

answer1(N, Ds, Fs, F0Ds) :-
	input(Ds, Fs),
	Fs = [F0|_],
	fold1(Ds, F0, F0Ds0),
	sort(F0Ds0, F0Ds),
	length(F0Ds, N).
