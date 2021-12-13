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

answer1(N) :- answer1(N, _, _, _).
answer1(N, Ds, Fs, F0Ds) :-
	input(Ds, Fs),
	Fs = [F0|_],
	fold1(Ds, F0, F0Ds0),
	sort(F0Ds0, F0Ds),
	length(F0Ds, N).


foldn(Ds, [], Ds).
foldn(Ds, [F|Fs], FnDs) :-
	fold1(Ds, F, D1),
	foldn(D1, Fs, FnDs).

max_x([X-_], X).
max_x([X-_|XYs], Mx) :-
	max_x(XYs, Mx0),
	(X > Mx0, Mx = X ; X =< Mx0, Mx = Mx0).
max_y([_-Y], Y).
max_y([_-Y|XYs], My) :-
	max_y(XYs, My0),
	(Y > My0, My = Y ; Y =< My0, My = My0).
	

show_dots(Ds) :-
	max_x(Ds, Cols1), Cols is Cols1 + 1,
	max_y(Ds, Rows1), Rows is Rows1 + 1,
	show_dots(0-0, Rows-Cols, Ds).
% not the most efficient iterating through points instead of dots,
% but hopefully member/2 isn't slow
show_dots(Rows-0, Rows-_, _).
show_dots(Row-Cols, Rows-Cols, Ds) :-
	Row < Rows,
	write('\n'),
	Row1 is Row + 1,
	show_dots(Row1-0, Rows-Cols, Ds).
show_dots(Row-Col, Rows-Cols, Ds) :-
	Row < Rows, Col < Cols,
	(member(Col-Row, Ds) -> write('#') ; write('.')),
	Col1 is Col + 1,
	show_dots(Row-Col1, Rows-Cols, Ds).

% no N version because that would require bitmap font, not really motivated right now, so just using my eyes
answer2(_) :- answer2(_,_,_).
answer2(Ds, Fs, FnDs) :-
	input(Ds, Fs),
	time(foldn(Ds, Fs, FnDs0)),
	sort(FnDs0, FnDs),
	show_dots(FnDs).
