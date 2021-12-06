:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).

min_max(X1, X2, X2, X1) :- X1 >= X2.
min_max(X1, X2, X1, X2) :- X1 < X2.
min_max([[X1,Y1,X2,Y2]], [XMin,YMin], [XMax, YMax]) :-
	min_max(X1, X2, XMin, XMax), min_max(Y1, Y2, YMin, YMax).
min_max([[X1,Y1,X2,Y2]|Lines], [XMin, YMin], [XMax, YMax]) :-
	min_max(Lines, [XMin0, YMin0], [XMax0, YMax0]),
	min_max(X1, X2, XMin1, XMax1), min_max(Y1, Y2, YMin1, YMax1),
	min_max(XMin1, XMin0, XMin, _),
	min_max(XMax1, XMax0, _, XMax),
	min_max(YMin1, YMin0, YMin, _),
	min_max(YMax1, YMax0, _, YMax).

line([X1,Y1,X2,Y2]) --> uint(X1), ",", uint(Y1), " -> ", uint(X2), ",", uint(Y2).
lines([L|Ls]) --> line(L), "\n", lines(Ls).
lines([]) --> [].

group_lines1([], [], []).
group_lines1([[X1,Y1,X2,Y2]|Ls], [[X1,Y1,X2,Y2]|Out], OutDiag) :-
	(X1 = X2 ; Y1 = Y2),
	group_lines1(Ls, Out, OutDiag).
group_lines1([[X1,Y1,X2,Y2]|Ls], Out, [[X1,Y1,X2,Y2]|OutDiag]) :-
	(X1 > X2 ; X1 < X2), (Y1 > Y2 ; Y1 < Y2),
	group_lines1(Ls, Out, OutDiag).

list_of(X, [X|Xs]) :- list_of(X, Xs).
list_of(_, []).
board_size(Board, X, Y) :- XY is X * Y, length(Board, XY).
board_row(Board, Stride, Y, Row) :-
	board_row(Board, Stride, 0, Y, Row).
board_row(Board, Stride, I, N, Row) :-
	I < N, II is I + 1,
	append(Pre, Post, Board), length(Pre, Stride),
	board_row(Post, Stride, II, N, Row).
board_row(Board, Stride, N, N, Row) :-
	append(Row, _, Board), length(Row, Stride).
board_xy(Board, Stride, X, Y, Val) :-
	I is Y*Stride + X,
	append(Pre, [Val|_], Board),
	length(Pre, I).

mark_point(Board, Stride, X, Y, BoardOut) :-
	I is Stride*Y + X,
	mark_point(Board, I, BoardOut).
mark_point(Board, I, BoardOut) :-
	append(Pre, [C|Post], Board), length(Pre, I),
	C1 is C + 1,
	append(Pre, [C1|Post], BoardOut).
mark_line(Board, Stride, [X,Y,X,Y], BoardOut) :-
	mark_point(Board, Stride, X, Y, BoardOut).
mark_line(Board, Stride, [X1,Y,X2,Y], BoardOut) :-
	X1 > X2,
	mark_line(Board, Stride, [X2,Y,X1,Y], BoardOut).
mark_line(Board, Stride, [X1,Y,X2,Y], BoardOut) :-
	X1 < X2,
	mark_point(Board, Stride, X1, Y, BoardCur),
	XX is X1 + 1,
	mark_board(BoardCur, Stride, [XX,Y,X2,Y], BoardOut).
mark_line(Board, Stride, [X,Y1,X,Y2], BoardOut) :-
	Y1 > Y2, mark_line(Board, Stride, [X,Y2,X,Y1], BoardOut).
mark_line(Board, Stride, [X,Y1,X,Y2], BoardOut) :-
	Y1 < Y2,
	mark_point(Board, Stride, X, Y1, BoardCur),
	YY is Y1 + 1,
	mark_line(BoardCur, Stride, [X,YY,X,Y2], BoardOut).

mark_lines(Board, _, [], Board).
mark_lines(Board, Stride, [L|Ls], BoardOut) :-
	mark_line(Board, Stride, L, BoardCur),
	mark_lines(BoardCur, Stride, Ls, BoardOut).

% building board with counts, then looping through board is M+N
% querying each point on board is M*N
% prepare: split y into points, make sure [X1,Y,X2,Y] that X2 > X1
% sort lines by Y then X1
% for each line: for each point in line: mark current_line
% count up points in current_line where marks >= 2, add to running sum

split_verts([], []).
split_verts([[X1,Y,X2,Y]|Ls], [[X1,Y,X2,Y]|LSplit]) :-
	split_verts(Ls, LSplit).
split_verts([[X,Y1,X,Y2]|Ls], [[X,Y1,X,Y1]|LSplit]) :-
	Y1 < Y2,
	YY is Y1 + 1,
	split_verts([[X,YY,X,Y2]|Ls], LSplit).

normalize([], []).
normalize([[X1,Y1,X2,Y2]|Ls], [[X1,Y1,X2,Y2]|LNorm]) :-
	(Y1 < Y2 ; Y1 = Y2, X1 < X2),
	normalize(Ls, LNorm).
normalize([[X1,Y1,X2,Y2]|Ls], [[X2,Y2,X1,Y1]|LNorm]) :-
	(Y1 > Y2 ; Y1 = Y2, X1 > X2),
	normalize(Ls, LNorm).

values([], []).
values([_-V|KVs], [V|Vs]) :- values(KVs, Vs).
key_lines([], []).
key_lines([[X1,Y,X2,Y]|Ls], [Y-[X1,Y,X2,Y]|Keyed]) :- key_lines(Ls, Keyed).
sort_lines(Ls, Sorted) :-
	key_lines(Ls, LKeyed),
	keysort(LKeyed, KSorted),
	values(KSorted, Sorted).

prepare1(Ls, YSorted) :-
	time(group_lines1(Ls, LStr, _)),
	time(normalize(LStr, LNorm)),
	time(split_verts(LNorm, LOut)),
	time(sort_lines(LOut, YSorted)).

% lines could be split into start/end, and sorted as points
% for each point:
% 	run_length = x - last_x
% 	increase counts for val by run_length
% 	val = point is start ? 1 : -1,
agg_counts([], []).
agg_counts(LHorz, Counts) :-
	horz_critpts(LHorz, CritPts, LNext),
	keysort(CritPts, SortedCritPts),
	agg_counts(SortedCritPts, 0, 0, [], LineCounts),
	agg_counts(LNext, CountsNext),
	merge(LineCounts, CountsNext, Counts).
agg_counts([], _, _, CountsCur, Counts) :- merge(CountsCur, [], Counts).
agg_counts([X-V|SortedCritPts], XLast, Val, Counts, CountsOut) :-
	RunLength is X - XLast,
	CountsCur = [Val-RunLength|Counts],
	ValNext is Val + V,
	agg_counts(SortedCritPts, X, ValNext, CountsCur, CountsOut).

horz_critpts([], [], []).
horz_critpts(LHorz, CritPts, LRest) :-
	LHorz = [[_, Y, _, Y]|_],
	horz_critpts(LHorz, Y, CritPts, LRest).
horz_critpts([], _, [], []).
horz_critpts([[X1,Y2,X2,Y2]|LHorz], Y1, [], [[X1,Y2,X2,Y2]|LHorz]) :- Y2 > Y1.
horz_critpts([[X1,Y,X2,Y]|LHorz], Y, [X1-1,XEnd-(-1)|CritPts], LRest) :-
	XEnd is X2 + 1,
	horz_critpts(LHorz, Y, CritPts, LRest).

% assumes A and B are [K-V|...], Ks are orderable, and Vs can be added
merge(A, B, Out) :-
	append(A, B, AB),
	keysort(AB, Sorted),
	merge_(Sorted, Out).
merge_([], []).
merge_([X], [X]).
merge_([K1-V1,K2-V2|Elems], [K1-V1|Out]) :-
	K2 > K1,
	merge_([K2-V2|Elems], Out).
merge_([K-V1,K-V2|Elems], Out) :-
	V is V1 + V2,
	merge_([K-V|Elems], Out).

calc_answer1([], 0).
calc_answer1([K-V|Counts], N) :-
	K >= 2, calc_answer1(Counts, N1), N is N1 + V
;	K < 2, calc_answer1(Counts, N).

% feel like some run-length encoding would be best here, but I don't really know how to write that easily
% also thinking about storing points in a quad-tree?
answer1(N) :- answer1(N, _, _, _).
answer1(N, Lines, Ls, Counts) :-
	phrase_from_input((lines(Lines), "\n")),
	prepare1(Lines, Ls),
	agg_counts(Ls, Counts),
	calc_answer1(Counts, N).

% this assumes all the points in LDiag have already been normalized to go from top-to-bottom (Y2 > Y1)
split_diag([], []).
split_diag([[X,Y,X,Y]|LDiag], [[X,Y,X,Y]|LDiagPts]) :- split_diag(LDiag, LDiagPts).
split_diag([[X1,Y1,X2,Y2]|LDiag], [[X1,Y1,X1,Y1]|LDiagPts]) :-
	(X2 > X1, XX is X1 + 1 ; X2 < X1, XX is X1 - 1),
	YY is Y1 + 1,
	split_diag([[XX,YY,X2,Y2]|LDiag], LDiagPts).

prepare2(Lines, YSorted) :-
	time(normalize(Lines, LNorm)),
	time(group_lines1(LNorm, LStr, LDiag)),
	time(split_verts(LStr, LSplit)),
	time(split_diag(LDiag, LDiagPts)),
	append(LDiagPts, LSplit, LOut),
	time(sort_lines(LOut, YSorted)).

answer2(N, Lines, Ls, Counts) :-
	phrase_from_input((lines(Lines), "\n")),
	prepare2(Lines, Ls),
	agg_counts(Ls, Counts),
	calc_answer1(Counts, N).

