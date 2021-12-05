:- use_module('../util.pl').
:- use_module(library(pio)).
:- use_module(library(lists)).

csints([N|Ns]) --> uint(N), ",", csints(Ns).
csints([N]) --> uint(N).

bingo_board([]) --> "\n".
bingo_board([A-0,B-0,C-0,D-0,E-0|Rest]) -->
	(" " ; []), uint(A), (" " ; "  "),
	uint(B), (" " ; "  "),
	uint(C), (" " ; "  "),
	uint(D), (" " ; "  "),
	uint(E), "\n",
	bingo_board(Rest).

bingo_boards([B|Boards]) --> bingo_board(B), bingo_boards(Boards).
bingo_boards([]) --> [].
puzzle_input(Nums, Boards) --> csints(Nums), "\n\n", bingo_boards(Boards).

collect_unused([], []).
collect_unused([_-1|Ns], NOut) :- collect_unused(Ns, NOut).
collect_unused([N-0|Ns], [N|NOut]) :- collect_unused(Ns, NOut).

sum([], 0).
sum([N|Ns], Sum) :- sum(Ns, Sum0), Sum is Sum0 + N.

final_score(Board, WN, Score) :-
	collect_unused(Board, Nums),
	sum(Nums, Sum),
	Score is WN * Sum.

mark_board([], _, []).
mark_board([N-_|Board], N, [N-1|NextBoard]) :- mark_board(Board, N, NextBoard).
mark_board([M-X|Board], N, [M-X|NextBoard]) :- N \= M, mark_board(Board, N, NextBoard).
mark_boards([], _, []).
mark_boards([B|Boards], N, [BNext|NextBoards]) :-
	mark_board(B, N, BNext),
	mark_boards(Boards, N, NextBoards).


collect_winners([], []).
% do I really need this to be strict unprovable?
collect_winners([B|In], Out) :- \+ bingo(B), collect_winners(In, Out).
collect_winners([B|In], [B|Out]) :- bingo(B), collect_winners(In, Out).

bingo(Board) :- hbingo(Board) ; vbingo(_, Board).
hbingo([_-1,_-1,_-1,_-1,_-1|_]).
hbingo([_,_,_,_,_|Rest]) :- hbingo(Rest).
vbingo(_, []).
vbingo(0, [_-1,_,_,_,_|Rest]) :- vbingo(0, Rest).
vbingo(1, [_,_-1,_,_,_|Rest]) :- vbingo(1, Rest).
vbingo(2, [_,_,_-1,_,_|Rest]) :- vbingo(2, Rest).
vbingo(3, [_,_,_,_-1,_|Rest]) :- vbingo(3, Rest).
vbingo(4, [_,_,_,_,_-1|Rest]) :- vbingo(4, Rest).

% play_bingo(Nums, Boards, Num, Winners)
play_bingo([N|Nums], Boards, W, Winners) :-
	mark_boards(Boards, N, NextBoards),
	(	Winners = [_|_], W = N, collect_winners(NextBoards, Winners)
	;	play_bingo(Nums, NextBoards, W, Winners)
	).

sample_input(Nums, Boards) :- phrase_from_file(puzzle_input(Nums, Boards), 'sample.txt').

answer1(N, Nums, Boards) :-
	play_bingo(Nums, Boards, WN, [Winner|_]),
	final_score(Winner, WN, N).
answer1(N) :-
	phrase_from_input(puzzle_input(Nums, Boards)),
	answer1(N, Nums, Boards).

group_winners([], [], []).
group_winners([B|Boards], [B|Winners], Losers) :- bingo(B), group_winners(Boards, Winners, Losers).
group_winners([B|Boards], Winners, [B|Losers]) :- \+ bingo(B), group_winners(Boards, Winners, Losers).
% play_bingo(Nums, Boards, Num, Winners)
play_bingo2(Boards, [N|Nums], WinNum, RestNums, Winners, Losers) :-
	mark_boards(Boards, N, NextBoards),
	group_winners(NextBoards, CurWinners, CurLosers),
	(	WinNum = N, Winners = CurWinners, Losers = CurLosers, RestNums = Nums
	;	play_bingo2(CurLosers, Nums, WinNum, RestNums, NextWinners, Losers),
		append(CurWinners, NextWinners, Winners)
	).

answer2(N) :- 
	phrase_from_input(puzzle_input(Nums, Boards)),
	answer2(N, Nums, Boards, _, _, _, _, _).
answer2(N, Nums, Boards, Last, Winners, WinNum, RestNums, LastNums) :-
	play_bingo2(Boards, Nums, _, RestNums, _, [Last]),
	play_bingo2([Last], RestNums, WinNum, LastNums, Winners, []),
	mark_board(Last, WinNum, Winner),
	final_score(Winner, WinNum, N).

