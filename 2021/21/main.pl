:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(format)).

% positions are 1-10
% Player 1 starting position: 7
% Player 2 starting position: 4
input(6, 3).

/*
This game consists of a single die, two pawns, and a game board with a circular track containing ten spaces marked 1 through 10 clockwise. Each player's starting space is chosen randomly (your puzzle input). Player 1 goes first.

Players take turns moving. On each player's turn, the player rolls the die three times and adds up the results. Then, the player moves their pawn that many times forward around the track (that is, moving clockwise on spaces in order of increasing value, wrapping back around to 1 after 10). So, if a player is on space 7 and they roll 2, 2, and 1, they would move forward 5 times, to spaces 8, 9, 10, 1, and finally stopping on 2.

After each player moves, they increase their score by the value of the space their pawn stopped on. Players' scores start at 0. So, if the first player starts on space 7 and rolls a total of 5, they would stop on space 2 and add 2 to their score (for a total score of 2). The game immediately ends as a win for any player whose score reaches at least 1000.
*/

% N turns -> +N
%   6+4+2+0+8+6+4+2+0+8 mod 10 = 0   6,0,2,2,0,6,0,2,2,0
%   5+3+1+9+7+5+3+1+9+7 mod 10 = 0   5,8,9,8,5,0,3,4,3,0
% P1_1 -> +P1_0+(2*3 mod 10)                       = +(P1_0+6 mod 10)
% P2_1 -> +P2_0+(5*3 mod 10)                       = +(P2_0+5 mod 10)
% P1_2 -> +P1_1+(8*3 mod 10)  = +P2_1+(8*3 mod 10) = +(P1_0+6+4 mod 10) = +(P1_0+0 mod 10)
% P2_2 -> +P2_1+(11*3 mod 10) = +P2_1+(1*3 mod 10) = +(P2_0+5+3 mod 10) = +(P2_0+8 mod 10)
% P1_3 -> +P1_2+(14*3 mod 10) = +P1_2+(4*3 mod 10) = +(P1_0+6+4+2 mod 10) = +(P1_0+2 mod 10)
% P2_3 -> +P2_2+(17*3 mod 10) = +P2_2+(7*3 mod 10) = +(P2_0+5+3+1 mod 10) = +(P2_0+9 mod 10)
% P1_4 -> +P1_3+(20*3 mod 10) = +P1_3+(0*3 mod 10) = +(P1_0+6+4+2+0 mod 10) = +(P1_0+2 mod 10)
% P1_4 -> +P1_3+(23*3 mod 10) = +P2_3+(3*3 mod 10) = +(P2_0+5+3+1+9 mod 10) = +(P2_0+8 mod 10)
% P1_5 -> +P1_4+(26*3 mod 10) = +P1_4+(6*3 mod 10) = +(P1_0+6+4+2+0+8 mod 10) = +(P1_0+0 mod 10)
% P1_5 -> +P1_4+(29*3 mod 10) = +P2_4+(9*3 mod 10) = +(P2_0+5+3+1+9+7 mod 10) = +(P2_0+5 mod 10)
% P1_6 -> +P1_5+(32*3 mod 10) = +P1_5+(2*3 mod 10) = +(P1_0+6+4+2+0+8+6 mod 10) = +(P1_0+6 mod 10)
% P1_6 -> +P1_5+(35*3 mod 10) = +P2_5+(5*3 mod 10) = +(P2_0+5+3+1+9+7+5 mod 10) = +(P2_0+0 mod 10)
% P1_7 -> +P1_6+(38*3 mod 10) = +P1_6+(8*3 mod 10) = +(P1_0+6+4+2+0+8+6+4 mod 10) = +(P1_0+0 mod 10)
% P1_7 -> +P1_6+(41*3 mod 10) = +P2_6+(1*3 mod 10) = +(P2_0+5+3+1+9+7+5+3 mod 10) = +(P2_0+3 mod 10)
% P1_8 -> +P1_7+(44*3 mod 10) = +P1_7+(4*3 mod 10) = +(P1_0+6+4+2+0+8+6+4+2 mod 10) = +(P1_0+2 mod 10)
% P1_8 -> +P1_7+(47*3 mod 10) = +P2_7+(7*3 mod 10) = +(P2_0+5+3+1+9+7+5+3+1 mod 10) = +(P2_0+4 mod 10)
%
% S1_1 -> 1+P1_1 = 1+(P1_0+6 mod 10)
% S2_1 -> 1+P2_1 = 1+(P2_0+5 mod 10)
% S1_2 -> S1_1 + 1+P1_2 = 1+P1_1 + 1+P1_2
%       = 1+((P1_0+6)+4 mod 10) + 1+(P1_0+6 mod 10)
%       = 2 + (P1_0+6 mod 10) + ((P1_0+6)+4 mod 10)
%       = 2 + (P1_0+6 mod 10) + (P1_0+0 mod 10)

det100(1, S, [S0,S1,S2,S3,S4,S5,S6,S7,S8,S9]) :-
	S0 is (S+6) mod 10, S1 is (S+0) mod 10, S2 is (S+2) mod 10, S3 is (S+2) mod 10, S4 is (S+0) mod 10,
	S5 is (S+6) mod 10, S6 is (S+0) mod 10, S7 is (S+2) mod 10, S8 is (S+2) mod 10, S9 is (S+0) mod 10.
det100(2, S, [S0,S1,S2,S3,S4,S5,S6,S7,S8,S9]) :-
	S0 is (S+5) mod 10, S1 is (S+8) mod 10, S2 is (S+9) mod 10, S3 is (S+8) mod 10, S4 is (S+5) mod 10,
	S5 is (S+0) mod 10, S6 is (S+3) mod 10, S7 is (S+4) mod 10, S8 is (S+3) mod 10, S9 is (S+0) mod 10.

sums(Vs, Ss) :- sums(Vs, 0, Ss).
sums([], _, []).
sums([V|Vs], P, [S|Ss]) :- S is V + P + 1, sums(Vs, S, Ss).

first_gte(Vs, Search, N, V) :-
	first_gte(0, Vs, Search, N, V).
first_gte(I, [V|_], Search, I, V) :- V >= Search.
first_gte(I, [V|Vs], Search, N, Vo) :-
	V < Search,
	I1 is I + 1,
	first_gte(I1, Vs, Search, N, Vo).

answer1(N) :- answer1(N, _, _, _, _, _, _).
answer1(N, S1, S2, S1s, S2s, Rolls, LoseScore) :-
	input(P1, P2),
	% P1 = 3, P2 = 7,

	det100(1, P1, P1vs), sums(P1vs, S1s),
	det100(2, P2, P2vs), sums(P2vs, S2s),
	append(_, [S1], S1s),
	append(_, [S2], S2s),
	format("P1: ~w ~w ~w~n", [P1, S1s, S1]),
	format("P2: ~w ~w ~w~n", [P2, S2s, S2]),

	(	S1 > S2,
		Ws = S1, Wsums = S1s,
		Ls = S2, Lsums = S2s, LTurnSub = 1
	;	S1 =< S2,
		Ws = S2, Wsums = S2s,
		Ls = S1, Lsums = S1s, LTurnSub = 0
	),

	% choose N such that
	% 10|N                                N/10 = M
	% and N+(N/10 * Ws) =< 1000           M*10+(M*Ws) =< 1000
	% and (N+10)+((N+10)/10 * Ws) > 1000  (M+1)*10+((M+1)*Ws) > 1000
	% M * (10+Ws) =< 1000 < (M+1) * (10+Ws)
	% M = 1000 div (10+Ws)
	TurnDiv10 is 1000 div Ws, TurnTail is 1000 mod Ws,
	first_gte([0|Wsums], TurnTail, TurnMod10, TurnVal),
	format("W ~w ~w ~w ~w~n", [TurnDiv10, TurnTail, TurnMod10, TurnVal]),
	% N rounds * 2 players * 3 rolls/player/round = M rolls
	Rounds is TurnDiv10 * 10 + TurnMod10,
	LRounds is Rounds - LTurnSub,
	Rolls is Rounds*3 + LRounds*3,
	LTurnDiv10 is LRounds div 10, LTurnMod10 is LRounds mod 10,
	append(LosePre, [LTurnVal|_], [0|Lsums]), length(LosePre, LTurnMod10),
	LoseScore is LTurnDiv10 * Ls + LTurnVal,
	N is Rolls * LoseScore.

