:- use_module('../util.pl').
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(time)).

onoff(1) --> "on".
onoff(0) --> "off".

int(I) --> "-", uint(In), { I is -In }.
int(I) --> uint(I).

bound([Lo,Hi]) --> int(Lo), "..", int(Hi).

line([X-[Y-[Z-IO]]]) --> onoff(IO), " x=", bound(X), ",y=", bound(Y), ",z=", bound(Z). 

lines([T|Ls]) --> line(T), "\n", lines(Ls).
lines([]) --> [].

input(Ls) :- phrase_from_input((lines(Ls), "\n")).

% can we store intervals as a tree?
% ^  ##
% | ###  2..3,2..3  t(2..3,n(2..3))
% | ##   3..4,3..4  t(2..2,n(2..3)),t(3..3,n(2..4)),t(4..4,n(3..4))
% |   #  4..4,1..1  t(2..2,n(2..3)),t(3..3,n(2..4)),t(4..4,n(1..1),n(3..4))
% 0--->

%  []           range contains
%      []       range does not contain
%  [...]        range intersects 1 min
%      [...]    range intersects 1 max
%   [......]    range intersects 2
% [..]    [..]

norm([], []).
norm([N], [N]).
norm([_-0|Ts], Tn) :-
	norm(Ts, Tn).
norm([[Min0,Max0]-S0, [Min1,Max1]-S1 | T], TOut) :-
	(S0 = [_|_] ; S0 = 1),
	(	Min1 =:= Max0+1, S0 = S1
	->	norm([[Min0,Max1]-S0|T], TOut)
	;	norm([[Min1,Max1]-S1|T], TOut1),
		TOut = [[Min0,Max0]-S0|TOut1]
	).

merge(0, 1, 1).
merge(0, 0, 0).
merge(1, 1, 1).
merge(1, 0, 0).
merge([], T2, T2).
merge(T1, [], T1) :- T1 = [_|_].
merge(T0, T1, Tn) :-
	T0 = [_|_], T1 = [_|_],
	format("merge(~w, ~w, ?)~n", [T0, T1]),
	merge_(T0, T1, To),
	format("norm(~w, ?)~n", [To]),
	norm(To, Tn).

merge_([[L,R]-S0|T0], [[L,R]-S1|T1], [[L,R]-S2|T2]) :-
	% subtree merge, advance both
	merge(T0, T1, T2),
	merge(S0, S1, S2).
merge_([[L0,R0]-S0|T0], [[L1,R1]-S1|T1], Tn) :-
	R1 < L0,
	% advance right
	merge([[L0,R0]-S0|T0], T1, T2),
	Tn = [[L1,R1]-S1|T2]
;	R0 < L1,
	% advance left
	merge(T0, [[L1,R1]-S1|T1], T2),
	Tn = [[L0,R0]-S0|T2]
;	R1 >= L0, R0 >= L1,
	% split and retry
	(L1 > L0 ; L1 < L0 ; L1=L0, R1 > R0 ; L1=L0, R1 < R0),
	split([L0,R0], [L1,R1], S0, S1, T0, T1, T0b, T1b),
	merge(T0b, T1b, Tn).

split(B0, B1, S0, S1, T0, T1, T0o, T1o) :-
	%format("split(~w, ~w, ?)~n", [B0, B1]),
	split_(B0, B1, S0, S1, T0, T1, T0o, T1o).
split_(B, B, S0, S1, T0, T1, [B-S0|T0], [B-S1|T1]).
split_(
	[L,R0], [L,R1], S0, S1, T0, T1,
	[[L,R0]-S0|T0], [[L,R0]-S1,[Cut,R1]-S1|T1]
) :- R0 < R1, Cut is R0 + 1.
split_(
	[L,R0], [L,R1], S0, S1, T0, T1,
	[[L,R1]-S0,[Cut,R0]-S0|T0], [[L,R0]-S1|T1]
) :- R0 > R1, Cut is R1 + 1.
split_(
	[L0,R0], [L1,R1], S0, S1, T0, T1,
	[[L0,Cut]-S0|T0b], T1b
) :-
	L0 < L1, Cut is L1 - 1,
	split([L1,R0], [L1,R1], S0, S1, T0, T1, T0b, T1b).
split_(
	[L0,R0], [L1,R1], S0, S1, T0, T1,
	T0b, [[L1,Cut]-S1|T1b]
) :-
	L0 > L1, Cut is L0 - 1,
	split([L0,R0], [L0,R1], S0, S1, T0, T1, T0b, T1b).

iter_merge(Ts, T, Rest) :- iter_merge(Ts, [], T, Rest).
iter_merge([], Acc, Acc, 0).
iter_merge(In, Acc, Acc, N) :- integer(N), N>0, length(In, N).
iter_merge([T|Ts], Acc, Tn, Rest) :-
	(integer(Rest), length([T|Ts], N), N > Rest ; var(Rest)),
	!,
	merge(Acc, T, Tc),
	iter_merge(Ts, Tc, Tn, Rest).

% I don't know, still pretty slow
% maybe use an octree split instead?
% I don't know how complicated that split is though...

answer1(_) :- fail.

answer2(_) :- fail.

