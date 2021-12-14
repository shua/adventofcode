:- use_module('../util.pl').
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

rules([From-To|Rules]) --> seq(From), " -> ", seq(To), "\n", {!}, rules(Rules).
rules([]) --> "\n".
input(Init, Rules) --> seq(Init), "\n\n", rules(Rules).

input(Init, Rules) :- phrase_from_input(input(Init, Rules)).

apply_rules1([C1,C2|In], Rules, [C1,C|Out]) :-
	member([C1,C2]-[C], Rules),
	apply_rules1([C2|In], Rules, Out).
apply_rules1([C], _, [C]).

apply_rulesn(N, In, Rules, Out) :-
	N > 0,
	N1 is N - 1,
	apply_rules1(In, Rules, Cur),
	apply_rulesn(N1, Cur, Rules, Out).
apply_rulesn(0, Cur, _, Cur).

min_max_values([K-V], K-V, K-V).
min_max_values([K-V|KVs], KMin-VMin, KMax-VMax) :-
	min_max_values(KVs, KMin0-VMin0, KMax0-VMax0),
	(	V > VMax0, KMax-VMax = K-V, KMin-VMin = KMin0-VMin0
	;	V < VMin0, KMin-VMin = K-V, KMax-VMax = KMax0-VMax0
	;	V =< VMax0, V >= VMin0, [KMin-VMin,KMax-VMax] = [KMin0-VMin0,KMax0-VMax0]
	).

answer1(N) :- answer1(N, _,_,_,_,_).
answer1(N, Init, Rules, S10, KMin-Min, KMax-Max) :-
	input(Init, Rules),
	time(apply_rulesn(10, Init, Rules, S10)),
	counts(S10, Cts),
	min_max_values(Cts, KMin-Min, KMax-Max),
	N is Max - Min.

% naive iteration takes 3-4s for N=10, and ??? for N=40
% can we do bottom-up, like fib and calc N-1, N-2 to get N?
% expand(40, [C1,C2], Out)
% expand(1, [C1,C2], [C1,C,C2]), expand(39, [C1,C], Out1), expand(39, [C,C2], Out2), append(Out1, Out2, Out)
% only thing I can think of is to memoize and pass around a map of expand(N, C1, C2, Cs)

rules_mem([], []).
rules_mem([[C1,C2]-[C]|Rules], [[C1,C2]-1-[C1,C,C2]|Out]) :-
	rules_mem(Rules, Out).
apply_rules(N, Cs, Rules, Out) :-
	rules_mem(Rules, Mem),
	apply_rules_mem(Cs-N, Mem, _, Out).

apply_rules_mem([C1,C2,C3|Cs]-N, Mem, Out, Exp) :-
	apply_rules_mem([C1,C2]-N, Mem, Cur, Exp0),
	apply_rules_mem([C2,C3|Cs]-N, Cur, Out, Expn),
	append(Exp0, Expn, Exp).
apply_rules_mem([C1,C2]-N, Mem, Mem, Exp) :- member([C1,C2]-N-Exp, Mem), !.
apply_rules_mem([C1,C2]-N, Mem, [[C1,C2]-N-Exp|Out], Exp) :-
	N > 1,
	N1 is N - 1,
	member([C1,C2]-1-[C1,C,C2], Mem),
	apply_rules_mem([C1,C]-N1, Mem, Mem1, Exp1),
	apply_rules_mem([C,C2]-N1, Mem1, Out, Exp2),
	append(Exp1, Exp2, Exp).

% wait, it about doubles with every iter, so 2^40 is going to be quite enormous
% also, we just want the *counts* of every symbol, not the actual string
%
% still taking too long, maybe memoize counts?

app_cts(_, [C], _, [C-1]).
app_cts(0, [C|Cs], _, [C-1|Cts]) :- app_cts(0, Cs, [], Cts).
app_cts(N, [C1,C2|Cs], Rules, Cts) :-
	N > 0, N1 is N - 1,
	member([C1,C2]-[C], Rules),
	app_cts(N, [C2|Cs], Rules, CtsIter),
	app_cts(N1, [C1,C,C2], Rules, CtsRecur),
	append(CtsIter, [C2-(-1)|CtsRecur], Cts0),
	clump(Cts0, Cts).

answer2(N) :- answer2(N, _,_,_,_,_).
answer2(N, Init, Rules, S40, KMin-Min, KMax-Max) :-
	input(Init, Rules),
	time(apply_rulesn(40, Init, Rules, S40)),
	counts(S40, Cts),
	min_max_values(Cts, KMin-Min, KMax-Max),
	N is Max - Min.


