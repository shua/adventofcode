:- use_module('../util.pl').
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(format)).

sample_src(1, "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]").
sample_src(2, "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"). % (the 2 has no regular number to its right, and so it is not added to any regular number).
sample_src(3, "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]").
sample_src(4, "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"). % (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).
sample_src(5, "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]").
sample_src(6, "[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").
sample_src(7, "[1,1]\n[2,2]\n[3,3]\n[4,4]", "[[[[1,1],[2,2]],[3,3]],[4,4]]").
sample_src(8, "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]", "[[[[3,0],[5,3]],[4,4]],[5,5]]").
sample_src(9, "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]", "[[[[5,0],[7,4]],[5,5]],[6,6]]").
sample_src(10, In, "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") :-
	append([
		"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n",
		"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n",
		"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n",
		"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n",
		"[7,[5,[[3,8],[1,4]]]]\n",
		"[[2,[2,2]],[8,[8,1]]]\n",
		"[2,9]\n",
		"[1,[[[9,3],9],[[9,0],[0,7]]]]\n",
		"[[[5,[7,4]],7],1]\n",
		"[[[[4,2],2],6],[8,7]]\n"
	], In).
sample_src(10-1, "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]","[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]").
sample_src(10-2, "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]","[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]").
sample_src(10-3, "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]","[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]").
sample_src(10-4, "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]\n[7,[5,[[3,8],[1,4]]]]","[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]").
sample_src(10-5, "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]\n[[2,[2,2]],[8,[8,1]]]","[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]").
sample_src(10-6, "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]\n[2,9]","[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]").
sample_src(10-7, "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]\n[1,[[[9,3],9],[[9,0],[0,7]]]]","[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]").
sample_src(10-8, "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]\n[[[5,[7,4]],7],1]","[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]").
sample_src(10-9, "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]\n[[[[4,2],2],6],[8,7]]","[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").

sample(N, In, Expected) :-
	sample_src(N, InSrc, ExpSrc),
	append(InSrc, "\n\n", InSrc1),
	append(ExpSrc, "\n\n", ExpSrc1),
	phrase((snums(In), ws(_)), InSrc1),
	phrase((snums([Expected]), ws(_)), ExpSrc1).
% intermediary values
sample(6-N, In, Expected) :-
	Exps = [
	/*after addition*/"[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]",
	/*after explode*/ "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]",
	/*after explode*/ "[[[[0,7],4],[15,[0,13]]],[1,1]]",
	/*after split*/   "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]",
	/*after split*/   "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]",
	/*after explode*/ "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
	],
	append(Pre, [ExpSrc|_], Exps), length(Pre, N),
	append(ExpSrc, "\n\n", ExpSrc1),
	phrase((snums([Expected]), ws(_)), ExpSrc1),
	sample(6, In, _).

/*
The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number.

For example, the magnitude of [9,1] is 3*9 + 2*1 = 29; the magnitude of [1,9] is 3*1 + 2*9 = 21. Magnitude calculations are recursive: the magnitude of [[9,1],[1,9]] is 3*29 + 2*21 = 129.
*/

snum(I) --> uint(I).
snum([A,B]) --> "[", snum(A), ",", snum(B), "]".
snums([]) --> "\n".
snums([N|Ns]) --> snum(N), "\n", snums(Ns).
input(Ns) :- phrase_from_input(snums(Ns)).

/*
This snailfish homework is about addition. To add two snailfish numbers, form a pair from the left and right parameters of the addition operator. For example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].

There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.

To reduce a snailfish number, you must repeatedly do the first action in this list that applies to the snailfish number:

    If any pair is nested inside four pairs, the leftmost such pair explodes.
    If any regular number is 10 or greater, the leftmost such regular number splits.

To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.

To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.
*/
sadd(A, B, [A,B]).
ssum([A], Ar) :- sreduce(A, Ar).
ssum([A,B|As], Res) :- sadd(A, B, AB), sreduce(AB, ABr), ssum([ABr|As], Res).

sreduce_iter(0, Sn, Sn).
sreduce_iter(N, In, Out) :-
	N > 0, N1 is N - 1,
	sreduce(0, In, Exp, Cur),
	format("~w ~w ~w~n", [N, Cur, Exp]),
	sreduce_iter(N1, Cur, Out).
sreduce(I, I) :- integer(I).
sreduce([A,B], R) :-
	sreduce(0, [A,B], Exp, R1),
	(	Exp = none,
		R = R1
	;	(Exp = exp(_,_) ; Exp = split),
		sreduce(R1, R)
	).
sreduce(4, [A,B], exp(some(A), some(B)), 0).
sreduce(Lvl, I, none, I) :-
	Lvl >= 0, Lvl =< 4, integer(I), I < 10.
sreduce(Lvl, I, split, [Il, Ir]) :-
	Lvl >= 0, Lvl =< 4, integer(I), I >= 10,
	Im is I mod 2, Il is I div 2, Ir is Il + Im.
sreduce(Lvl, [A,B], Exp, R) :-
	Lvl >= 0, Lvl < 4, L1 is Lvl + 1,
	sreduce(L1, A, Exp1, R1),
	(	Exp1 = exp(El, some(Er)),
		Exp = exp(El, none),
		sinsert(left, B, Er, R2),
		R = [R1, R2]
	;	(Exp1 = exp(_, none) ; Exp1 = split),
		Exp = Exp1,
		R = [R1, B]
	;	Exp1 = none, A = R1, % assert A is unchanged
		sreduce(L1, B, Exp2, R2),
		(	Exp2 = exp(some(El), Er),
			Exp = exp(none, Er),
			sinsert(right, A, El, R3),
			R = [R3, R2]
		;	(Exp2 = exp(none, _) ; Exp2 = split ; Exp = none),
			Exp = Exp2,
			R = [A, R2]
		)
	).
sinsert(_, I, V, R) :-
	integer(I), R is I + V.
sinsert(left, [A,B], V, [R,B]) :-
	sinsert(left, A, V, R).
sinsert(right, [A,B], V, [A,R]) :-
	sinsert(right, B, V, R).

slist(Sn, Sl) :-
	nonvar(Sn), slist_from(0, Sn, [], Sl).
slist(Sn, Sl) :-
	var(Sn), slist_to(0, Sl, [], Sn).
slist_from(L, I, Post, [L-I|Post]) :- integer(I).
slist_from(L, [A,B], Post, Sl) :-
	L1 is L + 1,
	slist_from(L1, B, Post, B1),
	slist_from(L1, A, B1, Sl).
slist_to(L, [Ll-V|Ls], Rest, [A,B]) :-
	Ll > L, L1 is L + 1,
	slist_to(L1, [Ll-V|Ls], Rl, A),
	slist_to(L1, Rl, Rest, B).
slist_to(L, [L-A|Ls], Ls, A).

slists([], []).
slists([Sn|Sns], [Sl|Sls]) :-
	slist(Sn, Sl), slists(Sns, Sls).


slreduce(Sl, Sr) :-
	slreduce([], Sl, Sr).
slreduce(Pre, [], Sr) :-
	reverse(Pre, Sr).
slreduce(Pre, [L-V|Ls], Sr) :-
	L < 5, V < 10,
	slreduce([L-V|Pre], Ls, Sr).
slreduce(Pre, [L-V1,L-V2|Ls], Sr) :-
	L = 5,
	(	Pre = [],
		(	Ls = [Lb-Vb|Lbs],
			Vb1 is Vb + V2,
			slreduce([], [4-0,Lb-Vb1|Lbs], Sr)
		;	Ls = [],
			format("Both pre and post are empty, this should never happen~n", []),
			fail
		)
	;	Pre = [La-Va|Las], Va1 is Va + V1,
		(	Ls = [Lb-Vb|Lbs], Vb1 is Vb + V2,
			slreduce(Las, [La-Va1,4-0,Lb-Vb1|Lbs], Sr)
		;	Ls = [],
			slreduce(Las, [La-Va1,4-0], Sr)
		)
	).
slreduce(Pre, [L-V|Post], Sr) :-
	L < 5, V >= 10,
	L1 is L + 1, Vm is V mod 2, Va is V div 2, Vb is Va + Vm,
	slreduce(Pre, [L1-Va,L1-Vb|Post], Sr).

incrkey([], []).
incrkey([K-V|KVs], [K1-V|KV1s]) :- K1 is K + 1, incrkey(KVs, KV1s).
sladd(V1, V2, V3) :-
	incrkey(V1, V1a),
	incrkey(V2, V2a),
	append(V1a, V2a, V3).
slsum([V], V).
slsum([V1,V2|Vs], Out) :-
	sladd(V1, V2, V3),
	slreduce(V3, V3r),
	slsum([V3r|Vs], Out).

