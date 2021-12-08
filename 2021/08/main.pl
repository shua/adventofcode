:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).

letter(a) --> "a".
letter(b) --> "b".
letter(c) --> "c".
letter(d) --> "d".
letter(e) --> "e".
letter(f) --> "f".
letter(g) --> "g".

word([C|Cs]) --> letter(C), word(Cs).
word([C]) --> letter(C).

words([W|Ws]) --> word(W), " ", words(Ws).
words([W]) --> word(W).

input_lines([Inputs-Outputs|Ls]) --> words(Inputs), " | ", words(Outputs), "\n", input_lines(Ls).
input_lines([]) --> [].

input(Lines) :- phrase_from_input((input_lines(Lines), ws(_))).

distinct_combos([], 0).
distinct_combos([_-[]|Lines], N) :- distinct_combos(Lines, N).
distinct_combos([IWs-[W|OWs]|Lines], N) :-
	( length(W, 2) ; length(W, 4) ; length(W, 3) ; length(W, 7) ),
	distinct_combos([IWs-OWs|Lines], N0),
	N is N0 + 1
;	distinct_combos([IWs-OWs|Lines], N).

answer1(N) :-
	input(Lines),
	distinct_combos(Lines, N).

% aaa
% b c
% ddd
% e f
% ggg
segs(0, [a, b, c, e, f, g]).
segs(1, [c, f]).
segs(2, [a, c, d, e, g]).
segs(3, [a, c, d, f, g]).
segs(4, [b, c, d, f]).
segs(5, [a, b, d, f, g]).
segs(6, [a, b, d, e, f, g]).
segs(7, [a, c, f]).
segs(8, [a, b, c, d, e, f, g]).
segs(9, [a, b, c, d, f, g]).

segs_length(2, [1]).
segs_length(3, [7]).
segs_length(4, [4]).
segs_length(5, [2,3,6]).
segs_length(6, [0,6,9]).
segs_length(7, [8]).

% a, ab, abc, abd, c
% b, ba, c
% length("b", 1) -> is(b, a) ; is(b, c)
% length("ba", 2) -> (is(b, a), is(a, b)) ; (is(a, a), is(b, b))
% length("c", 1) -> is(c, a) ; is(c, c)

eq_except(L1, L2, V1, V2) :-
	append(Pre, [V1|Post], L1),
	append(Pre, [V2|Post], L2).

key([], []).
key([K|Ks], [K-1|KVs]) :- key(Ks, KVs).

clump(Sorted, Clumped) :- clump(Sorted, [], Clumped).
clump([], Clumped, Clumped).
clump([K-N1|Sorted], [K-N2|Cur], Clumped) :-
	N is N1 + N2,
	clump(Sorted, [K-N|Cur], Clumped).
clump([K-N|Sorted], [], Clumped) :-
	clump(Sorted, [K-N], Clumped).
clump([K1-N1|Sorted], [K2-N2|Cur], Clumped) :-
	K1 \= K2,
	clump(Sorted, [K1-N1,K2-N2|Cur], Clumped).

filter2([], []).
filter2([K-N|M], [K|Out]) :- N >= 2, filter2(M, Out).
filter2([_-N|M], Out) :- N < 2, filter2(M, Out).

intersection(S1, S2, Int) :-
	append(S1, S2, All),
	key(All, Keyed),
	keysort(Keyed, Sorted),
	clump(Sorted, Clumped),
	filter2(Clumped, Int).

constrain([], _, M, M).
constrain([C1|W], Cs, MIn, MOut) :-
	eq_except(MIn, MCur, C1-C1In, C1-C1Out),
	C1Out = [_|_],
	intersection(C1In, Cs, C1Out),
	constrain(W, Cs, MCur, MOut).
constrain(W, MIn, MOut) :-
	length(W, N),
	segs(_, Cs),
	length(Cs, N),
	constrain(W, Cs, MIn, MOut).

it(_, 0, M, M).
it([W|Ws], N, MIn, MOut) :-
	N > 0,
	constrain(W, MIn, MCur),
	N1 is N - 1,
	it(Ws, N1, MCur, MOut).
it([W|Ws], N, M) :-
	All = "abcdefg",
	M0 = [a-All, b-All, c-All, d-All, e-All, f-All, g-All],
	it([W|Ws], N, M0, M).

% imagine a tree with children l0 = 6, l1 = 5, l2 = 4, ...
% you'll have 6! nodes or 720 nodes? not that much
% shouldn't that be not that much calculation?

% I give up, brute forcing it...
choose([C|Cs], Chosen, Rest) :-
	C = Chosen, Rest = Cs
;	choose(Cs, Chosen, Rest0), Rest = [C|Rest0].

permute([a-A, b-B, c-C, d-D, e-E, f-F, g-G]) :-
	choose("abcdefg", A, MA),
	choose(MA, B, MB),
	choose(MB, C, MC), 
	choose(MC, D, MD),
	choose(MD, E, ME),
	choose(ME, F, [G]).

inverse_map([], []).
inverse_map([K-V|KVs], [V-K|VKs]) :-
	inverse_map(KVs, VKs).

decode([], _, []).
decode([C|Cs], M, [D|Ds]) :-
	member(C-D, M),
	decode(Cs, M, Ds).

decode_words([], _, []).
decode_words([W|Ws], M, [S|Ss]) :-
	length(W, N), length(Seg, N),
	segs(S, Seg),
	decode(W, M, D),
	sort(D, Seg),
	decode_words(Ws, M, Ss).

sort_best_guess(Ws, Sorted) :-
	sort_best_guess(Ws, Easy, Hard),
	sort(Easy, EasyS), sort(Hard, HardS),
	append(EasyS, HardS, Sorted).
sort_best_guess([], [], []).
sort_best_guess([W|Ws], Easy, Hard) :-
	sort_best_guess(Ws, Easy0, Hard0),
	length(W, N), segs_length(N, Choices),
	length(Choices, CN),
	sort(W, WS),
	(	CN = 1, Easy = [WS|Easy0], Hard = Hard0
	;	CN > 1, Easy = Easy0, Hard = [WS|Hard0]
	)
	.

decode_lines([], []).
decode_lines([In-Out|Ls], [D|Ds]) :-
	time((
		permute(M),
		% sort_best_guess(In, In2),
		% sort_best_guess(Out, Out2),
		decode_words(In, M, _),
		decode_words(Out, M, D)
	)),
	decode_lines(Ls, Ds).

uint(Ds, I) :- uint(Ds, _, I).
uint([D], 1, D).
uint([D|Ds], E, I) :-
	uint(Ds, E0, I0),
	E is E0 * 10,
	I is I0 + (D*E).
uints([], []).
uints([Ds|Dss], [I|Is]) :-
	uint(Ds, I),
	uints(Dss, Is).

answer2(N, Decoded, Is) :-
	input(Lines), 
	decode_lines(Lines, Decoded),
	uints(Decoded, Is),
	sum(Is, N).

