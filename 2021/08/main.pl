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

% maybe better approach would be something like C in segs(7) & C nin segs(1) -> C = a

segsb(0, [1,1,1,0,1,1,1]).
segsb(1, [0,0,1,0,0,1,0]).
segsb(2, [1,0,1,1,1,0,1]).
segsb(3, [1,0,1,1,0,1,1]).
segsb(4, [0,1,1,1,0,1,0]).
segsb(5, [1,1,0,1,0,1,1]).
segsb(6, [1,1,0,1,1,1,1]).
segsb(7, [1,0,1,0,0,1,0]).
segsb(8, [1,1,1,1,1,1,1]).
segsb(9, [1,1,1,1,0,1,1]).

not([], []).
not([0|Bs], [1|Bns]) :- not(Bs, Bns).
not([1|Bs], [0|Bns]) :- not(Bs, Bns).

band([], [], []).
band([0|B1s], [_|B2s], [0|Bas]) :- band(B1s, B2s, Bas).
band([1|B1s], [0|B2s], [0|Bas]) :- band(B1s, B2s, Bas).
band([1|B1s], [1|B2s], [1|Bas]) :- band(B1s, B2s, Bas).

ones([], 0).
ones([1|Bs], N1) :- ones(Bs, N), N1 is N + 1.
ones([0|Bs], N) :- ones(Bs, N).

keybs(Cs, Bs, Map) :-
	append(Cs, "abcdefg", C1),
	counts(C1, Cts),
	not(Bs, Nbs),
	keybs(Cts, Bs, Nbs, Map).
keybs([], _, _, []).
keybs([C-2|Cts], Bs, Nbs, [C-Bs|Cout]) :-
	keybs(Cts, Bs, Nbs, Cout).
keybs([C-1|Cts], Bs, Nbs, [C-Nbs|Cout]) :-
	keybs(Cts, Bs, Nbs, Cout).

mand([], [], []).
mand([C-B1|C1s], [C-B2|C2s], [C-Bo|Cos]) :-
	band(B1, B2, Bo),
	mand(C1s, C2s, Cos).

decode1(W, MapOut) :-
	segsb(8, AllOn),
	keybs("abcdefg", AllOn, Map),
	decode1(W, Map, MapOut).
decode1(W, Map, MapOut) :-
	segsb(_, Bs), ones(Bs, N), length(W, N),
	keybs(W, Bs, WMap),
	mand(WMap, Map, MapOut).

mapvalid([]).
mapvalid([_-Bs|M]) :-
	ones(Bs, N), N > 0,
	mapvalid(M).

mapexact([]).
mapexact([_-Bs|M]) :-
	ones(Bs, 1),
	mapexact(M).

decoden(Ws, MapOut) :-
	segsb(8, AllOn),
	keybs("abcdefg", AllOn, Map),
	decoden(Ws, Map, MapOut).
decoden([], Map, Map).
decoden([W|Ws], Map, MapOut) :-
	decode1(W, Map, MapCur),
	mapvalid(MapCur),
	decoden(Ws, MapCur, MapOut).

key_length([], []).
key_length([W|Ws], [N-W|KVs]) :-
	length(W, Wn),
	(	(Wn = 2 ; Wn = 3 ; Wn = 4 ; Wn = 7), N = 1
	;	(Wn = 5 ; Wn = 6), N = 2
	),
	key_length(Ws, KVs).

sort_preferred_first(Ws, Sorted) :-
	key_length(Ws, KVs),
	keysort(KVs, KSorted),
	key_length(Sorted, KSorted).

map_bs_char([], []).
map_bs_char([C-[1,0,0,0,0,0,0]|M], [C-'a'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,1,0,0,0,0,0]|M], [C-'b'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,0,1,0,0,0,0]|M], [C-'c'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,0,0,1,0,0,0]|M], [C-'d'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,0,0,0,1,0,0]|M], [C-'e'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,0,0,0,0,1,0]|M], [C-'f'|Mo]) :- map_bs_char(M, Mo).
map_bs_char([C-[0,0,0,0,0,0,1]|M], [C-'g'|Mo]) :- map_bs_char(M, Mo).

decode_lines1([], []).
decode_lines1([In-Out|Ls], [Dec|Dls]) :-
	sort_preferred_first(In, InSrt),
	decoden(InSrt, MBs),
	map_bs_char(MBs, M),
	decode_words(Out, M, Dec),
	decode_lines1(Ls, Dls).

answer2b(N) :- time(answer2b(N, _, _, _)).
answer2b(N, Lines, Decoded, Is) :-
	input(Lines),
	time(decode_lines1(Lines, Decoded)),
	time(uints(Decoded, Is)),
	sum(Is, N).
	
