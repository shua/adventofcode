:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(time)).
:- use_module(library(dcgs)).
:- use_module(library(format)).


hexit([0,0,0,0]) --> "0".
hexit([0,0,0,1]) --> "1".
hexit([0,0,1,0]) --> "2".
hexit([0,0,1,1]) --> "3".
hexit([0,1,0,0]) --> "4".
hexit([0,1,0,1]) --> "5".
hexit([0,1,1,0]) --> "6".
hexit([0,1,1,1]) --> "7".
hexit([1,0,0,0]) --> "8".
hexit([1,0,0,1]) --> "9".
hexit([1,0,1,0]) --> "A".
hexit([1,0,1,1]) --> "B".
hexit([1,1,0,0]) --> "C".
hexit([1,1,0,1]) --> "D".
hexit([1,1,1,0]) --> "E".
hexit([1,1,1,1]) --> "F".

hexits([]) --> "\n".
hexits(Hs) --> hexit(H), hexits(Hs0), { append(H, Hs0, Hs) }.

input(Bs) :-
	phrase_from_input((hexits(Bs), "\n")).

sample(1, Bs) :- phrase(hexits(Bs), "D2FE28\n").
sample(2, Bs) :- phrase(hexits(Bs), "38006F45291200\n").
sample(3, Bs) :- phrase(hexits(Bs), "EE00D40C823060\n").
sample(4, Bs) :- phrase(hexits(Bs), "8A004A801A8002F478\n").
sample(5, Bs) :- phrase(hexits(Bs), "620080001611562C8802118E34\n").
sample(6, Bs) :- phrase(hexits(Bs), "C0015000016115A2E0802F182340\n").
sample(7, Bs) :- phrase(hexits(Bs), "A0016C880162017C3686B18A3D4780\n").


/*
 * every packet begins with a standard header: 
 * V = version, T = TypeID
 *
 *   VVVTTT...
 *
 * TypeID 4 - literal value
 *         1XXXX1XXXX...0XXXX
 *   VVV100AAAAABBBBB...NNNNN
 * every XXXX can be enccoded back to a number
 *
 * TypeID != 4 - operator packet
 *   VVVTTT0LLLLLLLLLLLLLLL...
 *   VVVTTT1LLLLLLLLLLL...
 * I := mode indicator
 *   0 -> next 15 bits decode to size in bits of sub-packets
 *   1 -> next 11 bits decode to number of immediate sub-packets
 */

binnum([B], 1, B).
binnum([B|Bs], E, N) :-
	binnum(Bs, E0, N0),
	E is E0 * 2,
	N is N0 + E*B.
binnum(Bs, N) :- binnum(Bs, _, N).

fmt_indent(0).
fmt_indent(Lvl) :- Lvl > 0, format("  ", []), Lvl1 is Lvl - 1, fmt_indent(Lvl1).
fmt_level(Lvl, Fmt, Args) :- fmt_indent(Lvl), format(Fmt, Args).


place(X), [X] --> [].
peek(X), [X] --> [X].

st_init(s(0, 0)).
st_addz(s(Z, Lvl), N, s(Zo, Lvl)) :- Zo is Z + N.
st_getz(s(Z, _), Z).
st_getlvl(s(_, Lvl), Lvl).
st_inclvl(s(Z, Lvl), s(Z, Lvl1)) :- var(Lvl1) -> Lvl1 is Lvl + 1 ; Lvl is Lvl1 - 1.


packet_hdr(Version, TypeID), [S] -->
	[S0],
	{ Vbs = [_,_,_], Tbs = [_,_,_] },
	Vbs, Tbs,
	{	binnum(Vbs, Version), binnum(Tbs, TypeID),
		st_addz(S0, 6, S)
	}.

packet_data(4, Data), [S] -->
	[S0],
	collect_term_bs(Bs, Zc),
	{ binnum(Bs, Data), st_addz(S0, Zc, S) },
	{ format("\"d\":~w},~n", [Data]) }.
packet_data(T, Data) -->
	{ T > 4 ; T < 4 },
	[S0, I],
	packet_op(S0, I, Data).
packet_op(S0, 0, Data) -->
	seqn(15, Ls),
	% add 15 + 1 to account for I
	{ binnum(Ls, Z), st_addz(S0, 16, S) },
	{ format("\"z\":~w, \"d\":[~n", [Z]) },
	place(S),
	packetz(Z, Data),
	{ st_getlvl(S, Lvl), L1 is Lvl - 1, fmt_level(L1, "]}~n", []) }.
packet_op(S0, 1, Data) -->
	seqn(11, Ls),
	% add 11 + 1 to account for I
	{ binnum(Ls, N), st_addz(S0, 12, S) },
	{ format("\"n\":~w, \"d\":[~n", [N]) },
	place(S),
	packetn(N, Data),
	{ st_getlvl(S, Lvl), L1 is Lvl - 1, fmt_level(L1, "]}~n", []) }.

packetz(0, []) --> [].
packetz(Z, []) -->
	{ Z =< 6, Z > 0 },
	[S0],
	{ st_addz(S0, Z, S) },
	pad(Z),
	% { length(Cs, Z) }, Cs, { st_getlvl(S0, L), fmt_level(L, "# REMAINING ~w~n", [Cs]) },
	place(S).
packetz(Z, [P|Ps]) -->
	{ Z > 6 },
	peek(S0), { st_getz(S0, Z0) },
	packet(P),
	peek(S1), { st_getz(S1, Z1) },
	{ Zn is Z0 + Z - Z1 },
	{	st_getlvl(S1, L),
		fmt_level(L, "# ~w prev ~w pend ~w left~n", [Z0, Z1, Zn]) },
	packetz(Zn, Ps).
packetn(0, []) --> [].
packetn(N, [P|Ps]) -->
	{ N > 0, N1 is N - 1 },
	packet(P),
	packetn(N1, Ps).

packet(P, Z) -->
	{ st_init(S) },
	place(S), packet(P), [Se],
	{ st_getz(Se, Z) }.
packet(p(Version, TypeID, Data)) -->
	packet_hdr(Version, TypeID),
	[S0],
	{	st_getlvl(S0, Lvl),
		fmt_level(Lvl, "{\"t\":~w, \"v\":~w, \"st\":\"~w\", ", [TypeID, Version, S0]),
		st_inclvl(S0, Sd)
	},
	place(Sd),
	packet_data(TypeID, Data),
	[Sn], { st_inclvl(S, Sn) }, place(S).

seqn(0, []) --> [].
seqn(N, [C|Cs]) --> [C], { N1 is N - 1 }, seqn(N1, Cs).

collect_term_bs([B1,B2,B3,B4|Bs], Z) -->
	[1,B1,B2,B3,B4],
	collect_term_bs(Bs, Zn),
	{ Z is Zn + 5 }.
collect_term_bs([B1,B2,B3,B4], 5) -->
	[0,B1,B2,B3,B4].

pad(0) --> [].
pad(Z) --> { var(Z) }, [0], pad(Z0), { Z is Z0 + 1 }.
pad(Z) --> { integer(Z), list_of(Z, 0, Pad) }, Pad.

answer1(N) :- answer1(N, _, _, _).
answer1(N, Bs, P, Z) :-
	input(Bs),
	phrase((packet(P, Z), pad(_)), Bs),
	add_versions(P, N).

add_versions(p(V, 4, _), V).
add_versions(p(V, T, [P|Ps]), Vo) :-
	( T < 4 ; T > 4 ),
	add_versions(P, V1),
	add_versions(p(0, T, Ps), Vn),
	Vo is V + V1 + Vn.
add_versions(p(V, _, []), V).

answer2(N) :- answer2(N, _, _, _).
answer2(N, Bs, P, Z) :-
	input(Bs),
	phrase((packet(P, Z), pad(_)), Bs),
	eval(P, N).

product([], 1).
product([V|Vs], P) :- product(Vs, Pn), P is V * Pn.
min([V], V).
min([V|Vs], M) :- min(Vs, Mn), (V < Mn, M = V ; V >= Mn, M = Mn).
max([V], V).
max([V|Vs], M) :- max(Vs, Mn), (V > Mn, M = V ; V =< Mn, M = Mn).

evaln([], []).
evaln([p(Vr,T,D)|Ps], [V|Vs]) :- eval(p(Vr,T,D), V), evaln(Ps, Vs).

eval(p(_, 0, Ps), N) :- evaln(Ps, Vs), sum(Vs, N).
eval(p(_, 1, Ps), N) :- evaln(Ps, Vs), product(Vs, N).
eval(p(_, 2, Ps), N) :- evaln(Ps, Vs), min(Vs, N).
eval(p(_, 3, Ps), N) :- evaln(Ps, Vs), max(Vs, N).
eval(p(_, 4, V), V).
eval(p(_, 5, Ps), N) :- evaln(Ps, [V1,V2]), (V1 > V2, N = 1 ; V1 =< V2, N = 0).
eval(p(_, 6, Ps), N) :- evaln(Ps, [V1,V2]), (V1 < V2, N = 1 ; V1 >= V2, N = 0).
eval(p(_, 7, Ps), N) :- evaln(Ps, [V1,V2]), (V1 = V2, N = 1 ; (V1 > V2; V1 < V2), N = 0).

