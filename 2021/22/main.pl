:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(format)).

% try dumb simple first
onoff(on) --> "on". onoff(off) --> "off".
int(In) --> "-", uint(I), { In is -I }.
int(I) --> uint(I).

instr(IO, [Xm,XM,Ym,YM,Zm,ZM]) -->
	onoff(IO),
	" x=", int(Xm), "..", int(XM),
	",y=", int(Ym), "..", int(YM),
	",z=", int(Zm), "..", int(ZM).
instrs([]) --> "\n".
instrs([instr(IO, Bounds)|Is]) --> instr(IO, Bounds), "\n", instrs(Is).

min(A, B, A) :- A < B. min(A, B, B) :- B < A. min(A, A, A).
max(A, B, A) :- A > B. max(A, B, B) :- B > A. max(A, A, A).
clamp(A, [Min,Max], B) :- max(A, Min, C), min(C, Max, B).

filter(_, [], []).
filter(C, [I|Is], IsP) :-
	I = instr(IO, [Xm,XM,Ym,YM,Zm,ZM]),
	clamp(Xm, C, XmP), clamp(XM, C, XMP),
	clamp(Ym, C, YmP), clamp(YM, C, YMP),
	clamp(Zm, C, ZmP), clamp(ZM, C, ZMP),
	(	( XmP = XMP ; XmP < XMP, ( YmP = YMP ; YmP < YMP, ZmP = ZMP ) ),
		filter(C, Is, IsP)
	;	XmP < XMP, YmP < YMP, ZmP < ZMP,
		IsP = [instr(IO, [XmP,XMP,YmP,YMP,ZmP,ZMP])|IsPP],
		filter(C, Is, IsPP)
	).
uniq([], []).
uniq([V], [V]).
uniq([I,I|Is], IsP) :-
	uniq([I|Is], IsP).
uniq([I1,I2|Is], [I1|IsP]) :-
	I1 \== I2,
	uniq([I2|Is], IsP).

input(Is) :- phrase_from_file(instrs(Is), 'input.txt').


% duh, build a tree of ranges, work through the list backwards
% only insert, where there is nothing, do not overwrite already written data

process(Is, T) :- process(Is, [], T).
process([], T, T).
process([instr(IO, AABB)|Is], T0, Tn) :-
	( length(Is, In), format("~d ~w ~w ", [In, IO, AABB]) ),
	insert(T0, IO, AABB, T1),
	format("~n", []),
	process(Is, T1, Tn).

% leaf, no overwrites are allowed, only insertion into non-existant var
insert(l(IO), _, [], l(IO)).
insert([], IO, [], l(IO)).
% empty tree
insert([], IO, [Bm,BM|Bs], [n(Bm,BM,TChildren)]) :-
	insert([], IO, Bs, TChildren).
% bounds are before
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], T) :-
	BM < Nm,
	insert([], IO, Bs, Children),
	T = [n(Bm,BM, Children), n(Nm,NM, NCs)|Ns].
% bounds are after
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Nm,NM, NCs)|T]) :-
	Bm > NM,
	insert(Ns, IO, [Bm,BM|Bs], T).
% bounds collide with prefix
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Bm,BM1, BCs)|T]) :-
	Bm < Nm, BM >= Nm,
	BM1 is Nm - 1, Bm1 = Nm,
	format("~w ", [Bs]),
	insert([], IO, Bs, BCs),
	insert([n(Nm,NM, NCs)|Ns], IO, [Bm1,BM|Bs], T).
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Nm,NM1, NCs)|T]) :-
	Bm > Nm, Bm =< NM,
	NM1 = Bm - 1, Nm1 = Bm,
	insert([n(Nm1,NM, NCs)|Ns], IO, [Bm,BM|Bs], T).
% bounds collide with suffixes
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Nm,NM1, NC1), n(Nm1,NM, NCs)|Ns]) :-
	Bm = Nm, BM < NM,
	NM1 = BM, Nm1 is BM + 1,
	format("~w ", [Bs]),
	insert(NCs, IO, Bs, NC1).
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Nm,NM, NC1)|T]) :-
	Bm = Nm, BM > NM,
	Bm1 is NM + 1,
	format("~w ", [Bs]),
	insert(NCs, IO, Bs, NC1),
	insert(Ns, IO, [Bm1,BM|Bs], T).
% bounds collide exactly
insert([n(Nm,NM, NCs)|Ns], IO, [Bm,BM|Bs], [n(Nm,NM, NC1)|Ns]) :-
	Bm = Nm, BM = NM,
	insert(NCs, IO, Bs, NC1).

area([], _, 0).
area(l(IO), IO, 1).
area(l(on), off, 0).
area(l(off), on, 0).
area([n(Nm,NM, NCs)|Ns], IO, N) :-
	area(NCs, IO, NN),
	area(Ns, IO, NsN),
	N is NsN + (NM - Nm + 1) * NN.

answer1(N) :-
	input(Is),
	filter([-50,50], Is, I1),
	reverse(I1, I2),
	process(I2, T),
	area(T, on, N).

answer2(N) :-
	input(Is),
	reverse(Is, Ir),
	process(Ir, T),
	area(T, on, N).
	
