:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).

edge(A-B) --> seq(C), "-", seq(D), { atom_chars(A, C), atom_chars(B, D) }.
edges([A-B,B-A|Es]) --> edge(A-B), "\n", edges(Es).
edges([]) --> [].

input(Es) :-
	phrase_from_input((edges(Es0), "\n")),
	sort(Es0, Es),
	!.

sample([start-'A',start-b,'A'-c,'A'-b,b-d,'A'-end,b-end]).

% recursive depth-firsh search
% paths from S-E
% SE = edges from S-_ in Es
% Pi = for S-Ei in SE . edges_without(Es, 
paths(Es, Start, End, Ps) :-
	paths(Es, [Start], End, [], Ps).
paths(_, [], _, _, []).
paths(Es, [N|Ns], End, Path, Paths) :-
	(	N = End
	->	reverse([N|Path], CompletePath),
		SubPaths = [CompletePath]
	;	visit_vertex(Es, N, VNext, EInt),
		rm_lowercase(EInt, [N], ENext),
		paths(ENext, VNext, End, [N|Path], SubPaths)
	),
	paths(Es, Ns, End, Path, OtherPaths),
	append(SubPaths, OtherPaths, Paths).

% Edges, Vertex, ConnectedVerts, 
visit_vertex(Es, V, VNext, ENext) :-
	visit_vertex_(Es, V, VNext0, ENext),
	sort(VNext0, VNext).
visit_vertex_([], _, [], []).
visit_vertex_([V1-V2|Edges], Vert, VNext, ENext) :-
	(V1 = Vert, VNext = [V2|VNextn] ; V2 = Vert, VNext = [V1|VNextn])
->	ENext = [V1-V2|ENextn],
	visit_vertex_(Edges, Vert, VNextn, ENextn)
;	ENext = [V1-V2|ENextn],
	visit_vertex_(Edges, Vert, VNext, ENextn).

% codes for lowercase 'a' and 'z'
lowercase(Atom) :- atom_codes(Atom, [C|_]), C >= 97, C =< 122.

just_lowercase([], []).
just_lowercase([A|As], Bs) :-
	lowercase(A)
->	just_lowercase(As, BNext), Bs = [A|BNext]
;	just_lowercase(As, Bs).

rm_lowercase(Es, Vs, EOut) :-
	just_lowercase(Vs, VLower),
	rm_vertices(Es, VLower, EOut).
rm_vertices([], _, []).
rm_vertices([V1-V2|Es], Vs, EOut) :-
	(	(member(V1, Vs) ; member(V2, Vs))
	->	EOut = ENext
	;	EOut = [V1-V2|ENext]
	),
	rm_vertices(Es, Vs, ENext).

write_paths([]).
write_paths([P|Ps]) :- write(P), write('\n'), write_paths(Ps).

answer1(N) :- answer1(N, _, _).
answer1(N, Es, Ps) :-
	input(Es),
	time(paths(Es, start, end, Ps)),
	length(Ps, N).

paths2(Es, Start, End, Ps) :-
	paths2(Es, [Start], End, [], Ps).
paths2(_, [], _, _, []).
paths2(Es, [N|Ns], End, Path, Paths) :-
	(	N = End
	->	reverse([N|Path], CompletePath),
		SubPaths = [CompletePath]
	;	visit_vertex(Es, N, VNext, EInt),
		rm_lowercase(EInt, [N], ENext),
		paths2(ENext, VNext, End, [N|Path], SubPaths)
	),
	paths2(Es, Ns, End, Path, OtherPaths),
	append(SubPaths, OtherPaths, Paths).

answer2(N, Es, Ps) :-
	input(Es),
	time(paths2(Es, start, end, Ps)),
	length(Ps, N).
	
