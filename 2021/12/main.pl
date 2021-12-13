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

reverse_elems([], []).
reverse_elems([E|Es], [Er|Ers]) :- reverse(E, Er), reverse_elems(Es, Ers).

% recursive depth-firsh search
% paths from S-E
% SE = edges from S-_ in Es
% Pi = for S-Ei in SE . edges_without(Es, 
%
% NOTE(jd): runtime for answer1 is around 1.6s, tried tabling, but it was much worse
paths(Es, Start, End, Ps) :-
	paths(Es, [Start], End, [], Ps0),
	reverse_elems(Ps0, Ps).
paths(_, [], _, _, []).
paths(Es, [N|Ns], End, Path, [[N|Path]|Paths]) :-
	N == End,
	paths(Es, Ns, End, Path, Paths).
paths(Es, [N|Ns], End, Path, Paths) :-
	N \== End,
	visit_vertex(Es, N, VNext),
	(	lowercase(N),
		rm_vertex(Es, N, ENext),
		paths(ENext, VNext, End, [N|Path], SubPaths)
	;	uppercase(N),
		paths(Es, VNext, End, [N|Path], SubPaths)
	),
	paths(Es, Ns, End, Path, OtherPaths),
	append(SubPaths, OtherPaths, Paths).

% Edges, Vertex, ConnectedVerts, 
visit_vertex(Es, V, VNext) :-
	visit_vertex_(Es, V, VNext0),
	sort(VNext0, VNext).
visit_vertex_([], _, []).
visit_vertex_([V1-V2|Edges], Vert, VNext) :-
	(	V1 == Vert, VNext = [V2|VNextn]
	;	V2 == Vert, VNext = [V1|VNextn]
	),
	visit_vertex_(Edges, Vert, VNextn)
;	V1 \== Vert, V2 \== Vert, visit_vertex_(Edges, Vert, VNext).

% codes for lowercase 'a' and 'z'
lowercase(Atom) :- atom_codes(Atom, [C|_]), C >= 97, C =< 122.
uppercase(Atom) :- atom_codes(Atom, [C|_]), C >= 65, C =< 90.

rm_lowercase(Es, V, EOut) :- lowercase(V), rm_vertex(Es, V, EOut).
rm_lowercase(Es, V, Es) :- uppercase(V).
rm_vertex([], _, []).
rm_vertex([V1-V2|Es], V, EOut) :-
	(V1 == V ; V2 == V),
	rm_vertex(Es, V, EOut).
rm_vertex([V1-V2|Es], V, [V1-V2|EOut]) :-
	V1 \== V, V2 \== V,
	rm_vertex(Es, V, EOut).

write_paths([]).
write_paths([P|Ps]) :- write(P), write('\n'), write_paths(Ps).

answer1(N) :- answer1(N, _, _).
answer1(N, Es, Ps) :-
	input(Es),
	time(paths(Es, start, end, Ps)),
	length(Ps, N).

paths2(Es, Start, End, Ps) :-
	paths2(Es, [Start], End, [], Ps0),
	reverse_elems(Ps0, Ps1),
	% rm dups
	% path2(a,path2(b,path(c))) vs path2(a,path(b,path(c))) both give [a,b,c]
	sort(Ps1, Ps).
paths2(_, [], _, _, []).
paths2(Es, [N|Ns], End, Path, [[N|Path]|Paths]) :-
	N == End,
	paths2(Es, Ns, End, Path, Paths).
paths2(Es, [N|Ns], End, Path, Paths) :-
	N \== End,
	visit_vertex(Es, N, VNext),
	% regular
	rm_lowercase(Es, N, ENext),
	paths2(ENext, VNext, End, [N|Path], SubPaths1),
	paths2(Es, Ns, End, Path, OtherPaths),
	% leave one lowercase in, but then proceed with original paths
	(	lowercase(N), N \== start,
		paths(Es, VNext, End, [N|Path], SubPaths2),
		append(SubPaths2, SubPaths1, SubPaths)
	;	(uppercase(N) ; N == start),
		SubPaths = SubPaths1
	),
	append(SubPaths, OtherPaths, Paths).

% 3m30s not ideal, but got the right answer :shrug:
answer2(N) :- answer2(N, _, _).
answer2(N, Es, Ps) :-
	input(Es),
	time(paths2(Es, start, end, Ps0)),
	sort(Ps0, Ps),
	length(Ps, N).
	
