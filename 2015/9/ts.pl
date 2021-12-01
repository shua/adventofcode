%road(london,dublin,464).
%road(london,belfast,518).
%road(dublin,belfast,141).

:- include("input.pl").

perm([],[]).
perm([H|T],O) :-
	perm(T,Oo),
	select(H,O,Oo).

distance(O) :- towns(T), perm(T,R), distance(R,O).
distance(Path,O) :- distance(Path,O,0).
distance([_],N,N).
distance([City,Next|Path],A,I) :-
	(road(City,Next,N);road(Next,City,N)),
	Nn is I + N,
	distance([Next|Path],A,Nn).

towns(T) :-
	road(_,_,_),
	findall(X,road(X,_,_),A),
	findall(Y,road(_,Y,_),B),
	append(A,B,C),
	list_to_set(C,T),!.

first(O) :-
	bagof(N, distance(N), Ns),
	min_list(Ns,O).

second(O) :-
	bagof(N, distance(N), Ns),
	max_list(Ns,O).

