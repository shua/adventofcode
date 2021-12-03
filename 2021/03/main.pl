:- use_module('../util.pl').

bits([]) --> "\n".
bits([B|Bs]) --> digit(B), bits(Bs).

add_bits([], [], []).
add_bits([A|As], [B|Bs], [C|Cs]) :- C is A + B, add_bits(As, Bs, Cs).

greater([], _, []).
greater([A|As], N, [B|Bs]) :- ( A > N, B = 1 ; A < N, B = 0 ), greater(As, N, Bs).

agg(N, In, Out) --> bits(Line), { add_bits(In, Line, Next), N1 is N+1 }, agg(N1, Next, Out).
agg(N, In, Out) --> [], { Half is N / 2, greater(In, Half, Out) }.
agg(Out) --> agg(0, [0,0,0,0, 0,0,0,0, 0,0,0,0], Out).



num_from_bits(N, N, []).
num_from_bits(In, Out, [B|Bs]) :- Next is In*2 + B, num_from_bits(Next, Out, Bs).
num_from_bits(N, Bs) :- num_from_bits(0, N, Bs).

not_bits([], []).
not_bits([A|As], [B|Bs]) :- ((B = 1, A = 0) ; (B = 0, A = 1)), not_bits(As, Bs).

answer1(N) :-
	phrase_from_input((agg(BitString), "\n")),
	num_from_bits(Gamma, BitString),
	not_bits(BitString, AlphaBitString),
	num_from_bits(Alpha, AlphaBitString),
	N is Gamma * Alpha.


tree(leaf(N), [], leaf(N1)) :- N1 is N + 1.
tree(node(N, Left, Right), [], node(N1, Left, Right)) :- N1 is N + 1.
tree(node(N, Left, Right), [0|Bs], node(N1, Left1, Right)) :-
	N1 is N + 1, tree(Left, Bs, Left1).
tree(node(N, Left, Right), [1|Bs], node(N1, Left, Right1)) :-
	N1 is N + 1, tree(Right, Bs, Right1).
tree(leaf(N), [0|Bs], node(N1, Left, leaf(0))) :-
	N1 is N + 1, tree(leaf(0), Bs, Left).
tree(leaf(N), [1|Bs], node(N1, leaf(0), Right)) :-
	N1 is N + 1, tree(leaf(0), Bs, Right).

tree_n(leaf(N), N).
tree_n(node(N, _, _), N).

ptree(In, Out) --> bits(Bs), { tree(In, Bs, Next) }, ptree(Next, Out).
ptree(T, T) --> [].
ptree(Out) --> ptree(leaf(0), Out).

o2_rating(leaf(_), []).
o2_rating(node(_, Left, Right), [0|Bs]) :- tree_n(Left, Ln), tree_n(Right, Rn), Ln > Rn, o2_rating(Left, Bs).
o2_rating(node(_, Left, Right), [1|Bs]) :- tree_n(Left, Ln), tree_n(Right, Rn), Ln =< Rn, o2_rating(Right, Bs).

co2_rating(leaf(_), []).
co2_rating(node(_, Left, Right), [0|Bs]) :- tree_n(Left, Ln), tree_n(Right, Rn), Ln > 0, (Ln =< Rn ; Rn = 0), co2_rating(Left, Bs).
co2_rating(node(_, Left, Right), [1|Bs]) :- tree_n(Left, Ln), tree_n(Right, Rn), Rn > 0, (Ln > Rn ; Ln = 0), co2_rating(Right, Bs).

answer2(N) :-
	phrase_from_input((ptree(BiTree), "\n")),
	o2_rating(BiTree, O2B),
	co2_rating(BiTree, CO2B),
	num_from_bits(O2rating, O2B),
	num_from_bits(CO2rating, CO2B),
	N is O2rating * CO2rating.
