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



