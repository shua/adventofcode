:- include("circuit.pl").
bit(a, N, B) :- and(b, c, N,  B).
bit(b, N, B) :- eql(43690, N, B).
bit(c, N, B) :- eql(255, N, B).
