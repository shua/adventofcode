bitbound(N, B) :- 
	(B is 0; B is 1),
	(N is 0; N is 1; N is 2; N is 3;
	N is 4; N is 5; N is 6; N is 7;
	N is 8; N is 9; N is 10; N is 11;
	N is 12; N is 13; N is 14; N is 15) .

bit(X, N, B) :-
	bitbound(N,B),
	integer(X),
	B is ((X /\ (1 << N)) >> N) .

eql(X, N, B) :- 
	bitbound(N,B),
	bit(X, N, B) .

not(X, N, B) :-
	bitbound(N,B),
	bit(X, N, Bx),
	B is 1 - Bx.

or(X, Y, N, B) :-
	bitbound(N,B),
	bit(X, N, Bx),
	bit(Y, N, By),
	B is Bx \/ By.

and(X, Y, N, B) :-
	bitbound(N,B),
	bit(X, N, Bx),
	bit(Y, N, By),
	B is Bx /\ By.

xor(X, Y, N, B) :-
	bitbound(N,B),
	or(X, Y, N, Bo),
	and(X, Y, N, Ba),
	B is Bo /\ \Ba.

shft(_, S, N, B) :-
	integer(S),
	bitbound(N,B),
	(N - S >= 16; N - S < 0),
	B is 0 .

shft(X, S, N, B) :-
	integer(S),
	bitbound(N,B),
	Ns is N - S,
	bit(X, Ns, B) .

num(X, C) :- 
	bit(X, 0, B0), 
	bit(X, 1, B1), 
	bit(X, 2, B2), 
	bit(X, 3, B3),
	bit(X, 4, B4), 
	bit(X, 5, B5), 
	bit(X, 6, B6), 
	bit(X, 7, B7),
	bit(X, 8, B8), 
	bit(X, 9, B9), 
	bit(X, 10, B10), 
	bit(X, 11, B11),
	bit(X, 12, B12), 
	bit(X, 13, B13), 
	bit(X, 14, B14), 
	bit(X, 15, B15),
	C is (B15 << 15) + (B14 << 14) + (B13 << 13) + (B12 << 12)
	   + (B11 << 11) + (B10 << 10) + (B9  << 9 ) + (B8  << 8 )
	   + (B7  << 7 ) + (B6  << 6 ) + (B5  << 5 ) + (B4  << 4 )
	   + (B3  << 3 ) + (B2  << 2 ) + (B1  << 1 ) + B0 .

