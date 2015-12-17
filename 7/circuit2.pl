bit_to_num_([],I,I).
bit_to_num_([Bh|Bt],I,O) :-
	(Bh =:= 1; Bh =:= 0),
	Ii is (I*2) + Bh,
	bit_to_num_(Bt,Ii,O).

bit_from_num_(L,L,E,_) :- length(L,E).
bit_from_num_(O,I,E,0) :-
	length(I,Li),
	Li < E,
	bit_from_num_(O,[0|I],E,0).
bit_from_num_(O,I,E,N) :-
	length(I,Li),
	Li < E,
	Ih is N mod 2,
	M is N // 2,
	bit_from_num_(O,[Ih|I],E,M).

bit_num(L,N) :- var(N), bit_to_num_(L,0,N).
bit_num(L,N) :- integer(N), bit_from_num_(L,[],16,N),!.

rsig(A,S) :-
	atom(A),
	readings(L,A,[]),
	member(A/B,L),
	bit_num(B,S).

readings(I,A,I) :-
	atom(A),
	member(A/B,I).
readings(O,A,I) :-
	atom(A),
	write('I don\'t know how to do this'), nl.

eql_(1,unk,1).
eql_(1,1,unk).
eql_(1,1,1).
eql_(0,unk,0).
eql_(0,0,unk).
eql_(0,0,0).
eql_(unk,unk,unk).


not_(1,unk,0).
not_(1,1,unk).
not_(1,1,0).
not_(0,unk,1).
not_(0,0,unk).
not_(0,0,1).
not_(unk,unk,unk).


and_(1,1,1,1,1).
and_(1,1,1,1,unk).
and_(1,1,1,unk,1).
and_(1,1,unk,1,1).
and_(1,unk,1,unk,unk).
and_(unk,1,unk,1,unk).
and_(unk,unk,unk,unk,unk).

and_(0,0,0,0,0).
and_(0,0,0,0,unk).
and_(0,unk,0,unk,0).
and_(unk,0,unk,0,0).

and_(0,1,0,1,0).
and_(0,1,0,1,unk).
and_(0,1,unk,1,0).
and_(1,0,1,0,0).
and_(1,0,1,0,unk).
and_(1,0,1,unk,0).


or_(1,1,1,1,1).
or_(1,1,1,1,unk).
or_(1,unk,1,unk,1).
or_(unk,1,unk,1,1).
or_(1,unk,1,unk,unk).
or_(unk,1,unk,1,unk).
or_(unk,unk,unk,unk,unk).

or_(0,0,0,0,0).
or_(0,0,0,0,unk).
or_(0,unk,0,unk,unk).
or_(unk,0,unk,0,unk).

or_(1,0,1,0,1).
or_(1,0,1,0,unk).
or_(1,0,unk,0,1).
or_(0,1,0,1,1).
or_(0,1,0,1,unk).
or_(0,1,0,unk,1).


