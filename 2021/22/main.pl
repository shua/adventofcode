:- use_module('../util.pl').

onoff(1) --> "on".
onoff(0) --> "off".

int(I) --> "-", uint(In), { I is -In }.
int(I) --> uint(I).

bound([Lo,Hi]) --> int(Lo), "..", int(Hi).

line(IO, [X,Y,Z]) --> onoff(IO), " x=", bound(X), ",y=", bound(Y), ",z=", bound(Z). 

lines([IO-XYZ|Ls]) --> line(IO, XYZ), "\n", lines(Ls).
lines([]) --> [].

input(Ls) :- phrase_from_input((lines(Ls), "\n")).


answer1(N) :- fail.

answer2(N) :- fail.

