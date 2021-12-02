:- use_module('../util.pl').

command, [X,Z] --> [X0,Z], "forward ", uint(N), "\n", { X is X0+N }.
command, [X,Z] --> [X,Z0], "up ", uint(N), "\n", { Z is Z0-N }.
command, [X,Z] --> [X,Z0], "down ", uint(N), "\n", { Z is Z0+N }.

commands_init, [0, 0] --> [].
commands_(X,Z) --> [X,Z].
commands_(X,Z) --> command, commands_(X,Z).
commands(X,Z) --> commands_init, commands_(X,Z).

answer1(N) :-
	phrase_from_input((commands(X,Z), "\n")),
	N is X * Z.

command2, [X,Z,A] --> [X0,Z0,A], "forward ", uint(N), "\n", { X is X0+N, Z is Z0 + (A*N) }.
command2, [X,Z,A] --> [X,Z,A0], "up ", uint(N), "\n", { A is A0-N }.
command2, [X,Z,A] --> [X,Z,A0], "down ", uint(N), "\n", { A is A0+N }.

commands2_init, [0,0,0] --> [].
commands2_(X,Z) --> command2, commands2_(X,Z).
commands2_(X,Z) --> [X,Z,_].
commands2(X,Z) --> commands2_init, commands2_(X,Z).
answer2(N) :-
	phrase_from_input((commands2(X,Z), "\n")),
	N is X * Z.
