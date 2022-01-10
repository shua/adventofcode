:- use_module('../util.pl').
:- use_module(library(lists)).

input([A,E,B,F,C,G,D,H]) -->
	"#############\n",
	"#...........#\n",
	"###", [A], "#", [B], "#", [C], "#", [D], "###\n",
	"  #", [E], "#", [F], "#", [G], "#", [H], "#\n",
	"  #########\n".

input(As) :- phrase_from_input((input(As), "\n")).

%  1  2   2   2   2  1
% #-#---#---#---#---#-#
%   2\2/2\2/2\2/2\2/
%     #   #   #   #
%    1|  1|  1|  1|
%     #   #   #   #

answer1(N) :- fail.

answer2(N) :- fail.

