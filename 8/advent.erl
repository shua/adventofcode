-module(advent).
-export([run/1,count_printable/1,count_encoded/1]).

run(Filename) ->
	{ok, Device} = file:open(Filename,read),
	First = for_each_line(Device, fun advent:count_printable/1, 0),
	io:fwrite("First: ~w~n", [First]),
	file:position(Device, 0),
	Second = for_each_line(Device, fun advent:count_encoded/1, 0),
	io:fwrite("Second: ~w~n", [Second]),
	file:close(Device).

for_each_line(Device, Proc, Accum) ->
	case io:get_line(Device, "") of
		eof -> Accum;
		Line -> NewAccum = Accum + Proc(Line),
%				io:fwrite("~w~n", [NewAccum]),
				for_each_line(Device, Proc, NewAccum)
	end.

% after doing the second, I think I could have done the first in a similar way
% without re, instead counting each initial \ as 1 and moving next n chars
count_printable(String) -> 
	NoSlash = re:replace(String, "\\\\\\\\", "_", [global, {return,list}]),
	NoHex = re:replace(NoSlash, "\\\\x..", "X", [global, {return,list}]),
	NoQuote = re:replace(NoHex, "\"", "", [global, {return,list}]),
%	io:fwrite("~s", [String]),
%	io:fwrite("~s", [NoSlash]),
%	io:fwrite("~s", [NoHex]),
%	io:fwrite("~s", [NoQuote]),
%	io:fwrite("~w ~w~n", [string:len(String)-1,string:len(NoQuote)-1]),
	string:len(String) - string:len(NoQuote).

count_encoded(String) when is_list(String) -> count_encoded(String, 0) - (string:len(String) - 1).
count_encoded([H|T], Accum) when H =:= $\"; H =:= $\\ -> 
%	io:fwrite("quote or slash~n", []), 
	count_encoded(T, Accum + 2);
count_encoded([H|T], Accum) when H =/= $\n -> 
%	io:fwrite("not nl~n", []), 
	count_encoded(T, Accum + 1);
count_encoded([$\n], Accum) -> 
%	io:fwrite("nl~n", []), 
	% plus two for surrounding quotes on final product
	Accum + 2.


