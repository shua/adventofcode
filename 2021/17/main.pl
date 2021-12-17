:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(time)).

input(192, 251, -89, -59).
sample(20, 30, -10, -5).

answer1(N) :-
	time((
	input(_, _, YMin, _),
	YMaxV is -1 * YMin - 1,
	N is (YMaxV * (YMaxV+1)) div 2
	)).

expected2([
	[23,-10],  [25,-9],   [27,-5],   [29,-6],   [22,-6],   [21,-7],   [9,0],     [27,-7],   [24,-5],
	[25,-7],   [26,-6],   [25,-5],   [6,8],     [11,-2],   [20,-5],   [29,-10],  [6,3],     [28,-7],
	[8,0],     [30,-6],   [29,-8],   [20,-10],  [6,7],     [6,4],     [6,1],     [14,-4],   [21,-6],
	[26,-10],  [7,-1],    [7,7],     [8,-1],    [21,-9],   [6,2],     [20,-7],   [30,-10],  [14,-3],
	[20,-8],   [13,-2],   [7,3],     [28,-8],   [29,-9],   [15,-3],   [22,-5],   [26,-8],   [25,-8],
	[25,-6],   [15,-4],   [9,-2],    [15,-2],   [12,-2],   [28,-9],   [12,-3],   [24,-6],   [23,-7],
	[25,-10],  [7,8],     [11,-3],   [26,-7],   [7,1],     [23,-9],   [6,0],     [22,-10],  [27,-6],
	[8,1],     [22,-8],   [13,-4],   [7,6],     [28,-6],   [11,-4],   [12,-4],   [26,-9],   [7,4],
	[24,-10],  [23,-8],   [30,-8],   [7,0],     [9,-1],    [10,-1],   [26,-5],   [22,-9],   [6,5],
	[7,5],     [23,-6],   [28,-10],  [10,-2],   [11,-1],   [20,-9],   [14,-2],   [29,-7],   [13,-3],
	[23,-5],   [24,-8],   [27,-9],   [30,-7],   [28,-5],   [21,-10],  [7,9],     [6,6],     [21,-5],
	[27,-10],  [7,2],     [30,-9],   [21,-8],   [22,-7],   [24,-9],   [20,-6],   [6,9],     [29,-5],
	[8,-2],    [27,-8],   [30,-5],   [24,-7]
]).

answer2(N) :- time(answer2(N, _, _, _, _, _, _)).
answer2(N, Yvs, Xvs, MaxSteps, StepVs, Vs, VSorted) :-
	input(XMin, XMax, YMin, YMax),
	%sample(XMin, XMax, YMin, YMax),
	collect_y(YMin, YMax, MaxSteps, Yvs),
	collect_x(XMin, XMax, MaxSteps, Xvs),
	intersections(Xvs, Yvs, StepVs),
	values(StepVs, SVs),
	append(SVs, Vs),
	sort(Vs, VSorted),
	length(VSorted, N).

collect_x(XMin, XMax, MaxSteps, Xvs) :-
	Xmv = XMin div MaxSteps, XMv = XMax,
	collect_x(XMin-XMax, MaxSteps, XMv, Xmv, [], Xvs).
collect_x(Target, Stepn, Vel, Vel, Acc, Xvs) :-
	collect_steps_x(Target, Stepn, Vel, 0, 0, Acc, Xvs).
collect_x(Target, Stepn, Veln, Vel, Acc, Xvs) :-
	Vel < Veln,
	collect_steps_x(Target, Stepn, Vel, 0, 0, Acc, Acc1),
	Vel1 is Vel + 1,
	collect_x(Target, Stepn, Veln, Vel1, Acc1, Xvs).
collect_steps_x(_, Sn, _, _, Sn, Acc, Acc).
collect_steps_x(Target, Sn, Vel, Pos, Si, Acc, Out) :-
	Si < Sn, S1 is Si + 1,
	(Si >= Vel, Veli = 0 ; Si < Vel, Veli = Vel - Si),
	Posi is Pos + Veli,
	collect_steps_x(Target, Sn, Vel, Posi, S1, Acc, Out0),
	(	inside(Target, Pos),
		Out = [Si-Vel|Out0]
	;	outside(Target, Pos),
		Out = Out0
	).


collect_y(YMin, YMax, MaxSteps, Yvs) :-
	Ymv = YMin, YMv is -1 * YMin - 1,
	MaxSteps is YMv*2 + 3,
	collect_y(YMin-YMax, MaxSteps, YMv, Ymv, [], Yvs).
collect_y(Target, Stepn, Vel, Vel, Acc, Yvs) :-
	collect_steps_y(Target, Stepn, Vel, 0, 0, Acc, Yvs).
collect_y(Target, Stepn, Veln, Vel, Acc, Yvs) :-
	Vel < Veln,
	collect_steps_y(Target, Stepn, Vel, 0, 0, Acc, Acc1),
	Vel1 is Vel + 1,
	collect_y(Target, Stepn, Veln, Vel1, Acc1, Yvs).

collect_steps_y(_, Sn, _, _, Sn, Acc, Acc).
collect_steps_y(Target, Sn, Vel, Pos, Si, Acc, Out) :-
	Si < Sn, S1 is Si + 1,
	Veli is Vel - Si, Posi is Pos + Veli,
	collect_steps_y(Target, Sn, Vel, Posi, S1, Acc, Out0),
	(	inside(Target, Pos),
		Out = [Si-Vel|Out0]
	;	outside(Target, Pos),
		Out = Out0
	).

inside(Mn-Mx, V) :- V >= Mn, V =< Mx.
outside(Mn-Mx, V) :- V < Mn ; V > Mx.

intersections(Xvs, Yvs, SVs) :-
	group_by_key(Xvs, Sx), % Sx = [Step-[X|Xs]|KVs]
	group_by_key(Yvs, Sy),
	iter_product(Sx, Sy, SVs).

zip([], [], []).
zip([K-V|KVs], [K|Ks], [V|Vs]) :- zip(KVs, Ks, Vs).
values(KVs, Vs) :- zip(KVs, _, Vs).
keys(KVs, Ks) :- zip(KVs, Ks, _).

product(Xs, Ys, XYs) :- product(Xs, Ys, [], XYs).
product1(_, [], Acc, Acc).
product1(X, [Y|Ys], Acc, [[X,Y]|XYs]) :-
	product1(X, Ys, Acc, XYs).
product([], _, Acc, Acc).
product([X|Xs], Ys, Acc, XYs) :-
	product1(X, Ys, Acc, Acc1),
	product(Xs, Ys, Acc1, XYs).

max([V], V).
max([I|Is], M) :- max(Is, M0), (I > M0, M = I ; I =< M0, M = M0).

iter_product([], _, []).
iter_product([_|_], [], []).
iter_product([K1-_|KV1], [K2-V2|KV2], KVProd) :-
	K1 < K2,
	iter_product(KV1, [K2-V2|KV2], KVProd).
iter_product([K1-V1|KV1], [K2-_|KV2], KVProd) :-
	K1 > K2,
	iter_product([K1-V1|KV1], KV2, KVProd).
iter_product([K1-V1|KV1], [K1-V2|KV2], [K1-VProd|KVProd]) :-
	product(V1, V2, VProd),
	iter_product(KV1, KV2, KVProd).

% KVs = [K1-Va,K1-Vb,K2-Vc|_] UKVs = [K1-[Va,Vb],K2-[Vc]|_]
group_by_key(KVs, UKVs) :-
	keysort(KVs, KVSrt),
	group_by_key_(KVSrt, UKVs).
group_by_key_(KVs, [K0-[V0|V0s]]) :-
	KVs = [K0-_|_],
	append(Pre, [K0-V0], KVs),
	values(Pre, V0s).
group_by_key_(KVs, [K0-[V0|V0s]|UKVs]) :-
	KVs = [K0-_|_],
	append(Pre, [K0-V0,K1-V1|Post], KVs), K1 > K0,
	values(Pre, V0s),
	group_by_key_([K1-V1|Post], UKVs).

set_diff(S1, S2, Sdiff) :-
	append(S1, S1, S3),
	append(S3, S2, S4),
	counts(S4, SCts),
	keys_for(SCts, 2, Sdiff).
keys_for([], _, []).
keys_for([_-V1|KVs], V2, Ks) :- (V1<V2;V1>V2), keys_for(KVs, V2, Ks).
keys_for([K-V|KVs], V, [K|Ks]) :-
	keys_for(KVs, V, Ks).


