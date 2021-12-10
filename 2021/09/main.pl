:- use_module('../util.pl').
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(time)).

line([]) --> "\n".
line([D|Ds]) --> digit(D), line(Ds).

matrix([], _) --> [].
matrix(M, Stride) --> line(L), { length(L, Stride) }, matrix(M0, Stride), { append(L, M0, M) }.

input(M, Stride) :- phrase_from_input((matrix(M, Stride), "\n")).

sample(M, Stride) :- phrase_from_file((matrix(M, Stride), "\n"), 'sample.txt').

list_of(0, _, []).
list_of(N, V, [V|Vs]) :-
	N > 0, N1 is N - 1,
	list_of(N1, V, Vs).

% keep buffer 1 line back
% scan first horizontal, then vertical
%    (1,1,1) U
% R0 [2,1,3] -> (0,1,0) H
% R  [3,2,1] -> (0,0,1) H1
%    (1,1,0) D
% row 1 lowpoints -> U&H&D = (0,1,0) -> [1]
% repeat with next row, H = H1, U = not(D)
scan(M, Stride, LowPts) :-
	append(R, M1, M), length(R, Stride),
	list_of(Stride, 1, Trues),
	scan_horz(R, H),
	scan(M1, Stride, R, Trues, H, LowPts).

scan([], _, R, U, H, LowPts) :-
	and([U,H], Mask),
	collect_mask(R, Mask, LowPts).
scan(M, Stride, R, U, H, LowPts) :-
	append(R1, M1, M), length(R1, Stride),
	mask_vert(R, R1, U, H, D, LowPtsCur),
	not(D, U1),
	scan_horz(R1, H1),
	scan(M1, Stride, R1, U1, H1, LowPtsRest),
	append(LowPtsCur, LowPtsRest, LowPts).

mask_vert(R0, R, U, H, D, LowPts) :-
	lt(R0, R, D),
	and([U,H,D], Mask),
	collect_mask(R0, Mask, LowPts).

lt([], [], []).
lt([A|As], [B|Bs], [M|Mask]) :-
	A < B, M = 1, lt(As, Bs, Mask)
;	A >= B, M = 0, lt(As, Bs, Mask).

and([], [], []).
and([A|As], [B|Bs], [M|Mask]) :-
	(	A=1, B=1, M=1
	;	A=0, B=_, M=0
	;	A=_, B=0, M=0
	),
	and(As, Bs, Mask).

and([Mask], Mask).
and([V1,V2|Vs], Mask) :-
	and(V1, V2, Cur),
	and([Cur|Vs], Mask).

not([], []).
not([1|As], [0|Mask]) :- not(As, Mask).
not([0|As], [1|Mask]) :- not(As, Mask).

scan_horz(Ds, Mask) :- scan_horz(Ds, [1], Mask).
scan_horz([_], LtPrev, LtPrev).
scan_horz([D0,D1|Ds], LtPrev, [B0|Mask]) :-
	lt([D0], [D1], LtNext),
	and(LtPrev, LtNext, [B0]),
	not(LtNext, LtPrev1),
	scan_horz([D1|Ds], LtPrev1, Mask).

collect_mask([], [], []).
collect_mask([_|Ds], [0|Mask], Out) :- collect_mask(Ds, Mask, Out).
collect_mask([D|Ds], [1|Mask], [D|Out]) :- collect_mask(Ds, Mask, Out).

answer1(N, M, Stride, LowPts) :-
	input(M, Stride),
	scan(M, Stride, LowPts),
	sum(LowPts, Sum),
	length(LowPts, LN),
	N is Sum + LN.

% basin...
% we can find 1d basin
basin_tags_horz([9], Id, [0], Id).
basin_tags_horz([D], Id, [Id], EndId) :- D < 9, EndId is Id + 1.
basin_tags_horz([9,D1|Ds], Id, [0|Tags], EndId) :-
	( D1 < 9, Id1 is Id+1 ; D1 = 9, Id1 = Id ),
	basin_tags_horz([D1|Ds], Id1, Tags, EndId).
basin_tags_horz([D0,D1|Ds], Id, [Id|Tags], EndId) :-
	D0 < 9,
	basin_tags_horz([D1|Ds], Id, Tags, EndId).

basin_tags(M, Stride, Tags) :-
	basin_tags(M, Stride, 1, Tags).
basin_tags([], _, _, []).
basin_tags(M, Stride, Id, [T|Tags]) :-
	append(R, M1, M), length(R, Stride),
	basin_tags_horz(R, Id, T, Id1),
	basin_tags(M1, Stride, Id1, Tags).

% volumes
volumes(Tags, Vols) :-
	volumes_(Tags, Vols0),
	rm_value(0, Vols0, _, Vols).
volumes_([], []).
volumes_([T|Tags], Vol) :-
	volumes(Tags, VolRest),
	init_count(T, VolCur),
	append(VolCur, VolRest, Vol0),
	merge_add(Vol0, Vol).
init_count([], []).
init_count([K|Ks], [K-1|KVs]) :- init_count(Ks, KVs).

merge_add(KVs, Merged) :-
	keysort(KVs, Sorted),
	merge_add_(Sorted, Merged).
merge_add_([K-V], [K-V]).
merge_add_([K1-V1,K2-V2|KVs], Merged) :-
	K1 = K2, V is V1 + V2, merge_add_([K1-V|KVs], Merged)
;	K1 < K2, Merged = [K1-V1|Merged0], merge_add_([K2-V2|KVs], Merged0).


% tag_edges/2 collects all the edges from all rows of tags
tag_edges([_], []).
tag_edges([T1,T2|Tags], Edges) :-
	tag_edges(T1, T2, ECur),
	append(ECur, ERest, Edges),
	tag_edges([T2|Tags], ERest).
% tag_edges/3 collects all the edges from two adjacent rows
tag_edges([], [], []).
tag_edges([T1|Tags1], [T2|Tags2], Edges) :-
	( T1 = 0 ; T2 = 0 ),
	tag_edges(Tags1, Tags2, Edges).
tag_edges([T1|Tags1], [T2|Tags2], [T1-T2|Edges]) :-
	T1 > 0, T2 > 0,
	tag_edges(Tags1, Tags2, Edges).

% merging node data for connected graphs
merge_graphs(Nodes, [], Nodes).
merge_graphs(NodeIn, [Id1-Id2|Edges], NodeOut) :-
	connected([Id1,Id2], Edges, Ids0, EdgesCur),
	sort(Ids0, Ids),
	merge_nodes(Ids, NodeIn, NodeCur),
	merge_graphs(NodeCur, EdgesCur, NodeOut).

connected([], Edges, [], Edges).
connected([Id|W], Edges, [Id|Out], EdgesOut) :-
	connected_to(Id, Edges, Ids, ECur),
	append(W, Ids, W1),
	connected(W1, ECur, Out1, EdgesOut),
	append(Ids, Out1, Out).

connected_graphs([], []).
connected_graphs([Id1-Id2|Edges], [Cluster|Rest]) :-
	connected([Id1,Id2], Edges, Cluster0, EdgesCur),
	sort(Cluster0, Cluster),
	connected_graphs(EdgesCur, Rest).

sum_clusters(Clusters, Volumes, Sums) :-
	cluster_map(Clusters, CMap),
	keysort(Volumes, VSorted),
	sum_clusters_(CMap, VSorted, Sums0),
	merge_add(Sums0, Sums).
sum_clusters_(_, [], []).
sum_clusters_(CMap, [Id-V|Volumes], [Id1-V|KVs]) :-
	member(Id-Id1, CMap),
	sum_clusters_(CMap, Volumes, KVs).
cluster_map([], []).
cluster_map([C|Clusters], Map) :-
	cluster_map_(C, MapC),
	cluster_map(Clusters, MapRest),
	append(MapC, MapRest, Map).
cluster_map_([Id0|Ids], [Id0-Id0|Map]) :-
	cluster_map_(Id0, Ids, Map).
cluster_map_(_, [], []).
cluster_map_(Id0, [Id|Ids], [Id-Id0|Map]) :-
	cluster_map_(Id0, Ids, Map).

% connected_to/4 scans all edges for any connections to the passed tagid
% NodeIn/Out is [K-V,...], Edges is [K1-K2, ...]
connected_to(_, [], [], []).
connected_to(Id, [Id1-Id2|Edges], Ids, [Id1-Id2|EOut]) :-
	( Id2 > Id ; Id2 < Id ),
	( Id1 > Id ; Id1 < Id ),
	connected_to(Id, Edges, Ids, EOut).
connected_to(Id, [Id-Id2|Edges], [Id2|Ids], EOut) :-
	( Id2 > Id ; Id2 < Id ), connected_to(Id, Edges, Ids, EOut).
connected_to(Id, [Id2-Id|Edges], [Id2|Ids], EOut) :-
	( Id2 > Id ; Id2 < Id ), connected_to(Id, Edges, Ids, EOut).
connected_to(Id, [Id-Id|Edges], [Id|Ids], EOut) :-
	connected_to(Id, Edges, Ids, EOut).

merge_nodes([Id0|Ids], Data, [Id0-V|Out]) :-
	rm_value(Id0, Data, V0, Data1),
	merge_nodes(Ids, Data1, Vn, Out),
	V is Vn + V0.
merge_nodes([], Data, 0, Data).
merge_nodes([Id|Ids], Data, V, DataOut) :-
	rm_value(Id, Data, V1, DataCur),
	merge_nodes(Ids, DataCur, Vn, DataOut),
	V is V1 + Vn.

rm_value(_, [], 0, []).
rm_value(K, [K-V|Out], V, Out).
rm_value(K, [K1-V1|Out1], V, [K1-V1|Out]) :-
	( K1 > K ; K1 < K ),
	rm_value(K, Out1, V, Out).


ord_insert(_, [], []).
ord_insert(V, VIn, VOut) :-
	VIn = [V1|VRest],
	(	V > V1, append(VOut, [_], [V,V1|VRest])
	;	V =< V1, ord_insert(V, VRest, VOut0), VOut = [V1|VOut0] ).

max_n_vals(KVs, MaxN) :-
	length(MaxN, N), list_of(N, 0, Acc),
	max_n_vals(KVs, Acc, MaxN).
max_n_vals([], MaxN, MaxN).
max_n_vals([_-V|KVs], Acc, MaxN) :-
	ord_insert(V, Acc, Acc1),
	max_n_vals(KVs, Acc1, MaxN).

rows(_, _, []).
rows(M, Stride, [R|Rs]) :-
	append(R, M1, M), length(R, Stride),
	rows(M1, Stride, Rs).

prepare2(Tags, Edges, Volumes) :-
	% input(M, Stride),
	sample(M, Stride),
	basin_tags(M, Stride, Tags),
	tag_edges(Tags, Edges0),
	sort(Edges0, Edges),
	volumes(Tags, Volumes).

time2(N, Tags, Edges, Volumes, EdgesOut, VolOut, Ids) :-
	prepare2(Tags, Edges, Volumes),
	time(time2_(N, Edges, Volumes, EdgesOut, VolOut, Ids)).
time2_(_, [], Volumes, [], Volumes, []).
time2_(0, Edges, Volumes, Edges, Volumes, []).
time2_(N, [Id1-Id2|Edges], Volumes, EdgesOut, VolOut, [Ids|IdsNext]) :-
	N > 0,
	time(connected([Id1,Id2], Edges, Ids0, EdgesCur)),
	sort(Ids0, Ids),
	time(merge_nodes(Ids, Volumes, VolCur)),
	N1 is N - 1,
	time2_(N1, EdgesCur, VolCur, EdgesOut, VolOut, IdsNext).

answer2a(N, Tags, Edges, Volumes, MergedVolumes) :-
	prepare2(Tags, Edges, Volumes),
	merge_graphs(Volumes, Edges, MergedVolumes0),
	rm_value(0, MergedVolumes0, _, MergedVolumes),
	max_n_vals(MergedVolumes, [Max1,Max2,Max3]),
	N is Max1 * Max2 * Max3.

% alternative to collecting all edges and then merging is to check up then left for previous id
% if true then add an edge
all_rows([], _, []).
all_rows(M, Stride, [R|Rs]) :- append(R, M1, M), length(R, Stride), all_rows(M1, Stride, Rs).
gtlt(N1, N2) :- (N1 > N2 ; N1 < N2).
basin_scan(M, Stride, Tags, Edges) :-
	list_of(Stride, 0, T0),
	all_rows(M, Stride, Rows),
	!,
	basin_scan(Rows, 1, T0, [], Tags, Edges).
basin_scan([], _, _, Edges, [], Edges).
basin_scan([R|Rows], NewId, TPrev, EdgeAcc, [T|Tags], Edges) :-
	basin_scan_horz(R, 0, NewId, TPrev, EdgeAcc, T, IdCur, EdgeCur),
	basin_scan(Rows, IdCur, T, EdgeCur, Tags, Edges).
basin_scan_horz([], _, NewId, [], EdgeAcc, [], NewId, EdgeAcc).
basin_scan_horz([9|Ds], _, NewId, [_|TPrev], EdgeAcc, [0|TOut], IdOut, Edges) :-
	basin_scan_horz(Ds, 0, NewId, TPrev, EdgeAcc, TOut, IdOut, Edges).
basin_scan_horz([D|Ds], LastId, NewId, [Tp|TPrev], EdgeAcc, [To|TOut], IdOut, Edges) :-
	D < 9,
	(	Tp > 0, To = Tp,
		(	LastId > 0,
			(	gtlt(LastId, Tp), EdgeCur = [Tp-LastId|EdgeAcc]
			;	LastId = Tp, EdgeCur = EdgeAcc
			)
		;	LastId = 0, EdgeCur = EdgeAcc
		),
		NewId2 = NewId
	;	Tp = 0,
		(	LastId > 0, To = LastId, NewId2 = NewId
		;	LastId = 0, To = NewId, NewId2 is NewId + 1
		),
		EdgeCur = EdgeAcc
	),
	basin_scan_horz(Ds, To, NewId2, TPrev, EdgeCur, TOut, IdOut, Edges).

show_tags([]).
show_tags([[0|Ds]|Rs]) :- write(' '), show_tags([Ds|Rs]).
show_tags([[D|Ds]|Rs]) :- D > 0, write(D), show_tags([Ds|Rs]).
show_tags([[]|Rs]) :- write('\n'), show_tags(Rs).

count9s([], 0).
count9s([9|M], Count) :- count9s(M, Count0), Count is Count0 + 1.
count9s([D|M], Count) :- D < 9, count9s(M, Count).
sum_vals([], 0).
sum_vals([_-V|KVs], Sum) :- sum_vals(KVs, Sum0), Sum is Sum0 + V.

answer2b(N, Ts, Es, Vs, CNon9, C9, VMerged) :-
	input(M, St),
	basin_scan(M, St, Ts, Es0),
	sort(Es0, Es),
	volumes(Ts, Vs),
	count9s(M, C9),
	sum_vals(Vs, CNon9),
	merge_graphs(Vs, Es, VMerged),
	max_n_vals(VMerged, [M1,M2,M3]),
	N is M1*M2*M3.

answer2(N) :- time(answer2b(N, _,_,_, _,_, _)).

