start_A_star( InitState, MaxStepsNum, NNodesNum, PathCost) :-
	score(InitState, 0, 0, InitCost, InitScore) ,
	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], 1, MaxStepsNum, NNodesNum, [ ], PathCost) .

search_A_star(Queue, StepCounter, MaxStepsNum, NNodesNum, ClosedSet, PathCost) :-
	StepCounter > MaxStepsNum, !,
	write('Step nr: '), write(StepCounter), nl,
	write('Steps number exceed steps limit, do you want to increase it? (y/n)\n'),
	read('y'),
	write('How many steps add to limit?\n'),
	read(AddToLimit),
	NewMaxStepsNum is MaxStepsNum + AddToLimit,
	search_A_star(Queue, StepCounter, NewMaxStepsNum, NNodesNum, ClosedSet, PathCost).

search_A_star(Queue, StepCounter, MaxStepsNum, NNodesNum, ClosedSet, PathCost) :-
	write('Step nr: '), write(StepCounter), nl,
	NewStepCounter is StepCounter + 1,
	write('Top '), write(NNodesNum), write(' nodes with smallest heuristic value\n'),
	write_n_nodes(NNodesNum, Queue),
	write('Enter nodes order\n'),
	read(NNodes),
	fetch_node_index(NNodes, NodeIndex),
	fetch(Node, NodeIndex, Queue, ClosedSet , RestQueue),
	write('\nYou\'re currently at node:\n'),
	write_node(Node), nl,
	continue(Node, NewStepCounter, MaxStepsNum, NNodesNum,RestQueue, ClosedSet, PathCost).


continue(node(State, Action, Parent, Cost, _), _, _, _, _, ClosedSet, path_cost(Path, Cost) ) :-
	goal( State), !,
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .

continue(Node, StepCounter, MaxStepsNum, NNodesNum, RestQueue, ClosedSet, Path)   :-
	expand( Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	search_A_star(NewQueue, StepCounter, MaxStepsNum, NNodesNum, [Node | ClosedSet ], Path).


write_node(node(State, Action, _, _, Score)) :-
	write('state: '), write(State), nl,
	write('action: '), write(Action), nl,
	write('total heuristic cost: '), write(Score), nl.

write_n_nodes(_, []) :- !.

write_n_nodes(NodesNum, _) :-
	NodesNum =< 0, !.

write_n_nodes(NodesNum, [Node | RestQueue]) :-
	NewNodesNum is NodesNum - 1,
	write_node(Node),
	write('--------------------------'), nl,
	write_n_nodes(NewNodesNum, RestQueue).

fetch_node_index([NodeIndex | _], NodeIndex).

fetch_node_index([_ | Rest], NodeIndex) :-
	fetch_node_index(Rest, NodeIndex).
	
fetch(node(State, Action,Parent, Cost, Score), 
			Ind, [node(State, Action,Parent, Cost, Score) | RestQueue], ClosedSet, RestQueue) :-
	Ind == 0,
	\+ member(node(State, _, _, _, _), ClosedSet).

fetch(Node, Ind, [Y | Queue], ClosedSet, [Y | RestQueue]) :-
	NewInd is Ind - 1,
	fetch(Node, NewInd, Queue, ClosedSet, RestQueue).

expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-
	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore)),
			NewNodes).


score(State, ParentCost, StepCost, Cost, FScore)  :-
	Cost is ParentCost + StepCost ,
	hScore(State, HScore),
	FScore is Cost + HScore.


insert_new_nodes( [ ], Queue, Queue).

insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes( RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [ ], [Node]) :- !.

insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-
	FScore >= FScore1,  ! ,
	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).

insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]).


build_path(node(nil, _, _, _, _), _, Path, Path) :- !.

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path) :-
	del(Nodes, node(EndState, Action, Parent, _, _), Nodes1) ,
	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1, [Action/EndState|PartialPath],Path).


del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).
	

succ(a, ab, 3, b).
succ(a, aj, 4, j).
succ(a, ag, 1, g).
succ(b, bd, 10, d).
succ(j, jd, 3, d).
succ(j, jg, 6, g).
succ(g, ge, 14, e).
succ(g, gf, 8, f).
succ(d, dh, 11, h).
succ(e, ef, 2, f).
succ(f, fh, 4, h).
succ(e, ei, 1, i).
succ(f, fi, 2, i).
succ(i, ic, 10, c).
succ(i, ih, 6, h).
succ(h, hc, 3, c).
succ(i, ig, 5, g).

goal(c).

hScore(a, 15).
hScore(b, 20).
hScore(c, 0).
hScore(d, 13).
hScore(e, 20).
hScore(f, 5).
hScore(g, 13).
hScore(h, 1).
hScore(i, 6).
hScore(j, 15).