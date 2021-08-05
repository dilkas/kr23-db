
django_separate_nodes([], [], []) :- 
    !.
django_separate_nodes([[X, Y] | Rest], [X | XRest], [Y | YRest]) :- 
    django_separate_nodes(Rest, XRest, YRest).



django_abstract_my_intersection(_, [], []) :- 
    !.
django_abstract_my_intersection(L1, [X | Rest], [X | L]) :- 
    abstract_member(X, L1),
    django_abstract_my_intersection(L1, Rest, L),
    !.
django_abstract_my_intersection(L1, [_ | Rest], L) :- 
    django_abstract_my_intersection(L1, Rest, L).




django_impose_arc_consistency([X, Y, Node1, List1], [Z, W, Node2, List2], [X, Y, Node1, Node1List], [Z, W, Node2, Node2List]) :- 
    all([Node1, Node2], (member(Node1, List1), member(Node2, List2)), NodePairs),
    django_separate_nodes(NodePairs, Node1List, Node2List).



django_impose_arc_consistency_on_set(Node, [], Node, []) :- 
    !.
django_impose_arc_consistency_on_set(Node, [First | Rest], RevisedNode, [First1 | Rest1]) :- 
    django_impose_arc_consistency(Node, First, Node1, First1),
    django_impose_arc_consistency_on_set(Node1, Rest, RevisedNode, Rest1).




django_arc_consistency([], []) :- 
    !.
django_arc_consistency([First | Rest], [First1 | Result]) :- 
    django_impose_arc_consistency_on_set(First, Rest, First1, Rest1),
    django_arc_consistency(Rest1, Result).



django_node_consistency([], []) :- 
    !.
django_node_consistency([[Index, ConstrCount, X, XList] | Rest], [[Index, IndexCount, X, XList1] | Rest1]) :- 
    all(X, member(X, XList), XList1),
    django_node_consistency(Rest, Rest1).




django_get_constraint_count_for_pair(ConstrCount1, P1, ConstrCount2, P2, NewConstrCount1, NewConstrCount2) :-
    P1 =.. [_ | V1],
    P2 =.. [_ | V2],
    django_abstract_my_intersection(V1, V2, V3),
    (all(X, (member(X,V3), once(var(X))), V),
     NewConstrCount1 is ConstrCount1,
     NewConstrCount2 is ConstrCount2
     ;
     NewConstrCount1 is ConstrCount1 - 1.0,
     NewConstrCount2 is ConstrCount2 - 1.0).




django_get_constraint_count_for_node(ConstrCount, _, [], ConstrCount, []) :- 
    !.
django_get_constraint_count_for_node(ConstrCount, P, [[Index, FirstConstrCount, FirstP, FirstPList] | Rest], NewConstrCount, [[Index, NewFirstConstrCount, FirstP, FirstPList] | Rest1]) :- 
    django_get_constraint_count_for_pair(ConstrCount, P, FirstConstrCount, FirstP, ConstrCount1, NewFirstConstrCount),
    django_get_constraint_count_for_node(ConstrCount1, P, Rest, NewConstrCount, Rest1).






django_get_constraint_count([], []) :- 
    !.
django_get_constraint_count([[Index, ConstrCount, P, PList] | Rest], [[Index, NewConstrCount, P, PList] | Rest2]) :- 
    django_get_constraint_count_for_node(ConstrCount, P, Rest, NewConstrCount, Rest1),
    django_get_constraint_count(Rest1, Rest2).





django_get_order_indices([], []) :- 
    !.
django_get_order_indices([[Index, ConstrCount, P, PList] | Rest], [[NewIndex, NewConstrCount, P, PList] | Rest2]) :-
    length(PList, ListLen),
    !,
    ListLen > 0.0,
    django_get_constraint_count([[Index, ConstrCount, P, List] | Rest], [[Index, NewConstrCount, P, PList] | Rest1]),
    NewIndex1 is NewConstrCount/ListLen,
    NewIndex is NewIndex1 * -1.0,
    !,
    django_get_order_indices(Rest1, Rest2).



django_reset_constraint_count([], []) :-
    !.
django_reset_constraint_count([[Index, _, P, PList] | Rest], [[Index, 0.0, P, PList] | Rest1]) :-
    django_reset_constraint_count(Rest, Rest1).




django_get_order1(Nodes, OrderedNodes) :- 
    django_reset_constraint_count(Nodes, Nodes1),
    django_get_order_indices(Nodes1, NodeIndices),
    sort(NodeIndices, OrderedNodes).
    


django_get_order_indices1([], []) :- 
    !.
django_get_order_indices1([[Index, ConstrCount, P, PList] | Rest], [[Index1, ConstrCount, P, PList] | Rest1]) :-
    P =.. [_ | V],
    get_unbound_count(V, 0, Index1),
    django_get_order_indices1(Rest, Rest1).
    

django_get_order(Nodes, OrderedNodes) :-
    django_get_order_indices1(Nodes, IndexedNodes),
    sort(IndexedNodes, OrderedNodes).




django_subsumes([]) :- 
    !.
django_subsumes([[Index, ConstrCount, X, XList] | Rest]) :-     
    member(X, XList),
    once(django_node_consistency(Rest, Rest1)),
    once(django_get_order(Rest1, Rest2)),
    django_subsumes(Rest2).





django_get_node_lists([], _, []) :- 
    !.
django_get_node_lists([Node1 | Rest], Ex, [[0.0, 0.0, Node1, Node1List] | Rest1]) :- 
    all(Node1, member(Node1, Ex), Node1List),
    django_get_node_lists(Rest, Ex, Rest1).



django_subsumption(Path, Ex) :- 
    django_get_node_lists(Path, Ex, AllNodes),
    django_arc_consistency(AllNodes, AllArcConsistentNodes),
    !,
    django_get_order(AllArcConsistentNodes, OrderedNodes),
    !,
    django_subsumes(OrderedNodes).
    
