%% getPredOrder(+, +, +, -)
%% getPredOrder(P1, P2, L, O)
%% True when P1 < P2 and O = lt, or P1 > P2 and O = gt, or P1 = P2 and O = eq. 
%% The <>= relation is defined by the user defined list L. The predicate name 
%% appearind earlier in the list is smaller. 

getPredOrder(P, P, _, eq) :-
    !.
getPredOrder(P1, _, [P1 | _], lt) :- 
    !.
getPredOrder(_, P2, [P2 | _], gt) :- 
    !.
getPredOrder(P1, P2, [_ | Rest], O) :-
    getPredOrder(P1, P2, Rest, O).




domVarOrder(V1, V2, Ord) :- 
    name(V1, [T1 | N1]),
    name(V2, [T2 | N2]),
    ((T1 == 98, T2 == 118),
     Ord = gt
     ;
     (T1 == 118, T2 == 98),
     Ord = lt
     ;
     name(X, N1),
     name(Y, N2),
     (X < Y -> Ord = lt ; Ord = gt)).




%% getVarOrder(+, +, -)
%% getVarOrder(V1, V2, O)
%% True when O expresses the relation (gt, lt or eq) of V1 to V2. Domain constants 
%% are smaller than action parameters that are smaller that domain variables. If 
%% both V1 and V2 are of the same type then the ordering is lexicographical. 

getVarOrder1(V, V, eq) :- 
    !.
getVarOrder1(V1, V2, Ord) :- 
    domConst(V1),
    (once(domConst(V2)),
     V1 @> V2,
     Ord = gt
     ;
     Ord = lt),
    !.
getVarOrder1(_, V2, gt) :- 
    domConst(V2),
    !.
getVarOrder1(V1, V2, Ord) :- 
    domVar(V1),
    (domVar(V2),
     domVarOrder(V1, V2, Ord)
     ;
     Ord = lt),
    !.
getVarOrder1(_, V2, gt) :- 
    domVar(V2),
    !.
getVarOrder1(V1, V2, lt) :-
    V1 @< V2,
    !.
getVarOrder1(V1, V2, gt) :- 
    V1 @> V2.



getVarOrder(V1, V2, Ord) :-		
    once(getVarOrder1(V1, V2, Ord1)),
    !,
    Ord = Ord1.

%% getVarListOrder(+, +, -)
%% getVarListOrder(L1, L2, O)
%% True when O describes the relation (gt, lt, or eq) between lists L1 and L2 of 
%% literal parameters. The relation between individual elements is decided by 
%% getVarOrder(+, +, -).

getVarListOrder([], [], eq) :- 
    !.
getVarListOrder([V1 | Rest1], [V2 | Rest2], Order) :- 
    getVarOrder(V1, V2, Ord),
    (Ord = eq,
     !,
     getVarListOrder(Rest1, Rest2, Order)
     ;
     Order = Ord).





get_b_order(V1, V2, BOrder) :- 
    ((member(X,V1),avgVar(X)) ->
        ((member(Y,V2),avgVar(Y)) ->
	    (BOrder = eq) 
	    ;
	    (BOrder = gt))
        ;
     	((member(Y,V2),avgVar(Y)) ->
	    (BOrder = lt)
	    ;
	    (BOrder = eq))).





get_arg_list_order([], [], eq) :- 
    !.



get_lit_order_by_arg_list(V1, V2, Order) :- 
    sortDVars(V1, SortedV1),
    sortDVars(V2, SortedV2).
    



%% order(+, +, -)
%% order(L1, L2, O)
%% True when L1 and L2 are terms and O describes the relation between them. 

order(L1, L2, gt) :- 
    number(L1), 
    not(number(L2)),
    !.
order(L1, L2, lt) :- 
    number(L2),
    not(number(L1)),
    !.

order(eq(a,_),L2,lt) :- 
    not((L2 =.. [eq | [a,_]])),
    !.		     
order(L1,eq(a,_),gt) :- 
    not((L1 =.. [eq | [a,_]])),
    !.		     
order(L1, L2, Order) :- 
    not(number(L1)),
    not(number(L2)),
    L1 =.. [P1 | V1],
    L2 =.. [P2 | V2],
    get_b_order(V1, V2, BOrder),
    (BOrder == eq ->    
         (predOrder(Ord),
          getPredOrder(P1, P2, Ord, PO),
          !,
          (PO = eq,
           !,
           getVarListOrder(V1, V2, Order)
           ;
           Order = PO))
	 ;   
	 (Order = BOrder)).


