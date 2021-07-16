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




domVarOrder(X, N1, X, N2, Ord) :- 
    name(R1,N1),
    name(R2,N2),	
    (number(R1) ->
	((R1 < R2) ->
	    (Ord = lt)
	    ;
	    (Ord = gt))
	;
	((R1 @< R2) ->
	    (Ord = lt)
	    ;
	    (Ord = gt))),	
    !.
domVarOrder(118, _, _, _, lt) :- 
    !.
domVarOrder(_, _, 118, _, gt) :- 
    !. 
domVarOrder(98, _, 120, _, gt) :- 
    !. 
domVarOrder(120, _, 98, _, lt).





getVarOrder(V, V, eq) :- 
    !.
getVarOrder(V1, V2, Ord) :-
    once(domConst(V1)),
    domConst(V2),
    ((V1 @< V2) ->
	(Ord = lt)
	;
	(Ord = gt)),
    !.
getVarOrder(V1, V2, lt) :-
    domConst(V1),
    !.
getVarOrder(V1, V2, gt) :-
    domConst(V2),
    !.
getVarOrder(V1, V2, Ord) :-		
    name(V1, [Type1 | N1]),
    name(V2, [Type2 | N2]),
    domVarOrder(Type1, N1, Type2, N2, Ord).






get_max_prec_var([], Var, Var) :- 
    !. 
get_max_prec_var([Var1 | Rest], MinSoFar, Var) :- 
    getVarOrder(Var1, MinSoFar, Ord),
    (Ord == gt ->
	(MinSoFar1 = Var1)
	;
	(MinSoFar1 = MinSoFar)),
    get_max_prec_var(Rest, MinSoFar1, Var).



get_max_prec_var(VarList, Var) :- 
    VarList = [Var1 | Rest],
    get_max_prec_var(Rest, Var1, Var). 	 
    



getArgListOrder([], [], eq) :- 
    !. 
getArgListOrder(_, [], lt) :- 
    !.
getArgListOrder([], _, gt) :- 
    !.
getArgListOrder(Args1, Args2, Ord) :- 
    get_max_prec_var(Args1, Var1),
    get_max_prec_var(Args2, Var2),	
    getVarOrder(Var1, Var2, Ord1),
    ((Ord1 == eq) ->
	(del(Var1, Args1, Args11),
	 del(Var2, Args2, Args22),
	 getArgListOrder(Args11, Args22, Ord))
	;
	(Ord = Ord1)).



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
order(eq(a,X),eq(a,Y),Ord) :- 
    getVarOrder(X,Y,Ord),
    !.	
order(eq(a,_),L2,lt) :- 
    !.		     
order(L1,eq(a,_),gt) :- 
    !.		     
order(L1, L2, Order) :- 
    not(number(L1)),
    not(number(L2)),
    L1 =.. [P1 | Args1],
    L2 =.. [P2 | Args2],
    getArgListOrder(Args1, Args2, Ord1),
    !,	
    ((Ord1 == eq) ->    
         (predOrder(PredOrd),
          getPredOrder(P1, P2, PredOrd, PO),
	  !,
          (PO = eq ->
             (getVarListOrder(Args1, Args2, Order))
             ;
             (Order = PO)))
	 ;   
	 (Order = Ord1)).











test_prec :-
    D = [node(7,eq(b1,xl),6,2),node(6,empty(b1),5,0),node(5,tfull(xh),4,1),node(4,tin(xh,b1),3,1),node(3,shop(b1),0,1),node(2,empty(b1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)],
    pp(D),
    reorder_diagram(D,R),
    pp(R).
