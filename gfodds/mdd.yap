
%%%
%%% Format: 
%%% [node(9, v1, [edge(s1,8), edge(s2,5), edge(s3,7), edge(d,1)])]
%%%



%%%%%%%%%%%%%%%%%%%% MDD CREATION %%%%%%%%%%%%%%%%%%%%%


make_mdd_from_leaf(V, [node(0,V,[])]) :- !.



make_partial_mdd_from_row([Var], Index, [Elem], 0, [node(Index,Var,[edge(Elem,0)]),node(0, 0.0, [])]) :- 
    !.
make_partial_mdd_from_row([Var], Index, [Elem], 1, [node(Index,Var,[edge(Elem,1)]),node(1, 1.0, [])]) :- 
    !.
make_partial_mdd_from_row([Var | Rest], Index, [Value | Other], Leaf, [node(Index, Var, [edge(Value, ChildIndex)]) | M]) :-
    ChildIndex is Index - 1,
    make_partial_mdd_from_row(Rest, ChildIndex, Other, Leaf, M).




make_mdd_from_pred_table(_, _, [], [], M, M) :-
    !.
make_mdd_from_pred_table(Vars, MDepth, [], [Row | Rest], MDDSoFar, M) :-
    make_partial_mdd_from_row(Vars, MDepth, Row, 0, PartialM),
    mdd_apply(MDDSoFar, PartialM, +, MDDSoFar1),
    make_mdd_from_pred_table(Vars, MDepth, [], Rest, MDDSoFar1, M),
    !.
make_mdd_from_pred_table(Vars, MDepth, [Row | Rest], NegLitTable, MDDSoFar, M) :-
    make_partial_mdd_from_row(Vars, MDepth, Row, 1, PartialM),
    mdd_apply(MDDSoFar, PartialM, +, MDDSoFar1),
    make_mdd_from_pred_table(Vars, MDepth, Rest, NegLitTable, MDDSoFar1, M).




make_mdd_from_pred_table([[],[]], [[]], [node(1,1.0,[])]) :- 
    !.
make_mdd_from_pred_table([[]], [[],[]], [node(0,0.0,[])]) :- 
    !.
make_mdd_from_pred_table([Vars | LitTable], [_ | NegLitTable], M) :- 
    length(Vars, VarsLen),
    MDepth is VarsLen + 1,
    recorda(mdd_build, mdd_build, RefMDDMerge),
    make_mdd_from_pred_table(Vars, MDepth, LitTable, NegLitTable, [node(0,0.0,[])], M1),
    erase(RefMDDMerge),
    mdd_apply([node(0,0.0,[])], M1, +, M).


   
   

%%%%%%%%%%%%%%%%%%%%%%%% MDD APPLY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractNewMDDIndex(I) :- 
    recorded(mdd_indexcount, I, RefIndexCount),
    erase(RefIndexCount),
    I1 is I + 1,
    recorda(mdd_indexcount, I1, _).		      





mddVarOrder(V1, V2, eq) :- 
    once(number(V1)),
    number(V2),
    !.
mddVarOrder(V1, V2, gt) :- 
    number(V1),
    !.
mddVarOrder(V1, V2, lt) :- 
    number(V2),
    !.
mddVarOrder(V1, V2, Ord) :- 
    domVarOrder(V1, V2, Ord).



mdd_apply_mult_add_edge([], _, []) :- 
    !.
mdd_apply_mult_add_edge([node(Index,V,EL) | Rest],I-Dir,[node(Index,V,[I-Dir | EL]) | Other]) :- 
    number(V),
    mdd_apply_mult_add_edge(Rest, I-Dir, Other),
    !.
mdd_apply_mult_add_edge([Node | Rest], I-Dir, [Node | Other]) :- 
    mdd_apply_mult_add_edge(Rest, I-Dir, Other).





mdd_apply_op_on_edgeLists(_, EL1, _,  EL2, +, EL) :- 
    batchInsert(EL1, EL2, EL),
    !.
mdd_apply_op_on_edgeLists(_, EL1, _, EL2, -, EL) :- 
    batchInsert(EL1, EL2, EL),
    !.
mdd_apply_op_on_edgeLists(_, EL1, _, EL2, avg, EL) :- 
    batchInsert(EL1, EL2, EL),
    !.
mdd_apply_op_on_edgeLists(N, EL1, N, EL2, max, EL) :- 
    recorded(maxblock_prefer_high, maxblock_prefer_high,_),		      
    sort([EL1,EL2],SortedEL),
    last(SortedEL,EL),
    !.
mdd_apply_op_on_edgeLists(N, EL1, N, EL2, max, EL) :- 
    sort([EL1,EL2],[EL | _]),
    !.
mdd_apply_op_on_edgeLists(N1, EL1, N2, _, max, EL1) :- 
    N1 > N2,
    !.
mdd_apply_op_on_edgeLists(_, _, _, EL2, max, EL2) :- 
    !.
mdd_apply_op_on_edgeLists(1.0, _, _, EL2, *, EL2) :-
    !.
mdd_apply_op_on_edgeLists(0.0, _, _, _, *, []) :-
    !.
mdd_apply_op_on_edgeLists(_, EL1, _, [], div, EL1).





		

mdd_apply(node(I1, _, _), _, node(I2, _, _), _, _, I) :- 
    recorded(mdd_apply, arc(pair(I1, I2), node(I,_,_)), _),
    !.
mdd_apply(node(I1, N1, EL1), M1, node(I2, N2, EL2), M2, Op, I) :- 
    once(number(N1)),
    number(N2),
    !,
    applyOp(N1, N2, Op, N),
    mdd_apply_op_on_edgeLists(N1, EL1, N2, EL2, Op, EL),
    (recorded(mdd_apply, arc(pair(_, _), node(I, N, EL)), _) ->
        (true)
	;
	(extractNewMDDIndex(I),
	 recorda(mdd_apply, arc(pair(I1,I2), node(I, N, EL)), _))),
    !.
mdd_apply(node(I1, Var, Edges1), M1, node(I2, Var, Edges2), M2, Op, I) :- 
%    write(Var), write(' is equal to '), write(Var), nl, 
    findall(EdgeValue, (member(edge(EdgeValue,_),Edges1), member(edge(EdgeValue,_),Edges2)), EdgeValues12),
    findall(EdgeValue, (member(edge(EdgeValue,_),Edges1), not(member(edge(EdgeValue,_),Edges2))), EdgeValues1),
    findall(EdgeValue, (member(edge(EdgeValue,_),Edges2), not(member(edge(EdgeValue,_),Edges1))), EdgeValues2),
    
    findall(edge(EdgeValue,TargetIndex),
	   	(member(EdgeValue,EdgeValues12), 
		   once((member(edge(EdgeValue,ChildIndex1),Edges1),
			 member(node(ChildIndex1,ChildVar1,ChildEdges1), M1),
			 member(edge(EdgeValue,ChildIndex2),Edges2),
			 member(node(ChildIndex2,ChildVar2,ChildEdges2), M2),
			 mdd_apply(node(ChildIndex1,ChildVar1,ChildEdges1),M1,node(ChildIndex2,ChildVar2,ChildEdges2),M2,Op,TargetIndex)))),
	    EdgeTargets12),

    findall(edge(EdgeValue,TargetIndex),
	   	(member(EdgeValue,EdgeValues1), 
		   once((member(edge(EdgeValue,ChildIndex1),Edges1),
			 member(node(ChildIndex1,ChildVar1,ChildEdges1), M1),
			 mdd_apply(node(ChildIndex1,ChildVar1,ChildEdges1),M1,node(0,0.0,[]),[node(0,0.0,[])],+,TargetIndex)))),
	    EdgeTargets1),

    findall(edge(EdgeValue,TargetIndex),
	   	(member(EdgeValue,EdgeValues2), 
		   once((member(edge(EdgeValue,ChildIndex2),Edges2),
			 member(node(ChildIndex2,ChildVar2,ChildEdges2), M2),
			 mdd_apply(node(0,0.0,[]),[node(0,0.0,[])],node(ChildIndex2,ChildVar2,ChildEdges2),M2,+,TargetIndex)))),
	    EdgeTargets2),
    
    conc(EdgeTargets12, EdgeTargets1, EdgeTargets121),
    conc(EdgeTargets121, EdgeTargets2, EdgeTargets),

    (((recorded(mdd_build, mdd_build, _) ; (member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2))) -> 
      (sort(EdgeTargets, Edges),
       ((recorded(mdd_apply, arc(pair(_,_), node(I,Var,Edges)), _)) ->
	     (true)
	     ;
	     (extractNewMDDIndex(I))),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,Var,Edges)),_))
      ;
      (member(edge(_, I), EdgeTargets),
       recorded(mdd_apply,arc(pair(_,_),node(I,ChildVar,Edges)),_),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,ChildVar,Edges)),_))),
    !.		
mdd_apply(node(I1, Var1, Edges1), M1, node(I2, Var2, Edges2), M2, Op, I) :-  
    once(mddVarOrder(Var1, Var2, Ord)),
    Ord = lt,
	!,
%    write(Var1), write(' is less than '), write(Var2), nl, 	
    findall(edge(EdgeValue, TargetIndex), 
    		(member(edge(EdgeValue, ChildIndex), Edges1), 
    		    once((member(node(ChildIndex, ChildVar, ChildEdgeList), M1), 
		          mdd_apply(node(ChildIndex, ChildVar, ChildEdgeList), M1, node(I2, Var2, Edges2), M2, Op, TargetIndex)))), 
		EdgeTargets),
    (((recorded(mdd_build, mdd_build, _) ; (member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2))) -> 
      (sort(EdgeTargets, Edges),
       ((recorded(mdd_apply, arc(pair(_,_), node(I,Var1,Edges)), _)) ->
	     (true)
	     ;
	     (extractNewMDDIndex(I))),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,Var1,Edges)),_))
      ;
      (member(edge(_, I), EdgeTargets),
       recorded(mdd_apply,arc(pair(_,_),node(I,Var,Edges)),_),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,Var,Edges)),_))),
    !.          
mdd_apply(node(I1, Var1, Edges1), M1, node(I2, Var2, Edges2), M2, Op, I) :- 
%    write(Var1), write(' is greater than '), write(Var2), nl,
    findall(edge(EdgeValue, TargetIndex), 
    		(member(edge(EdgeValue, ChildIndex), Edges2), 
    		    once((member(node(ChildIndex, ChildVar, ChildEdgeList), M2), 
		          mdd_apply(node(I1, Var1, Edges1), M1, node(ChildIndex, ChildVar, ChildEdgeList), M2, Op, TargetIndex)))), 
		EdgeTargets),
    (((recorded(mdd_build, mdd_build, _) ; (member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2))) -> 
      (sort(EdgeTargets, Edges),
       ((recorded(mdd_apply, arc(pair(_,_), node(I,Var2,Edges)), _)) ->
	     (true)
	     ;
	     (extractNewMDDIndex(I))),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,Var2,Edges)),_))
      ;
      (member(edge(_, I), EdgeTargets),
       recorded(mdd_apply,arc(pair(_,_),node(I,Var,Edges)),_),
       recorda(mdd_apply,arc(pair(I1,I2),node(I,Var,Edges)),_))).          
    





mdd_apply1(Root1, M1, Root2, M2, Op, Res) :- 
    eraseall(mdd_indexcount),	      
    recorda(mdd_indexcount, 2, RefIndexCount),
    mdd_apply(Root1, M1, Root2, M2, Op, Root),
    eraseall(mdd_indexcount),
    all(X, recorded(mdd_apply, arc(_, X), _), L),
    eraseall(mdd_apply),
    sort(L, Res1),
    reverse(Res1, Res).	


    
mdd_apply(M1, M2, Op, Res) :- 
    M1 = [Root1 | _],
    M2 = [Root2 | _],
    mdd_apply1(Root1,M1,Root2,M2,Op,Res).





mdd_chain_apply1([], _, [_,M], _, M) :- 
    !.
mdd_chain_apply1([[Node, M1] | Rest], IndexStart, [RootSoFar,MSoFar], Op, M) :-
    eraseall(mdd_indexcount),	      
    recorda(mdd_indexcount, IndexStart, RefIndexCount),
    mdd_apply(Node, M1, RootSoFar, MSoFar, Op, _),
    eraseall(mdd_indexcount),
    all(X, recorded(mdd_apply, arc(_, X), _), L),
    eraseall(mdd_apply),
    sort(L, Res1),
    reverse(Res1, MSoFar1),	
    MSoFar1 = [RootSoFar1 | _],
    RootSoFar1 = node(IndexLeap, _, _),
    IndexStart1 is IndexLeap + 1,
    mdd_chain_apply1(Rest, IndexStart1, [RootSoFar1, MSoFar1], Op, M).




mdd_chain_apply([], M, _, M) :- 
    !.
mdd_chain_apply([M1 | Rest], MSoFar, Op, M) :-
    mdd_apply(M1, MSoFar, Op, MSoFar1),
    mdd_chain_apply(Rest, MSoFar1, Op, M).




mdd_chain_apply(MList, Op, M) :-
    MList = [First | Rest],
    mdd_chain_apply(Rest, First, Op, M).





%%%%%%%%%%%%%%%%%%%%%%% MARGINALIZE %%%%%%%%%%%%%%%%



mdd_edge_parentUpdate([], _, _, []) :- 
    !.
mdd_edge_parentUpdate([edge(E,SourceIndex) | Rest], SourceIndex, TargetIndex, [edge(E,TargetIndex) | Other]) :-
    mdd_edge_parentUpdate(Rest, SourceIndex, TargetIndex, Other),
    !.
mdd_edge_parentUpdate([edge(E,I) | Rest], SourceIndex, TargetIndex, [edge(E,I) | Other]) :-
    mdd_edge_parentUpdate(Rest, SourceIndex, TargetIndex, Other).



mdd_parent_update([], _, _, []) :- 
    !.
mdd_parent_update([node(I,V,EL) | Rest], SourceIndex, TargetIndex, [node(I,V,EL) | Other]) :- 
    number(V),
    mdd_parent_update(Rest, SourceIndex, TargetIndex, Other),
    !.
mdd_parent_update([node(I,V,EL) | Rest], SourceIndex, TargetIndex, [node(I,V,EL1) | Other]) :- 
    mdd_edge_parentUpdate(EL, SourceIndex, TargetIndex, EL1),
    mdd_parent_update(Rest, SourceIndex, TargetIndex, Other).


mdd_skip(M, [], M) :- 
    !.
mdd_skip(M, [[Source, Target] | Rest], Res) :- 
    mdd_parent_update(M, Source, Target, M1),
    mdd_skip(M1, Rest, Res).
        
    
mdd_skip(M, [], AllVarIndices, []) :- 
    !.
mdd_skip(M, [Val | Rest], AllVarIndices, [Res | RestrictedMs]) :- 
    all([I,Target], (member(I, AllVarIndices), once((member(node(I,_,EL),M), member(edge(Val,Target), EL)))), STPairs),
    mdd_skip(M, STPairs, Res),
    mdd_skip(M, Rest, AllVarIndices, RestrictedMs).








mdd_aggregate_over_variable(Var, M, Res) :-
    M = [node(_, Var, EL) | _],
    findall([node(I,V,EL1),M], (member(edge(_,I), EL), once(member(node(I,V,EL1), M))), RestrictedMs),
    (name(Var, [118 | _]) ->
        (mdd_chain_apply(RestrictedMs, max, Res))
	;
	(mdd_chain_apply(RestrictedMs, +, Res1),
	 length(RestrictedMs, Len),
	 mdd_apply(Res1, [node(2,Len,[])], div, Res))),
     !.
mdd_aggregate_over_variable(Var, M, Res) :-
    all(I, member(node(I,Var,_), M), AllVarIndices),
    !,
    AllVarIndices = [First | _],
    member(node(First,Var,EL), M),
    all(Val, member(edge(Val,_),EL), AllVarValues),
    mdd_skip(M, AllVarValues, AllVarIndices, RestrictedMs),    
    (name(Var, [118 | _]) ->
        (mdd_chain_apply(RestrictedMs, max, Res))
	;
	(mdd_chain_apply(RestrictedMs, +, Res1),
	 length(RestrictedMs, Len),
	 mdd_apply(Res1, [node(2,Len,[])], div, Res))),
    !.	
mdd_aggregate_over_variable(_, M, M).    			       




mdd_eliminate_all_variables([], M, M) :- 
    !.
mdd_eliminate_all_variables([Var | Rest], M, Res) :-
    mdd_aggregate_over_variable(Var, M, M1),
    mdd_eliminate_all_variables(Rest, M1, Res).




mdd_optimize(Index, M, OptimizedM) :- 
    recorded(varToElim, varToElim(Index, VarsToElim), _),
%   write('Vars to elim: '), write(VarsToElim), nl,
    findall(X, (member(X,VarsToElim), once(avgVar(X))), AvgVarsToElim1),
    sortDVarsRev(AvgVarsToElim1, AvgVarsToElim),
    mdd_eliminate_all_variables(AvgVarsToElim, M, AvgVarElimM),
%      	         write('AvgVarElimTable='), write(AvgVarElimTable), nl, 
%    write('Avg Var Elim M: '), write(AvgVarElimM), nl, 
    recorded(allDVars, AllDVars, _),
    ((member(X, AllDVars), once(avgVar(X)), once(not(member(X, AvgVarsToElim)))) ->
        (OptimizedM = AvgVarElimM)
	;
	(diff(VarsToElim, AvgVarsToElim, MaxVarsToElim1),
    	 sortDVarsRev(MaxVarsToElim1, MaxVarsToElim),
	 mdd_eliminate_all_variables(MaxVarsToElim, AvgVarElimM, OptimizedM))).



%%%%%%%%%%%% RECURSIVE OPTIMIZATION %%%%%%%%%%%%%%%%%
	     


mdd_recursive_eliminate_variable([], _, M, Res) :- 
    mdd_apply(M, [node(0,0.0,[])], +, Res),
    !.
mdd_recursive_eliminate_variable([node(Index,Var,EL) | Rest], IndexStart, M, Res) :- 
    findall([node(I,Var1,EL1), M], (member(edge(_,I), EL), member(node(I,Var1,EL1), M)), Targets),
    !,
    Targets = [First | OtherTargets],
    (avgVar(Var) ->
        (mdd_chain_apply1(OtherTargets, IndexStart, First, +, M1),
	 M1 = [Root1 | _],
	 length(Targets, Len),
	 mdd_chain_apply1([[Root1,M1]], IndexStart, [node(2,Len,[]),[node(2,Len,[])]], div, M2))
	; 
        mdd_chain_apply1(Targets, IndexStart, First, max, M2)),
    M2 = [node(NewIndex,_,_) | _],
    conc(M, M2, M3),
    mdd_parent_update(M3, Index, NewIndex, M4),
    length(M4, M4Len),
    IndexStart1 is M4Len + 5,
    mdd_recursive_eliminate_variable(Rest, IndexStart1, M4, Res),
    !.
mdd_recursive_eliminate_variable(_, _, M, M).






mdd_recursive_eliminate_all_variables([], M, M) :- 
    !.
mdd_recursive_eliminate_all_variables([Var | Rest], M, OptimizedM) :- 
    M = [node(_, Var, EL) | MRest],
    findall([node(I,Var1,EL1) | MRest], (member(edge(_,I), EL), member(node(I,Var1,EL1), M)), Targets),
%    Targets = [First | OtherTargets],
    (avgVar(Var) ->
        (mdd_chain_apply(Targets, +, M1),
    	 M1 = [Root1 | _],
    	 length(Targets, Len),
    	 mdd_apply(M1, [node(2,Len,[])], div, OptimizedM))
    	; 
        mdd_chain_apply(Targets, max, OptimizedM)),
    !.
mdd_recursive_eliminate_all_variables([Var | Rest], M, OptimizedM) :- 
    findall(node(Index, Var, EL), member(node(Index, Var, EL), M), AllVarNodes),
    M = [node(RootIndex,_,_) | _],
    IndexStart is RootIndex + 1,
    mdd_recursive_eliminate_variable(AllVarNodes, IndexStart, M, M1),
    mdd_recursive_eliminate_all_variables(Rest, M1, OptimizedM).



all_avg_vars_not_eliminated(AllDVars, AvgVarsToElim) :- 
    member(X, AllDVars), 
    once(avgVar(X)), 
    once(not(member(X, AvgVarsToElim))).

	     
mdd_optimize_recursive(Index, M, OptimizedM) :- 
    recorded(varToElim, varToElim(Index, VarsToElim), _),
%    write('Vars to elim: '), write(VarsToElim), nl, 
    findall(X, (member(X,VarsToElim), once(avgVar(X))), AvgVarsToElim1),
    sortDVarsRev(AvgVarsToElim1, AvgVarsToElim),
    mdd_recursive_eliminate_all_variables(AvgVarsToElim, M, AvgVarElimM),
%    write('Avg Var Elim M: '), write(AvgVarElimM), nl, 
    recorded(allDVars, AllDVars, _),
    (all_avg_vars_not_eliminated(AllDVars, AvgVarsToElim) ->
        (OptimizedM = AvgVarElimM)
	;
	(diff(VarsToElim, AvgVarsToElim, MaxVarsToElim1),
    	 sortDVarsRev(MaxVarsToElim1, MaxVarsToElim),
	 mdd_recursive_eliminate_all_variables(MaxVarsToElim, AvgVarElimM, OptimizedM))).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_mdd_apply :-
    recorda(mdd_build, mdd_build, _),	       
    mdd_apply([node(0,0.0,[])],[node(3,v1,[edge(a,2)]),node(2,v2,[edge(b,1)]),node(1,1.0,[])],+,M),
    eraseall(mdd_build),	       
    nl,nl,write(M),nl,nl.	

		

test_mdd_make_partial :-
    make_partial_mdd_from_row([v1,v2], 3, [a,b], 1, M),
    write(M),nl.




test_mdd_make :-
    LitTable = [[v1,v2],[a,b],[a,c],[c,c]], 
    NegLitTable = [[v1,v2],[a,a],[b,a],[b,b],[b,c],[c,a],[c,b]],
    make_mdd_from_pred_table(LitTable, NegLitTable, M),
    write(M),nl.





test_mdd4 :-
    LitTable = [[b1,v3],[s1,t1],[s2,t2]],
    NegLitTable = [[b1,v3],[s1,t2],[s2,t1],[s3,t1],[s3,t2],[s4,t1],[s4,t2],[d,t1],[d,t2]],
    make_mdd_from_pred_table(LitTable, NegLitTable, M1),
    ppmdd(M1),nl,
    mdd_apply([node(1,1.0,[])],M1,-,M2),
    ppmdd(M2),nl,
    M3 = [node(3,b1,[edge(d,1),edge(s1,2),edge(s2,2),edge(s3,2),edge(s4,2)]),node(2,0.6,[3-t]),node(1,0.0,[3-f])],
    M4 = [node(0,0.0,[])],
    mdd_apply_mult_add_edge(M3,4-t,M5),
    mdd_apply_mult_add_edge(M4,4-f,M6),
    mdd_apply(M1,M5,*,M7),
    ppmdd(M7),nl, 
    mdd_apply(M2,M6,*,M8),	
    ppmdd(M8),nl,    
    mdd_apply(M7,M8,+,M9),
    write(M9),nl,
    ppmdd(M9),nl.    






test_mdd5 :-
    LitTable = [[b1],[s1],[s2],[d]],
    NegLitTable = [[b1],[s3],[s4]],
    make_mdd_from_pred_table(LitTable, NegLitTable, M1),
    ppmdd(M1),nl,
    mdd_apply([node(1,1.0,[])],M1,-,M2),
    ppmdd(M2),nl,
    M3 = [node(5,b1,[edge(d,1),edge(s1,3),edge(s2,4),edge(s3,1),edge(s4,1)]),node(4,v3,[edge(t1,1),edge(t2,2)]),node(3,v3,[edge(t1,2),edge(t2,1)]),node(2,0.6,[4-t,3-t]),node(1,0.0,[4-f])],
    M4 = [node(0,0.6,[])],
    mdd_apply_mult_add_edge(M3,5-t,M5),
    mdd_apply_mult_add_edge(M4,5-f,M6),
    mdd_apply(M1,M5,*,M7),
    ppmdd(M7),nl, 
    mdd_apply(M2,M6,*,M8),	
    ppmdd(M8),nl,    
    mdd_apply(M7,M8,+,M9),
    write(M9),nl,
    ppmdd(M9),nl.    
    

test_mdd6 :-
    M = [node(8,v1,[edge(a,4),edge(b,7),edge(c,6)]), node(7,b1,[edge(x,4),edge(y,4),edge(z,2)]), node(6,b1,[edge(x,2), edge(y,3), edge(z,4)]), node(4,4.0,[]), node(3,3.0,[]), node(2,2.0,[])],
    mdd_aggregate_over_variable(v1,M,M1),
    ppmdd(M1).






