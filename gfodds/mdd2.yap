
%%%
%%% Format: 
%%% [node(9, v1, [edge(s1,8), edge(s2,5), edge(s3,7), edge(d,1)])]
%%%



%%%%%%%%%%%%%%%%%%%% MDD CREATION %%%%%%%%%%%%%%%%%%%%%


make_node_mdd_from_leaf(Index, V) :- 
    number_chars(Index,IndexChars), 
    atom_concat([nodeMDD,- | IndexChars], M),
    recorda(M, node(V,[]), Ref),
    recorda(M, root(Ref), _).




make_mdd_for_pred([], VarVarLit, LitList, LitM, _, _, Ref) :- 
    (member(VarVarLit, LitList) ->
        recorda(LitM, node(1.0,[]), Ref)
	;
	recorda(LitM, node(0.0,[]), Ref)),
    !.
make_mdd_for_pred([Var | Rest], VarVarLit, LitList,  LitM, Key, BindingList, Ref) :- 
    member(Var:ObjList, BindingList),
    findall(edge(Obj,ChildRef), (member(Obj, ObjList), once((member(Var:Obj, Key), make_mdd_for_pred(Rest, VarVarLit, LitList, LitM, Key, BindingList, ChildRef)))), EL),
    recorda(LitM, node(Var,EL), Ref).





make_mdd_for_pred(Lit, LitList, LitM) :- 
    Lit =.. [Pred | Vars1],
    (all(X, (member(X,Vars1), once(domVar(X))), LitVars) ; LitVars = []),
    sortDVarsRev(LitVars, Vars2),
    reverse(Vars2, Vars),
    findall(X:ObjList, (member(X,Vars), once(recorded(varTypes, varTypes(X, ObjList), _))), BindingList),
    getSubsKey(Vars, Key),		    
    variablizeNumList(Vars1, Key, VarVarList),
    VarVarLit =.. [Pred | VarVarList],
    atom_concat([LitM, '1'], LitM1),		       
    make_mdd_for_pred(Vars, VarVarLit, LitList, LitM1, Key, BindingList, RootRef1),
    recorda(LitM1, root(RootRef1), _),
    mdd_reduce(LitM1, LitM),
    eraseall(LitM1).
   


%%%%%%%%%%%%%%%%%%%%%%%% MDD DESTRUCTION %%%%%%%%%%%%%%%%%%%%%%%


mdd_destroy(NodeRef) :-
    recorded(_, node(_, EL), NodeRef),		     
    erase(NodeRef),
    findall(_, (member(edge(_, ChildRef), EL), once(mdd_destroy(ChildRef))), _).    


%%%%%%%%%%%%%%%%%%%%%%%% MDD APPLY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractNewMDDIndex(I) :- 
    recorded(mdd_indexcount, I, RefIndexCount),
    erase(RefIndexCount),
    I1 is I + 1,
    recorda(mdd_indexcount, I1, _).		      



mdd_reduce(M, Res) :-
    recorda(zeromdd, node(0.0,[]), ZeroRef),
    recorda(zeromdd, root(ZeroRef), _),
    mdd_apply(M, zeromdd, Res, +),
    eraseall(zeromdd),
    eraseall(M).




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





		

mdd_apply(Ref1, _, _, Ref2, _, _, _, _, Ref) :- 
    recorded(mdd_apply, arc(pair(Ref1, Ref2), Ref), _),
    !.
mdd_apply(Ref1, node(N1, EL1), M1, Ref2, node(N2, EL2), M2, M, Op, Ref) :- 
    once(number(N1)),
    number(N2),
    !,
    applyOp(N1, N2, Op, N),
    mdd_apply_op_on_edgeLists(N1, EL1, N2, EL2, Op, EL),
    (recorded(M, node(N, EL), Ref) ->
        (true)
	;
	(recorda(M, node(N, EL), Ref))),
    recorda(mdd_apply, arc(pair(Ref1,Ref2), Ref), _),
    !.
mdd_apply(Ref1, node(Var, Edges1), M1, Ref2, node(Var, Edges2), M2, M, Op, Ref) :- 
%    write(Var), write(' is equal to '), write(Var), nl, 
    findall(edge(EdgeValue,TargetIndex),
	   	(member(edge(EdgeValue,ChildIndex1),Edges1),
		 once((recorded(M1, node(ChildVar1,ChildEdges1), ChildIndex1),
		       member(edge(EdgeValue,ChildIndex2),Edges2),
		       recorded(M2, node(ChildVar2,ChildEdges2), ChildIndex2),
		       mdd_apply(ChildIndex1, node(ChildVar1,ChildEdges1),M1,ChildIndex2,node(ChildVar2,ChildEdges2),M2,M,Op,TargetIndex)))),
	    EdgeTargets),

    ((member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2) -> 
      (sort(EdgeTargets, Edges),
       (recorded(M, node(Var,Edges), Ref) ->
	     (true)
	     ;
	     (recorda(M,node(Var,Edges),Ref))))
      ;
      (member(edge(_, Ref), EdgeTargets))),
    recorda(mdd_apply,arc(pair(Ref1,Ref2), Ref),_),
    !.		
mdd_apply(Ref1, node(Var1, Edges1), M1, Ref2, node(Var2, Edges2), M2, M, Op, Ref) :-  
    once(mddVarOrder(Var1, Var2, Ord)),
    Ord = lt,
	!,
%    write(Var1), write(' is less than '), write(Var2), nl, 	
    findall(edge(EdgeValue, TargetIndex), 
    		(member(edge(EdgeValue, ChildIndex), Edges1), 
    		    once((recorded(M1, node(ChildVar, ChildEdgeList), ChildIndex), 
		          mdd_apply(ChildIndex, node(ChildVar, ChildEdgeList), M1, Ref2, node(Var2, Edges2), M2, M, Op, TargetIndex)))), 
		EdgeTargets),

    ((member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2) -> 
      (sort(EdgeTargets, Edges),
       (recorded(M, node(Var1,Edges), Ref) ->
	     (true)
	     ;
	     (recorda(M, node(Var1,Edges), Ref))))
      ;
      (member(edge(_, Ref), EdgeTargets))),
    recorda(mdd_apply,arc(pair(Ref1,Ref2),Ref),_),
    !.          
mdd_apply(Ref1, node(Var1, Edges1), M1, Ref2, node(Var2, Edges2), M2, M, Op, Ref) :- 
%    write(Var1), write(' is greater than '), write(Var2), nl,
    findall(edge(EdgeValue, TargetIndex), 
    		(member(edge(EdgeValue, ChildIndex), Edges2), 
    		    once((recorded(M2, node(ChildVar, ChildEdgeList), ChildIndex), 
		          mdd_apply(Ref1, node(Var1, Edges1), M1, ChildIndex, node(ChildVar, ChildEdgeList), M2, M, Op, TargetIndex)))), 
		EdgeTargets),
    ((member(edge(_,T1), EdgeTargets), member(edge(_, T2), EdgeTargets), T1 \== T2) -> 
      (sort(EdgeTargets, Edges),
       (recorded(M, node(Var2,Edges), Ref) ->
	     (true)
	     ;
	     (recorda(M, node(Var2,Edges), Ref))))
      ;
      (member(edge(_, Ref), EdgeTargets))),
    recorda(mdd_apply,arc(pair(Ref1,Ref2),Ref),_).          
    




    
mdd_apply(M1, M2, M, Op) :- 
    recorded(M1, root(RootRef1), _),
    recorded(M1, node(Var1, EL1), RootRef1),
    recorded(M2, root(RootRef2), _),
    recorded(M2, node(Var2, EL2), RootRef2),
    mdd_apply(RootRef1,node(Var1,EL1),M1,RootRef2,node(Var2,EL2),M2,M,Op,RootRef),
    eraseall(mdd_apply),
    recorda(M, root(RootRef), _).





mdd_chain_apply([FinalRef], _, _, _, FinalRef) :- 
    !.
mdd_chain_apply([NodeRef1, NodeRef2 | Rest], M, NewM, Op, FinalRef) :-
    recorded(M, Node1, NodeRef1),
    recorded(M, Node2, NodeRef2),
    mdd_apply(NodeRef1, Node1, M, NodeRef2, Node2, M, NewM, Op, ResRef),
    eraseall(mdd_apply),
    mdd_chain_apply([ResRef | Rest], M, NewM, Op, FinalRef).





%%%%%%%%%%%% RECURSIVE OPTIMIZATION %%%%%%%%%%%%%%%%%
	     



mdd_recursive_eliminate_variable(NodeRef, _, _, _, NewNodeRef) :- 
    recorded(mdd_recursive_eliminate, arc(NodeRef, NewNodeRef), _),
    !.    
mdd_recursive_eliminate_variable(NodeRef, Var, M, NewM, NewNodeRef) :-
    recorded(M, node(Var, EL), NodeRef),
    findall(ChildRef, member(edge(_, ChildRef), EL), AllChildRefs),
    (maxVar(Var) -> 
    	(mdd_chain_apply(AllChildRefs, M, NewM, max, NewNodeRef))
	;
	(atom_concat([NewM, 't'], NewMtemp),
	 mdd_chain_apply(AllChildRefs, M, NewMtemp, +, NewNodeReftemp),
	 length(AllChildRefs, Len),
	 recorda(lenMDD, node(Len, []), LenRootRef),
	 recorded(NewMtemp, NewMtempRoot, NewNodeReftemp),
	 mdd_apply(NewNodeReftemp, NewMtempRoot, NewMtemp, LenRootRef, node(Len,[]), lenMDD, NewM,div,NewNodeRef),
	 erase(LenRootRef))),
    recorda(mdd_recursive_eliminate, arc(NodeRef, NewNodeRef), _),
    !.
mdd_recursive_eliminate_variable(NodeRef, Var, M, NewM, NewNodeRef) :-
    recorded(M, node(Var1, EL), NodeRef),
    findall(edge(Obj,NewChildRef), (member(edge(Obj,ChildRef), EL), once(mdd_recursive_eliminate_variable(ChildRef, Var, M, NewM, NewChildRef))), NewEL),
    recorda(NewM, node(Var1, NewEL), NewNodeRef),
    recorda(mdd_recursive_eliminate, arc(NodeRef, NewNodeRef), _).






mdd_recursive_eliminate_all_variables([], M, M) :- 
    !.
mdd_recursive_eliminate_all_variables([Var | Rest], M, OptimizedM) :- 
    recorded(M, root(NodeRef), _),
    atom_concat([M, '1'], NewM1),
    mdd_recursive_eliminate_variable(NodeRef, Var, M, NewM1, NewNodeRef1),
    eraseall(mdd_recursive_eliminate),
    recorda(NewM1, root(NewNodeRef1), _),
    eraseall(M),
    atom_concat([M, '1', '0'], NewM),
    mdd_reduce(NewM1, NewM),
    mdd_recursive_eliminate_all_variables(Rest, NewM, OptimizedM).





all_avg_vars_not_eliminated(AllDVars, AvgVarsToElim) :- 
    member(X, AllDVars), 
    once(avgVar(X)), 
    once(not(member(X, AvgVarsToElim))).






mdd_optimize_recursive(Index, M, OptimizedM) :- 
    recorded(varToElim, varToElim(Index, VarsToElim), _),
    write('Vars to elim: '), write(VarsToElim), nl, 
    findall(X, (member(X,VarsToElim), once(avgVar(X))), AvgVarsToElim1),
    sortDVarsRev(AvgVarsToElim1, AvgVarsToElim),
    mdd_recursive_eliminate_all_variables(AvgVarsToElim, M, AvgVarElimM),
    write('Avg Var Elim M: '), printmdd(AvgVarElimM), nl, 
    recorded(allDVars, AllDVars, _),
    (all_avg_vars_not_eliminated(AllDVars, AvgVarsToElim) ->
        (OptimizedM1 = AvgVarElimM)
	;
	(diff(VarsToElim, AvgVarsToElim, MaxVarsToElim1),
    	 sortDVarsRev(MaxVarsToElim1, MaxVarsToElim),
	 mdd_recursive_eliminate_all_variables(MaxVarsToElim, AvgVarElimM, OptimizedM1))),
    mdd_reduce(OptimizedM1, OptimizedM).
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


collectmdd(Ref, I) :-
    recorded(collectmdd, arc(Ref, node(I,V,EL)), _),
    !.
collectmdd(Ref,I) :- 
    recorded(_, node(V,EL), Ref),
    findall(edge(Obj,ChildI), (member(edge(Obj,ChildRef),EL), once(collectmdd(ChildRef, ChildI))), EL1),
    extractNewMDDIndex(I),
    recorda(collectmdd, arc(Ref, node(I,V,EL1)), _).
    
    



printmdd(M) :- 
    recorded(M, root(Ref), _),
    recorda(mdd_indexcount, 1, _),
    collectmdd(Ref, Root),
    eraseall(mdd_indexcount),
    findall(node(I,V,EL), recorded(collectmdd, arc(_, node(I,V,EL)), _), MDD1),
    eraseall(collectmdd),
    insertsort(MDD1,MDD),
    ppmdd(MDD),
    nl.






test_mdd_apply :-
    recorda(mdd_build, mdd_build, _),	       
    mdd_apply([node(0,0.0,[])],[node(3,v1,[edge(a,2)]),node(2,v2,[edge(b,1)]),node(1,1.0,[])],+,M),
    eraseall(mdd_build),	       
    nl,nl,write(M),nl,nl.	

		



test_mdd_make1 :-
    recorda(varTypes, varTypes(v1, [a,b,c,d]), _),
    recorda(varTypes, varTypes(b1, [e,f]), _),	 
    Lit = p(v1,b1,v1),
    LitList =  [p(a,e,a),p(b,e,b),p(c,f,c),p(d,f,d)],
    make_mdd_for_pred(Lit, LitList, m),    
    printmdd(m),nl.




test_mdd_make2 :- 
    recorda(varTypes, varTypes(v2, [a,b,c,d]), _),
    recorda(varTypes, varTypes(b1, [e,f]), _),	 
    Lit = p(v2,b1),
    LitList =  [p(a,e),p(b,f),p(d,e),p(d,f)],
    make_mdd_for_pred(Lit, LitList, n),    
    printmdd(n),nl.




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





hmm(0) :- !.
hmm(X) :- 
%    bb_put(key, X),
%    bb_delete(key,Y),
    recorda(key, X, _),
    eraseall(key), 
    X1 is X - 1,
    hmm(X1).

