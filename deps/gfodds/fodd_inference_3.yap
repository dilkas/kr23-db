
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BRUTE FORCE (Enumeration valuation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_rem_var_type_objs([], VarObjs, D, VarObjs, []) :-
    !.			       
collect_rem_var_type_objs([Var | Rest], VarObjs, D, NewVarObjs, NewVarEqList) :- 
    member(node(_, eq(Var, Var1), _, _), D), 
    member(varobj(Var1, TypeObjs), VarObjs),
    recorda(varTypes, varTypes(Var, TypeObjs), _),
    collect_rem_var_type_objs(Rest, [varobj(Var, TypeObjs) | VarObjs], D, NewVarObjs, NewVarEqList),
    !.
collect_rem_var_type_objs([Var | Rest], VarObjs, D, NewVarObjs, NewVarEqList) :- 
    member(node(_, eq(Var1, Var), _, _), D), 
    member(varobj(Var1, TypeObjs), VarObjs),
    recorda(varTypes, varTypes(Var, TypeObjs), _),
    collect_rem_var_type_objs(Rest, [varobj(Var, TypeObjs) | VarObjs], D, NewVarObjs, NewVarEqList),
    !.
collect_rem_var_type_objs([Var | Rest], VarObjs, D, NewVarObjs, [Var | NewVarEqList]) :-
    collect_rem_var_type_objs(Rest, VarObjs, D, NewVarObjs, NewVarEqList). 
  



get_rem_var_type_objs([], VarObjs, _, _, VarObjs) :- 
    !.    
get_rem_var_type_objs(VarEqList, VarObjs, D, Dom, AllVarObjs) :- 
    collect_rem_var_type_objs(VarEqList, VarObjs, D, NewVarObjs, NewVarEqList),
    (VarEqList = NewVarEqList ->    
    	       member(objects(maxobj,TypeObjs),Dom),
    	       findall(varobj(Var,TypeObjs), member(Var, VarEqList), EqVarObjs),
    	       conc(VarObjs, EqVarObjs, AllVarObjs)
     	       ;
    	       get_rem_var_type_objs(NewVarEqList, NewVarObjs, D, Dom, AllVarObjs)).    



		
get_var_type_objs([], _, _, [], []) :- 
    !.
get_var_type_objs([Var | Rest], D, Dom, [varobj(Var, TypeObjs) | Other], VarEqList) :- 
    member(node(_, eq(a,Var),_,_), D),
    member(objects(maxobj,TypeObjs),Dom),
    recorda(varTypes, varTypes(Var, TypeObjs), _),	
    get_var_type_objs(Rest, D, Dom, Other, VarEqList),
    !.		   
get_var_type_objs([Var | Rest], D, Dom, [varobj(Var, TypeObjs) | Other], VarEqList) :- 
    member(node(_, Lit, _, _), D),		   
    once((Lit =.. [LitPred | LitVars])),
    LitPred \== eq,
    nth(N, LitVars, Var),
    arg_types(ArgTypes),
    member(pred(LitPred, LitArgTypes), ArgTypes),
    nth(N, LitArgTypes, Type),
    member(objects(Type, TypeObjs), Dom),
    recorda(varTypes, varTypes(Var, TypeObjs), _),
    get_var_type_objs(Rest, D, Dom, Other, VarEqList),
    !.
get_var_type_objs([Var | Rest], D, Dom, VarObjs, [Var | VarEqList]) :- 
    get_var_type_objs(Rest, D, Dom, VarObjs, VarEqList).





get_var_domains(Vars, D, Dom, AllVarObjs) :- 
    get_var_type_objs(Vars, D, Dom, VarObjs, VarEqList),
    get_rem_var_type_objs(VarEqList, VarObjs, D, Dom, AllVarObjs).








traverseDD(I, D, _, Edges, Edges, Map) :- 
    member(node(I, Map, -1,-1), D),
    !.	            
traverseDD(I, D, Int, EdgesSoFar, Edges, Map) :- 
    member(node(I,Lit,L,R), D), 
    (subsumption([Lit], Int, noconv, 1),
     traverseDD(L, D, Int, [I-t | EdgesSoFar], Edges, Map)
     ;
     traverseDD(R, D, Int, [I-f | EdgesSoFar], Edges, Map)).

	      
traverseDD(D, Int, Edges, Map) :- 
    D = [node(I,_,_,_) | _],
    traverseDD(I, D, Int, [], Edges, Map).



sum_maps_merge_edges([], Edges, Edges, MapSum, MapSum) :- 
    !.
sum_maps_merge_edges([[Map1, Edges1] | Rest], EdgesSoFar, Edges, MapSumSoFar, MapSum) :- 
    MapSumSoFar1 is MapSumSoFar + Map1,
    batchInsert(Edges1, EdgesSoFar, EdgesSoFar1),
    sum_maps_merge_edges(Rest, EdgesSoFar1, Edges, MapSumSoFar1, MapSum).
    


get_avg_block_maps(D, [], _, I, Map, Edges) :- 
    traverseDD(D,I,Edges,Map),
%    write(Map),nl, 	
    !.
get_avg_block_maps(D, [Var:VarVar | Rest], VarObjs, I, Map, Edges) :- 
    once(member(varobj(Var, TypeObjs), VarObjs)),
    member(VarVar, TypeObjs),
%    once((write(Var), write('='), write(VarVar), write(' '))),
    get_avg_block_maps(D, Rest, VarObjs, I, Map, Edges).




get_max_avg_maps(D, [], AvgKey, VarObjs, I, Map, Edges) :-
    findall([Map, Edges], get_avg_block_maps(D, AvgKey, VarObjs, I, Map, Edges), AllMapsEdges),
    sum_maps_merge_edges(AllMapsEdges, [], Edges, 0.0, MapSum),
    length(AllMapsEdges, Count),
    Map is MapSum/Count,
    !.
get_max_avg_maps(D, [Var:VarVar | Rest], AvgKey, VarObjs, I, Map, Edges) :- 
    once(member(varobj(Var, TypeObjs), VarObjs)),
    member(VarVar, TypeObjs),
%    once((write(Var), write('='), write(VarVar), write(' '))),
    get_max_avg_maps(D, Rest, AvgKey, VarObjs, I, Map, Edges).




fodd_eval(D, I, MaxKey, AvgKey, AllVarObjs, Edges, Map) :- 
    all(map_edges(Map, Edges), get_max_avg_maps(D, MaxKey, AvgKey, AllVarObjs, I, Map, Edges), AllMapsEdges),    
    all(Map, member(map_edges(Map, _), AllMapsEdges), AllMaps),
    max_list(AllMaps, Map),
    all(Edges1, member(map_edges(Map,Edges1), AllMapsEdges), AllInstrEdges),
%    length(AllInstrEdges, Len),
%    random(1,Len,N),
%    nth(N,AllInstrEdges, Edges),
     sort(AllInstrEdges, AllInstrEdges1),
%    write('All Instr Edges: '), write(AllInstrEdges1), nl, 
     (recorded(maxblock_prefer_high, maxblock_prefer_high, _) ->
     	 (last(AllInstrEdges1,Edges))
	 ;	 
         (AllInstrEdges1 = [Edges | _])),
    eraseall(maxavgmap).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% GFODD Inference by Variable Elimination %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



mark_variables_to_eliminate([]) :- 
    !.
mark_variables_to_eliminate([node(_, _, -1,-1) | D]) :- 
    mark_variables_to_eliminate(D),				     
    !.
mark_variables_to_eliminate([node(Index, Lit, L, R) | D]) :- 
    recorded(varToElim, varToElim(Index,IndexVars), _),
    Lit =.. [_ | LitVars],
    diff(IndexVars, LitVars, ChildVars),
    (member(node(L, _, -1,-1), D) ->
        (true)	    
        ;
    	(recorded(varToElim, varToElim(L, LVars), LRef) ->
    		(intersection(ChildVars, LVars, NewLVars),
		 (NewLVars = LVars ->
		 	   (true)
			   ;
			   (erase(LRef), recorda(varToElim, varToElim(L, NewLVars), _))))
		;
		(recorda(varToElim, varToElim(L, ChildVars), _)))),
    (member(node(R, _, -1,-1), D) ->
        (true)	    
        ;
    	(recorded(varToElim, varToElim(R, RVars), RRef) ->
    		(intersection(ChildVars, RVars, NewRVars),
		 (NewRVars = RVars ->
		 	   (true)
			   ;
			   (erase(RRef), recorda(varToElim, varToElim(R, NewRVars), _))))
		;
		(recorda(varToElim, varToElim(R, ChildVars), _)))),
    mark_variables_to_eliminate(D).
     
     





add_edge_to_max_block([], _, []) :- 
    !.
add_edge_to_max_block([MaxBlock | Rest], I-Dir, [[I-Dir | MaxBlock] | NewMaxBlock]) :- 
    add_edge_to_max_block(Rest, I-Dir, NewMaxBlock).






merge_var_lists(VarList1, VarList2, Vars) :- 
    union(VarList1, VarList2, Vars1),
    sortDVarsRev(Vars1, Vars).






multiplyRows(_, _, [], _, _, []) :- 
    !.
multiplyRows(Row1, Row2, [Var | Rest], Vars1, Vars2, [Elem | Other]) :- 
    (nth(N1, Vars1, Var) ->
    	    (nth(N1, Row1, Elem))
	    ;
	    (true)),
    (nth(N2, Vars2, Var) ->
    	    (nth(N2, Row2, Elem))
	    ;
	    (true)),
    multiplyRows(Row1, Row2, Rest, Vars1, Vars2, Other).




multiplyNodeTables([Vars1 | Table1], [[x, y | Vars2] | Table2], I-Dir, ProductVars, [[x, y | ProductVars] | ProductTable]) :- 
    findall([Value, NewMaxBlock | ProductRow], (member(Row1, Table1), member([Value, MaxBlock | Row2], Table2), once(add_edge_to_max_block(MaxBlock, I-Dir, NewMaxBlock)), once(multiplyRows(Row1, Row2, ProductVars, Vars1, Vars2, ProductRow))), ProductTable).




multiplyNodeTables([Vars1 | Table1], [[x, y | Vars2] | Table2], I-Dir, [[x, y | ProductVars] | ProductTable]) :- 
    merge_var_lists(Vars1, Vars2, ProductVars),
    multiplyNodeTables([Vars1 | Table1], [[x, y | Vars2] | Table2], I-Dir, ProductVars, [[x, y | ProductVars] | ProductTable]).




aggregate_avg_variable_over_block(Block, [], Count, MaxBlock, MaxBlock, Sum, ResValMap) :-
    ResValMap is Sum/Count,
%    write(ResValMap), nl, 
    !.
aggregate_avg_variable_over_block(Block, [[ValMap, MaxBlock, BValue | Block] | Rest], Count, MaxBlockSoFar, FinalMaxBlock, Sum, ResValMap) :- 
    Sum1 is Sum + ValMap,
    Count1 is Count + 1,
    (all(MB, (member(M, MaxBlock), member(MSF, MaxBlockSoFar), once(batchInsert(M, MSF, MB))), MaxBlockSoFar1) ; MaxBlockSoFar1 = [[]]),
    aggregate_avg_variable_over_block(Block, Rest, Count1, MaxBlockSoFar1, FinalMaxBlock, Sum1, ResValMap),
    !.
aggregate_avg_variable_over_block(Block, [_ | Rest], Count, MaxBlockSoFar, FinalMaxBlock, Sum, ResValMap) :- 
    aggregate_avg_variable_over_block(Block, Rest, Count, MaxBlockSoFar, FinalMaxBlock, Sum, ResValMap).




aggregate_avg_variable_over_all_blocks([], _, []) :- 
    !.
aggregate_avg_variable_over_all_blocks([Block | Rest], Table, [[Value, MaxBlock | Block] | ResTable]) :- 
%     write('Doing Block '), write(Block), nl, 
    aggregate_avg_variable_over_block(Block, Table, 0, [[]], MaxBlock, 0.0, Value),
    aggregate_avg_variable_over_all_blocks(Rest, Table, ResTable). 






eliminate_avg_variable_from_table(VarToElim, [[x, y | Vars] | Table], [[x, y | Vars1] | ResTable]) :- 
    nth(N, Vars, VarToElim, Vars1),
    shift_var_to_head(Table, N, [], AllBlocks, Table1),
%    write('AllBlocks='), write(AllBlocks), nl,
    aggregate_avg_variable_over_all_blocks(AllBlocks, Table1, ResTable).




eliminate_all_avg_variables_from_table([], Table, Table) :-
    !.
eliminate_all_avg_variables_from_table([Var | Rest], [[x, y | Vars] | Table], ResTable) :- 
    memberchk(Var, Vars),					    
    eliminate_avg_variable_from_table(Var, [[x, y | Vars] | Table], NextTable),	
    eliminate_all_avg_variables_from_table(Rest, NextTable, ResTable),
    !.
eliminate_all_avg_variables_from_table([_ | Rest], [[x, y | Vars] | Table], ResTable) :- 
    eliminate_all_avg_variables_from_table(Rest, [[x, y | Vars] | Table], ResTable).





aggregate_max_variable_over_block(Block, VarToElim, [], VarValList, ResMaxBlockSubs, ResValMap) :-
%    findall(Value, member(maxval(Value, MaxBlock, VarValue), VarValList), Values),		
%    max_list(Values, ResValMap),
%    all(MaxBlockMember, (member(maxval(ResValMap, MaxBlock, MaxVarVal), VarValList), member(MaxBlockMember, MaxBlock)), ResMaxBlockSubs),
    sort(VarValList, SortedVarValList),
    last(SortedVarValList, maxval(ResValMap, HighestResMaxBlock, _)),
    (recorded(maxblock_prefer_high, maxblock_prefer_high,_) ->
        (ResMaxBlockSubs = HighestResMaxBlock)
	;
	once(member(maxval(ResValMap,ResMaxBlockSubs,_), SortedVarValList))),
    !.
aggregate_max_variable_over_block(Block, VarToElim, [[ValMap, MaxBlockSubs, VarValue | Block] | Rest], VarValList, ResMaxBlockSubs, ResValMap) :- 
%    write(Block1), nl,				 
    aggregate_max_variable_over_block(Block, VarToElim, Rest, [maxval(ValMap,MaxBlockSubs,VarValue) | VarValList], ResMaxBlockSubs, ResValMap),
    !.
aggregate_max_variable_over_block(Block, VarToElim, [_ | Rest], VarValList, ResMaxBlockSubs, ResValMap) :- 
    aggregate_max_variable_over_block(Block, VarToElim, Rest, VarValList, ResMaxBlockSubs, ResValMap).






aggregate_max_variable_over_all_blocks([], _, _, []) :- 
    !.
aggregate_max_variable_over_all_blocks([Block | Rest], VarToElim, Table, [[Value, MaxBlockSubs | Block] | ResTable]) :- 
%    write('Aggregating over '), write(Block), nl, 					      
    aggregate_max_variable_over_block(Block, VarToElim, Table, [], MaxBlockSubs, Value),					           
    aggregate_max_variable_over_all_blocks(Rest, VarToElim, Table, ResTable). 







shift_var_to_head([], N, AllBlocks, AllBlocks, []) :- 
    !.
shift_var_to_head([[ValMap, MaxBlockSubs | Row] | Rest], N, AllBlocksSoFar, AllBlocks, [[ValMap, MaxBlockSubs, MaxVarVal | Row1] | Other]) :- 
    nth(N, Row, MaxVarVal, Row1),
    (ground(Row1) ->
        (insert(Row1, AllBlocksSoFar, AllBlocksSoFar1))
	;
	(AllBlocksSoFar1 = AllBlocksSoFar)),
    shift_var_to_head(Rest, N, AllBlocksSoFar1, AllBlocks, Other).




eliminate_max_variable_from_table(VarToElim, [[x, y | Vars] | Table], [[x, y | Vars1] | ResTable]) :-
    nth(N, Vars, VarToElim, Vars1),
    shift_var_to_head(Table, N, [], AllBlocks, Table1),
    aggregate_max_variable_over_all_blocks(AllBlocks, VarToElim, Table1, ResTable).




eliminate_all_max_variables_from_table([], Table, Table) :-
    !.
eliminate_all_max_variables_from_table([Var | Rest], [[x, y | Vars] | Table], ResTable) :- 
    memberchk(Var, Vars),					    
    eliminate_max_variable_from_table(Var, [[x, y | Vars] | Table], NextTable),	
    eliminate_all_max_variables_from_table(Rest, NextTable, ResTable),
    !.
eliminate_all_max_variables_from_table([_ | Rest], [[x, y | Vars] | Table], ResTable) :- 
    eliminate_all_max_variables_from_table(Rest, [[x, y | Vars] | Table], ResTable).





optimizeTable(Index, [[x,y | TableVars] | Table], OptimizedTable) :- 
    recorded(varToElim, varToElim(Index, VarsToElim), _),
%    write('Vars to elim: '), write(VarsToElim), nl,
    findall(X, (member(X,VarsToElim), once(avgVar(X))), AvgVarsToElim1),
    insertsort(AvgVarsToElim1, AvgVarsToElim),
    eliminate_all_avg_variables_from_table(AvgVarsToElim, [[x,y | TableVars] | Table], AvgVarElimTable),
%      	         write('AvgVarElimTable='), write(AvgVarElimTable), nl, 
    recorded(allDVars, AllDVars, _),
    ((member(X, AllDVars), once(avgVar(X)), once(not(member(X, AvgVarsToElim)))) ->
        (OptimizedTable = AvgVarElimTable)
	;
	(diff(VarsToElim, AvgVarsToElim, MaxVarsToElim1),
    	 insertsort(MaxVarsToElim1, MaxVarsToElim),
	 eliminate_all_max_variables_from_table(MaxVarsToElim, AvgVarElimTable, OptimizedTable))).

    		 




projectRowToVarList(Row, TableVars, [], []) :- 
    !.
projectRowToVarList(Row, TableVars, [Var | ProjectedVars], [Elem | ProjRow]) :- 
    nth(N, TableVars, Var),			 
    nth(N, Row, Elem),
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow),
    !.
projectRowToVarList(Row, TableVars, [_ | ProjectedVars], [_ | ProjRow]) :- 
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow).





projectTableToVarList1([], TableVars, ProjectedVars, []) :- 
    !.
projectTableToVarList1([[Value, MaxBlock | Row] | Table], TableVars, ProjectedVars, [[Value, MaxBlock | ProjRow] | ProjTable]) :- 
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow),
    projectTableToVarList1(Table, TableVars, ProjectedVars, ProjTable).
    		



multiplyVarRows(_, _, [], _, _, ProductRow, ProductRow) :- 
    !.
multiplyVarRows(Row1, Row2, [Var | Rest], Vars1, Vars2, ProductRowSoFar, ProductRow) :- 
    (nth(N1, Vars1, Var) ->
    	    (nth(N1, Row1, Elem))
	    ;
	    (true)),
    (nth(N2, Vars2, Var) ->
    	    (nth(N2, Row2, Elem))
	    ;
	    (true)),
    (ground(Elem) ->
            (ProductRowSoFar1 = [Elem | ProductRowSoFar])
	    ;
	    (ProductRowSoFar1 = ProductRowSoFar)),	    
    multiplyVarRows(Row1, Row2, Rest, Vars1, Vars2, ProductRowSoFar1, ProductRow).




multiplyVarTables([Var1 | Table1], [[x, y | Vars2] | Table2], ProductVars, [[x, y | IntermediateVars] | ProductTable]) :- 
    merge_var_lists([Var1],Vars2,IntermediateVars),			
    reverse(ProductVars, ProductVarsRev),			
    findall([Value, MaxBlock | ProductRow], (member(Row1, Table1), member([Value, MaxBlock | Row2], Table2), once(multiplyVarRows(Row1, Row2, ProductVarsRev, [Var1], Vars2, [], ProductRow))), ProductTable).




projectTableToVarList(Table, TableVars, [], _, Table) :- 
    !.
projectTableToVarList(Table, TableVars, [Var | Rest], ProjectedVars, ProjectedTable) :- 
    memberchk(Var, TableVars),
    projectTableToVarList(Table, TableVars, Rest, ProjectedVars, ProjectedTable),
    !.
projectTableToVarList(Table, TableVars, [Var | Rest], ProjectedVars, ProjectedTable) :- 
    recorded(varTypes, varTypes(Var, VarObjs), _),
    findall([X], member(X, VarObjs), VarObjTable),    
    multiplyVarTables([Var | VarObjTable], [[x, y | TableVars] | Table], ProjectedVars, [[x, y | IntermediateVars] | IntermediateTable]),
    projectTableToVarList(IntermediateTable, IntermediateVars, Rest, ProjectedVars, ProjectedTable).




mergeTables([[x,y | _]], Table, Table) :- 
    !.	      
mergeTables(Table, [[x,y | _]], Table) :- 
    !.	      
mergeTables([[x, y | Vars1] | Table1], [[x, y | Vars2] | Table2], [[x, y | MergedVars] | MergedTable]) :- 
    merge_var_lists(Vars1, Vars2, MergedVars),
    projectTableToVarList(Table1, Vars1, MergedVars, MergedVars, ProjTable1),
    projectTableToVarList(Table2, Vars2, MergedVars, MergedVars, ProjTable2),
    conc(ProjTable1, ProjTable2, MergedTable).
        


getRowForPredTable(_, _, [], []) :- 
    !.
getRowForPredTable(LitVars, LitArgs, [X | Vars], [Y | Row]) :- 
    nth(N, LitVars, X),
    nth(N, LitArgs, Y),
    getRowForPredTable(LitVars, LitArgs, Vars, Row).







getPredTablesFromLit(eq(X1,X2), _, [Vars | LitTable], [Vars | NegLitTable]) :- 
    once(domVar(X1)),
    domVar(X2),
%    write(eq(X1,X2)), write(' both vars begin --- '),  nl,			
    sortDVarsRev([X1,X2], Vars2),
    (ve_by_mdd ->
	reverse(Vars2, Vars)
	; 
	(Vars = Vars2)),
    recorded(varTypes, varTypes(X1, TypeObjs), _),
    findall([Y1,Y1], member(Y1,TypeObjs), LitTable),
    findall([Y1,Y2], (member(Y1,TypeObjs), member(Y2, TypeObjs), once((Y1 \= Y2))), NegLitTable),
%    write('eq both vars end --- '), nl,				
    !.
getPredTablesFromLit(eq(X1,X2), _, [[X1], [X2]], [[X1] | NegLitTable]) :- 			
    domVar(X1),	
%    write(eq(X1,X2)), write(' left var begin --- '), nl,				
    recorded(varTypes, varTypes(X1, TypeObjs), _),
    findall([Y1], (member(Y1,TypeObjs), once((Y1 \= X2))), NegLitTable),
%    write('eq left var end --- '), nl,			
    !.
getPredTablesFromLit(eq(X1,X2), _, [[X2], [X1]], [[X2] | NegLitTable]) :- 
    domVar(X2),	
%    write(eq(X1,X2)), write(' right var begin --- '), nl,			
    recorded(varTypes, varTypes(X2, TypeObjs), _),
    findall([Y1], (member(Y1,TypeObjs), once((Y1 \= X1))), NegLitTable),
%    write('eq right var end --- '), nl,				
    !.
getPredTablesFromLit(eq(X1,X1), _, [[],[]], [[]]) :- 
%    write('eq neither vars begin --- '), nl,
%    write('eq neither vars end --- '), nl,						         
    !.
getPredTablesFromLit(eq(X1,X2), _, [[]], [[],[]]) :- 
%    write('eq neither vars begin --- '), nl,
%    write('eq neither vars end --- '), nl,						      
    !.
getPredTablesFromLit(Lit, I, [Vars | LitTable], [Vars | NegLitTable]) :- 
    Lit =.. [Pred | Vars1],
%    write(Pred),write(' begin --- '), nl,				
    getNeg(Pred,NegPred),	
    (all(X, (member(X,Vars1), once(domVar(X))), LitVars) ; LitVars = []),
    sortDVarsRev(LitVars, Vars2),
    (ve_by_mdd ->
	reverse(Vars2, Vars)
	; 
	(Vars = Vars2)),
    getSubsKey(Vars, Key),		    
    variablizeNumList(Vars1, Key, VarVarList),
    VarVarLit =.. [Pred | VarVarList],
    VarVarNegLit =.. [NegPred | VarVarList],
    member([Pred | PredLits], I),
    member([NegPred | NegPredLits], I),
    findall(Row, (member(VarVarLit, PredLits), once((VarVarLit =.. [_ | LitArgs], getRowForPredTable(Vars1, LitArgs, Vars, Row)))), LitTable),
    findall(Row, (member(VarVarNegLit, NegPredLits), once((VarVarNegLit =.. [_ | LitArgs], getRowForPredTable(Vars1, LitArgs, Vars, Row)))), NegLitTable).
%    write(Pred),write(' end --- '), nl.

%    recorda(predTable, predTable(Pred, [Vars | LitTable]), _),
%    recorda(predTable, predTable(NegPred, [Vars | NegLitTable]), _).

    














fodd_inference_by_variable_elimination(_, []) :- 
    !.
fodd_inference_by_variable_elimination(I, [node(Index, V, -1, -1) | D]) :- 
    recorda(varElimTable, varElimTable(Index, [[x,y],[V,[[]]]]), _),
    fodd_inference_by_variable_elimination(I, D),
    !.    
fodd_inference_by_variable_elimination(I, [node(Index, Lit, L, R) | D]) :-      
    statistics(cputime, [T1,_]), 
    getPredTablesFromLit(Lit, I, LitTable, NegLitTable),!,
%    getPredTablesFromLit(Lit, I, K1, K2),!,
%    getPredTablesFromLit(Lit, I, K3, K4),!,
%    getPredTablesFromLit(Lit, I, K5, K6),!,
%    getPredTablesFromLit(Lit, I, K7, K8),!,

    LitTable = [TVars1 | _],
    length(TVars1, TVars1Len),
    length(LitTable, LitTableLen),
    length(NegLitTable, NegLitTableLen),
    U1 is TVars1Len * LitTableLen,
    U2 is TVars1Len * NegLitTableLen,
%    write(U1),write(','),write(U2),write(','), 

%    statistics(cputime, [T2,_]),
%    T3 is T2 - T1,
%    write(T3),write(','), 

    (debug(ve) -> (write('Node Index: '), write(Index), nl) ; true),
    (debug(ve) -> (write('Table for true branch of literal '), write(Lit), write(' (LitTable)'), nl) ; true), 
    (debug(ve) -> (print_lit_table(LitTable)) ; true),
    (debug(ve) -> (write('Table for false branch of literal '), write(Lit), write(' (NegLitTable)'), nl) ; true),
    (debug(ve) -> (print_lit_table(NegLitTable)) ; true), 
    recorded(varElimTable, varElimTable(L, LTable), _),
    recorded(varElimTable, varElimTable(R, RTable), _),
    (debug(ve) -> (write('Table bubbled up from left side of literal (LTable)  '), nl) ; true), 
    (debug(ve) -> (print_table(LTable)) ; true),
    (debug(ve) -> (write('Table bubbled up from right side of literal (RTable)  '), nl) ; true),
    (debug(ve) -> (print_table(RTable)) ; true),

    statistics(cputime, [T4,_]), 
    !, multiplyNodeTables(LitTable, LTable, Index-t, LLitTable),
    !, multiplyNodeTables(NegLitTable, RTable, Index-f, RLitTable),
%    !, multiplyNodeTables(LitTable, LTable, Index-t, J1),
%    !, multiplyNodeTables(NegLitTable, RTable, Index-f, J2),
%    !, multiplyNodeTables(LitTable, LTable, Index-t, J3),
%    !, multiplyNodeTables(NegLitTable, RTable, Index-f, J4),
%    !, multiplyNodeTables(LitTable, LTable, Index-t, J5),
%    !, multiplyNodeTables(NegLitTable, RTable, Index-f, J6),
%    !, multiplyNodeTables(LitTable, LTable, Index-t, J7),
%    !, multiplyNodeTables(NegLitTable, RTable, Index-f, J8),

    LLitTable = [LLitVars1 | _],
    RLitTable = [RLitVars1 | _],
    length(LLitVars1, LLitVars1Len),
    length(LLitTable, LLitTableLen),
    length(RLitVars1, RLitVars1Len),    
    length(RLitTable, RLitTableLen),
    U3 is LLitVars1Len * LLitTableLen,
    U4 is RLitVars1Len * RLitTableLen,
%    write(U3),write(','),write(U4),write(','), 


%    statistics(cputime, [T5,_]),
%    T6 is T5 - T4,
%    write(T6),write(','), 

    (debug(ve) -> (write('Multiplied LitTable and LTable (LLitTable)  '),nl) ; true), 
    (debug(ve) -> (print_table(LLitTable)) ; true),     
    (debug(ve) -> (write('Multiplied NegLitTable and RTable (RLitTable)  '),nl) ; true),
    (debug(ve) -> (print_table(RLitTable)) ; true),     

    statistics(cputime, [T7,_]), 
    !, mergeTables(LLitTable, RLitTable, NodeTable),
%    !, mergeTables(LLitTable, RLitTable, H1),
%    !, mergeTables(LLitTable, RLitTable, H2),
%    !, mergeTables(LLitTable, RLitTable, H3),
%    !, mergeTables(LLitTable, RLitTable, H4),

    NodeTable = [NodeTableVars1 | _],
    length(NodeTableVars1, NodeTableVars1Len),
    length(NodeTable, NodeTableLen),
    U5 is NodeTableVars1Len * NodeTableLen,
%    write(U5),write(','), 


%    statistics(cputime, [T8,_]),
%    T9 is T8 - T7,
%    write(T9),write(','), 

    (debug(ve) -> (write('NodeTable  (merged LLitTable and RLitTable)'), nl) ; true),
    (debug(ve) -> (print_table(NodeTable)) ; true),
%%	length(NodeTable, Rows), NodeTable = [Top | _], length(Top, Cols), 
%%    write('Node Table with '), write(Rows), write(' rows and '), write(Cols), write(' columns'), nl,

    statistics(cputime, [T10,_]), 
    !, optimizeTable(Index, NodeTable, OptimizedTable), 
%    !, optimizeTable(Index, NodeTable, OptimizedTable1), 
%    !, optimizeTable(Index, NodeTable, OptimizedTable2), 
%    !, optimizeTable(Index, NodeTable, OptimizedTable3), 
%    !, optimizeTable(Index, NodeTable, OptimizedTable4), 

    OptimizedTable = [OptimizedTableVars1 | _],
    length(OptimizedTableVars1, OptimizedTableVars1Len),
    length(OptimizedTable, OptimizedTableLen),
    U6 is OptimizedTableVars1Len * OptimizedTableLen,
%    write(U6),write(','), 


%    statistics(cputime, [T11,_]),
%    T12 is T11 - T10,
%    write(T12),write(','), 

    (debug(ve) -> (write('Optimized Node Table  '), nl) ; true),
    (debug(ve) -> (print_table(OptimizedTable)) ; true),
    (debug(ve) -> (write(' ------------ '), nl) ; true),
    recorda(varElimTable, varElimTable(Index, OptimizedTable), _),
    !,
    fodd_inference_by_variable_elimination(I, D).
 


   


fodd_inference_by_variable_elimination_mdd(I, []) :- 
    !.
fodd_inference_by_variable_elimination_mdd(I, [node(Index, V, -1, -1) | D]) :- 
    make_mdd_from_leaf(V, M),
    recorda(varElimTable, varElimTable(Index, M), _),
    fodd_inference_by_variable_elimination_mdd(I, D),
    !.    
fodd_inference_by_variable_elimination_mdd(I, [node(Index, Lit, L, R) | D]) :- 
    statistics(cputime, [T1,_]), 
    getPredTablesFromLit(Lit, I, LitTable, NegLitTable),!,
%    getPredTablesFromLit(Lit, I, LitTable1, NegLitTable1),!,
%    getPredTablesFromLit(Lit, I, LitTable2, NegLitTable2),!,
%    getPredTablesFromLit(Lit, I, LitTable3, NegLitTable3),!,
%    getPredTablesFromLit(Lit, I, LitTable4, NegLitTable4),!,
    statistics(cputime, [T2,_]),
    T3 is T2 - T1,
%    write(T3),write(','), 

    statistics(cputime, [T4,_]), 
    make_mdd_from_pred_table(LitTable, NegLitTable, LitM),
    mdd_apply([node(1,1.0,[])],LitM, -, NegLitM), 	
%    make_mdd_from_pred_table(LitTable, NegLitTable, LitM1),
%    mdd_apply([node(1,1.0,[])],LitM, -, NegLitM1), 	
%    make_mdd_from_pred_table(LitTable, NegLitTable, LitM2),
%    mdd_apply([node(1,1.0,[])],LitM, -, NegLitM2), 	
%    make_mdd_from_pred_table(LitTable, NegLitTable, LitM3),
%    mdd_apply([node(1,1.0,[])],LitM, -, NegLitM3), 	
%    make_mdd_from_pred_table(LitTable, NegLitTable, LitM4),
%    mdd_apply([node(1,1.0,[])],LitM, -, NegLitM4), 	

    length(LitM, U1),
    length(NegLitM, U2),
%    write(U1),write(','),write(U2),write(','), 


    statistics(cputime, [T5,_]),
    T6 is T5 - T4,
%    write(T6),write(','), 
 
    (debug(ve) -> (write('Hmm Node Index: '), write(Index), nl) ; true),
    (debug(ve) -> (write('Table for true branch of literal '), write(Lit), write(' (LitTable)'), nl) ; true), 
    (debug(ve) -> (ppmdd(LitM)) ; true),
    (debug(ve) -> (write('Table for false branch of literal '), write(Lit), write(' (NegLitTable)'), nl) ; true), 
    (debug(ve) -> (ppmdd(NegLitM)) ; true),

    recorded(varElimTable, varElimTable(L, LM1), _),
    recorded(varElimTable, varElimTable(R, RM1), _),

    statistics(cputime, [T7,_]), 
    mdd_apply_mult_add_edge(LM1,Index-t,LM),
    mdd_apply_mult_add_edge(RM1,Index-f,RM),
%    mdd_apply_mult_add_edge(LM1,Index-t,LM1),
%    mdd_apply_mult_add_edge(RM1,Index-f,RM1),
%    mdd_apply_mult_add_edge(LM1,Index-t,LM2),
%    mdd_apply_mult_add_edge(RM1,Index-f,RM2),
%    mdd_apply_mult_add_edge(LM1,Index-t,LM3),
%    mdd_apply_mult_add_edge(RM1,Index-f,RM3),
%    mdd_apply_mult_add_edge(LM1,Index-t,LM4),
%    mdd_apply_mult_add_edge(RM1,Index-f,RM4),


    statistics(cputime, [T8,_]),
    T9 is T8 - T7 + T6 + T3,
%    write(T9),write(','), 

    (debug(ve) -> (write('Table bubbled up from left side of literal (LTable)  '), nl) ; true), 
    (debug(ve) -> (ppmdd(LM)) ; true),
    (debug(ve) -> (write('Table bubbled up from right side of literal (RTable)  '), nl) ; true),
    (debug(ve) -> (ppmdd(RM)) ; true),

    statistics(cputime, [T10,_]), 
    !, mdd_apply(LitM, LM, *, LLitM),
    !, mdd_apply(NegLitM, RM, *, RLitM),
%    !, mdd_apply(LitM, LM, *, LLitM1),
%    !, mdd_apply(NegLitM, RM, *, RLitM1),
%    !, mdd_apply(LitM, LM, *, LLitM2),
%    !, mdd_apply(NegLitM, RM, *, RLitM2),
%    !, mdd_apply(LitM, LM, *, LLitM3),
%    !, mdd_apply(NegLitM, RM, *, RLitM3),
%    !, mdd_apply(LitM, LM, *, LLitM4),
%    !, mdd_apply(NegLitM, RM, *, RLitM4),

    length(LLitM, U3),
    length(RLitM, U4),
%    write(U3),write(','),write(U4),write(','), 
     

    statistics(cputime, [T11,_]),
    T12 is T11 - T10,
%    write(T12),write(','), 

    (debug(ve) -> (write('Multiplied LitTable and LTable (LLitTable)  '),nl) ; true), 
    (debug(ve) -> (ppmdd(LLitM)) ; true),     
    (debug(ve) -> (write('Multiplied NegLitTable and RTable (RLitTable)  '),nl) ; true),
    (debug(ve) -> (ppmdd(RLitM)) ; true),    

    statistics(cputime, [T13,_]), 
    !, mdd_apply(LLitM, RLitM, +, NodeM),
%    !, mdd_apply(LLitM, RLitM, +, NodeM1),
%    !, mdd_apply(LLitM, RLitM, +, NodeM2),
%    !, mdd_apply(LLitM, RLitM, +, NodeM3),
%    !, mdd_apply(LLitM, RLitM, +, NodeM4),


    length(NodeM, U5),
%    write(U5),write(','),

    statistics(cputime, [T14,_]),
    T15 is T14 - T13,
%    write(T15),write(','), 

    (debug(ve) -> (write('NodeTable  (merged LLitTable and RLitTable)'), nl) ; true),
    (debug(ve) -> (ppmdd(NodeM)) ; true),

    statistics(cputime, [T16,_]),
%    !, mdd_optimize(Index, NodeM, OptimizedM), 
     !, mdd_optimize_recursive(Index, NodeM, OptimizedM),
%    !, mdd_optimize(Index, NodeM, OptimizedM1), 
%    !, mdd_optimize(Index, NodeM, OptimizedM2), 
%    !, mdd_optimize(Index, NodeM, OptimizedM3), 
%    !, mdd_optimize(Index, NodeM, OptimizedM4), 

    length(OptimizedM, U6),
%    write(U6),write(','),

    statistics(cputime, [T17,_]),
    T18 is T17 - T16,
%    write(T18),write(','), 

    (debug(ve) -> (write('Optimized Node Table  '), nl) ; true),
    (debug(ve) -> (ppmdd(OptimizedM)) ; true),
    (debug(ve) -> (write(' ------------ '), nl) ; true),
    recorda(varElimTable, varElimTable(Index, OptimizedM), _),
    !,
    fodd_inference_by_variable_elimination_mdd(I, D).








fodd_infer(I, D, Edges, Res) :- 
    D = [node(RootIndex, _, _, _) | _],
    getDVars(D,Vars),
    recorda(varToElim, varToElim(RootIndex,Vars), _),
    mark_variables_to_eliminate(D),
    reverse(D,Drev),
    recorded(rewdPred, RewdPred, _),  
    interpretationObjs3579(RewdPred, Dom),
    get_var_type_objs(Vars, D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    fodd_inference_by_variable_elimination(I, AllVarObjs, Drev),
    eraseall(varToElim),
    recorded(varElimTable, varElimTable(RootIndex, [[x,y],[Res,MaxBlock]]), _),
    eraseall(varElimTable),
    eraseall(predTable),
    findall(X:Y, (member(X, Vars), once((member([X:Y], MaxBlock) ; copy_term(Z,Y)))), MaxBlockKey),
    get_edges_from_maxBlock(RootIndex, D, MaxBlockKey, I, Edges).
        	      
    



get_edges_from_maxBlock(Index, D, Key, I, Edges) :-
    D = [node(RootIndex, _, _, _) | _],			       
    variablizeDD(D,Key,VarD),
    get_all_paths(RootIndex,VarD,[],[],Paths),
    (all(Edge, (member(path(_,_,PathEdges,PF), Paths),once(subsumption(PF,I,noconv,1)),member(Edge,PathEdges)), Edges1) ; Edges1 = []),
    insertsort(Edges1, Edges).


get_maxBlock_Key(MaxBlock, [], []) :- 
    !.
get_maxBlock_Key(MaxBlock, [Var | DVars], [Var:KeyVar | Key]) :- 
    member(Var:KeyVar, MaxBlock),
    get_maxBlock_Key(MaxBlock, DVars, Key),
    !.
get_maxBlock_Key(MaxBlock, [Var | DVars], [Var:_ | Key]) :- 
    get_maxBlock_Key(MaxBlock, DVars, Key).




get_edges_from_all_maxBlocks(D, DVars, MaxBlocks, I, EdgesI) :- 
    all(Edges, (member(MaxBlock, MaxBlocks), once((get_maxBlock_Key(MaxBlock, DVars, Key),  get_edges_from_maxBlock(1, D, Key, I, Edges)))), AllEdges),
    sort(AllEdges, AllEdges1),
%    write('AllEdges: '), write(AllEdges1), nl, 
    AllEdges1 = [EdgesI | _].






perform_gfodd_reduction_by_ve([], _, _, _, Edges, Edges, []) :- 
    !.
perform_gfodd_reduction_by_ve([I | Ints], D, Drev, DVars, EdgesSoFar, Edges, [Map | AllMaps]) :- 
    (ve_by_mdd ->
	(fodd_inference_by_variable_elimination_mdd(I, Drev),
         recorded(varElimTable, varElimTable(RootIndex, [node(_,Map,MaxBlocks1)]), _),
	 MaxBlocks = [MaxBlocks1])
	;
        (fodd_inference_by_variable_elimination(I, Drev),
         recorded(varElimTable, varElimTable(RootIndex, [[x,y],[Map,MaxBlocks]]), _))),
    eraseall(varElimTable),
    eraseall(predTable),    
%    write('Map='), write(Map), 
     sort(MaxBlocks, [EdgesI | _]), 
%    write(' EdgesI: '), write(EdgesI), nl, 
    batchInsert(EdgesI, EdgesSoFar, EdgesSoFar1),
    perform_gfodd_reduction_by_ve(Ints, D, Drev, DVars, EdgesSoFar1, Edges, AllMaps).
 




gfodd_reduction_by_ve(D, R) :-
    D = [node(RootIndex, _, _, _) | _],
    getDVars(D,Vars),
    recorda(allDVars, Vars, RefAllDVars),
    recorda(varToElim, varToElim(RootIndex,Vars), _),
    mark_variables_to_eliminate(D),
    reverse(D,Drev),
    recorded(rewdPred, RewdPred, _),  
    interpretations3579(RewdPred, Ints),
    interpretationObjs3579(RewdPred, Dom),
    !,
    get_var_domains(Vars, D, Dom, _),	 
    !,	
    perform_gfodd_reduction_by_ve(Ints, D, Drev, Vars, [], Edges, AllMaps),
    eraseall(varToElim),
    erase(RefAllDVars),
    (recorded(max_over_actions_reduction, Action, Ref) -> 
    	(erase(Ref),
%	 write('Action Recorded: '), write(Action), nl,
	 recorda(max_over_actions_values, action_values(Action,AllMaps), _))
	; 
	(true)),
    get_all_edges(D, AllEdges),
    diff(AllEdges, Edges, EdgesToRemove),
%    write('Edges To Remove: '), write(EdgesToRemove), nl, 
    (EdgesToRemove = [],
     Res1 = D
     ;	       	       
     insert(node(0,0.0,-1,-1),D, D0),
     replace_edge_target(D0, EdgesToRemove, D1),
     D1 = [D1Root | _],
     recorda(indexCount,2,_),    
     apply1(D1Root, D1, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
     eraseall(indexCount),
     all(X, recorded(apply, arc(_,X), _), Res),
     eraseall(apply),
     insertsort(Res, Res1)),
    (modelcheckingnode ->
     (recorda(node_reduction, node_reduction(AllMaps), NodeRedRef),
      gfodd_node_reduction_by_ve(Res1, R),
      erase(NodeRedRef))
     ;
     (R = Res1)),
    eraseall(varTypes).



 
gfodd_evaluation_by_ve(D, I, Dom, Map) :- 
    D = [node(RootIndex, _, _, _) | _],
    getDVars(D,Vars),
    recorda(varToElim, varToElim(RootIndex,Vars), _),
    recorda(allDVars, Vars, RefAllDVars),
    mark_variables_to_eliminate(D),
    reverse(D,Drev),
    get_var_domains(Vars, D, Dom, _),
    (ve_by_mdd ->
	(fodd_inference_by_variable_elimination_mdd(I, Drev),
	 (recorded(varElimTable, varElimTable(RootIndex, [node(_,Map,_)]), _) ->
	  	(true)
		;
		(member(objects(_,[]),Dom) -> 
			(Map = 1.0)
			;
			(!,fail))))
	;
	(fodd_inference_by_variable_elimination(I, Drev),
    	 (recorded(varElimTable, varElimTable(RootIndex, [[x,y],[Map,_]]), _) ->
	  	(true)
		;
		(member(objects(_,[]),Dom) -> 
			(Map = 1.0)
			;
			(!,fail))))),
    eraseall(varElimTable),    
    erase(RefAllDVars),
    eraseall(predTable),
    eraseall(varTypes),	
    eraseall(varToElim).




%%%%%%%%%%%%%%%%%%%%%%%%%%%% VE NODE REDUCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%


test_node_candidate_by_ve(_, [], _, []) :-
    !.
test_node_candidate_by_ve(D, [OldMap | AllMaps], Dom, [I | Ints]) :-
    gfodd_evaluation_by_ve(D, I, Dom, Map),
    !,
    ((Map = OldMap) ; (write(I), nl, !, fail)),
    !,
    test_node_candidate_by_ve(D, AllMaps, Dom, Ints).




perform_gfodd_node_reduction(D, [], _, _, _, D) :-
    !.
perform_gfodd_node_reduction(D, [CandNode | Candidates], AllMaps, Dom, Ints, Res) :-
    write('Testing '), write(CandNode), nl, 				
    once((del(node(CandNode, _, L, R), D, D1),
          parentUpdate(D1, L, CandNode, DL, _))),
    test_node_candidate_by_ve(DL, AllMaps, Dom, Ints),
    !,	
    DL = [DLRoot | _],	       
    recorda(indexCount,2,_),    
    apply1(DLRoot, DL, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
    eraseall(indexCount),
    all(X, recorded(apply, arc(_,X), _), DL1),
    eraseall(apply),
    insertsort(DL1, DL2),
    !,
    perform_gfodd_node_reduction(DL2, Candidates, AllMaps, Dom, Ints, Res),
    !.
perform_gfodd_node_reduction(D, [CandNode | Candidates], AllMaps, Dom, Ints, Res) :-
    once((del(node(CandNode, _, L, R), D, D1),
          parentUpdate(D1, R, CandNode, DR, _))),
    test_node_candidate_by_ve(DR, AllMaps, Dom, Ints),
    !,	
    DR = [DRRoot | _],	       
    recorda(indexCount,2,_),    
    apply1(DRRoot, DR, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
    eraseall(indexCount),
    all(X, recorded(apply, arc(_,X), _), DR1),
    eraseall(apply),
    insertsort(DR1, DR2),
    !,
    perform_gfodd_node_reduction(DR2, Candidates, AllMaps, Dom, Ints, Res),
    !.
perform_gfodd_node_reduction(D, [_ | Candidates], AllMaps, Dom, Ints, Res) :-
    perform_gfodd_node_reduction(D, Candidates, AllMaps, Dom, Ints, Res).
      







gfodd_node_reduction_by_ve(D, Res) :-    
    recorded(rewdPred, RewdPred, _),
    interpretationObjs3579(RewdPred, Dom),
    interpretations3579(RewdPred, Ints),
    recorded(node_reduction, node_reduction(AllMaps), _),
    findall(Index, (member(node(Index,eq(X1,X2),NL,NR), D), once((memberchk(b1, [X1,X2])))), Candidates),
%    write('Candidates: '), write(Candidates), nl, 
    perform_gfodd_node_reduction(D, Candidates, AllMaps, Dom, Ints, Res).








profiling_info1([], Pr, Mult, Merge, Opt) :- 
    write('Time to create Pred Tables: '), write(Pr), nl,
    write('Time to Multiply Tables: '), write(Mult), nl,
    write('Time to Merge Tables: '), write(Merge), nl,
    write('Time to Optimize Tables: '), write(Opt), nl,
    !.
profiling_info1([A,B,C,D | Rest], Pr, Mult, Merge, Opt) :-
    Pr1 is Pr + A,
    Mult1 is Mult + B,
    Merge1 is Merge + C,
    Opt1 is Opt + D,
    profiling_info1(Rest, Pr1, Mult1, Merge1, Opt1).





profiling_info2([], Pr, Make, Add, Mult, Merge, Opt) :- 
    write('Time to create Pred Tables: '), write(Pr), nl,
    write('Time to make MDDs: '), write(Make), nl,
    write('Time to add edges to MDDs: '), write(Add), nl,
    write('Time to Multiply Tables: '), write(Mult), nl,
    write('Time to Merge Tables: '), write(Merge), nl,
    write('Time to Optimize Tables: '), write(Opt), nl,
    !.
profiling_info2([A,B,C,D,E,F | Rest], Pr, Make, Add, Mult, Merge, Opt) :-
    Pr1 is Pr + A,
    Make1 is Make + B,
    Add1 is Add + C,
    Mult1 is Mult + D,
    Merge1 is Merge + E,
    Opt1 is Opt + F,
    profiling_info2(Rest, Pr1, Make1, Add1, Mult1, Merge1, Opt1).







ugich(Total, Count, [], [], [], []) :- 
    Avg is Count/Total,
    write(Avg), nl,
    !.
ugich(Total, Count, [TT1, TT2, TT3, TT4 | Rest1], [MT1, MT2, MT3, MT4 | Rest2], [TS1, TS2, TS3, TS4, TS5, TS6 | Rest3], [MS1, MS2, MS3, MS4, MS5, MS6 | Rest4]) :- 
    (((TT3 < MT3, TS2 > MS2), (TT4 < MT4, TS2 > MS2)) ->
    	 (Count1 is Count + 1)
	 ;
	 (Count1 = Count)),
    Total1 is Total + 1,
    ugich(Total1, Count1, Rest1, Rest2, Rest3, Rest4).




ugich :-
    ['baghuya1'], ['baghuya2'], ['baghuya3'], ['baghuya4'],
    aaa1(TT),
    aaa2(MT),
    bbb1(TS),
    bbb2(MS),
    write(hmm), nl, 
    ugich(0,0,TT,MT,TS,MS).