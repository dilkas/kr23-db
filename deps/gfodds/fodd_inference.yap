
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BRUTE FORCE (Enumeration valuation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_rem_var_type_objs([], VarObjs, D, VarObjs, []) :-
    !.			       
collect_rem_var_type_objs([Var | Rest], VarObjs, D, NewVarObjs, NewVarEqList) :- 
    member(node(_, eq(Var, Var1), _, _), D), 
    member(varobj(Var1, TypeObjs), VarObjs),
    collect_rem_var_type_objs(Rest, [varobj(Var, TypeObjs) | VarObjs], D, NewVarObjs, NewVarEqList),
    !.
collect_rem_var_type_objs([Var | Rest], VarObjs, D, NewVarObjs, NewVarEqList) :- 
    member(node(_, eq(Var1, Var), _, _), D), 
    member(varobj(Var1, TypeObjs), VarObjs),
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
    get_var_type_objs(Rest, D, Dom, Other, VarEqList),
    !.
get_var_type_objs([Var | Rest], D, Dom, VarObjs, [Var | VarEqList]) :- 
    get_var_type_objs(Rest, D, Dom, VarObjs, VarEqList).







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
    

store_max_avg_maps(D, [], _, AvgVarVar, varobj(_,AvgVarObjs), I) :-
    once((findall([Map, Edges], (member(AvgVarVar, AvgVarObjs), once((traverseDD(D,I,Edges,Map), write('b='), write(AvgVarVar), write(' '), write(Map), write(' --- '), write(Edges), nl))), MapEdges),
%    once((findall([Map, Edges], (member(AvgVarVar, AvgVarObjs), once((traverseDD(D,I,Edges,Map)))), MapEdges),
    sum_maps_merge_edges(MapEdges, [], Edges, 0.0, MapSum),
    length(MapEdges, Len),
    Map is MapSum/Len,
    once((write('avg='), write(Map), nl)),
    insertsort(Edges, Edges1),
    recorda(maxavgmap,map_edges(Map,Edges1),_))).  
store_max_avg_maps(D, [Var:VarVar | Rest], VarObjs, AvgVarVar, AvgVarObjs, I) :- 
    once(member(varobj(Var, TypeObjs), VarObjs)),
    member(VarVar, TypeObjs),
    once((write(Var), write('='), write(VarVar), write(' '))),
    store_max_avg_maps(D, Rest, VarObjs, AvgVarVar, AvgVarObjs, I).



fodd_eval(D, I, Key, AllVarObjs, AvgVarVar, AvgVarObjs, Edges, Map) :- 
    all(_, store_max_avg_maps(D, Key, AllVarObjs, AvgVarVar, AvgVarObjs, I), _),    
    all(Map, recorded(maxavgmap, map_edges(Map, _), _), AllMaps),
    max_list(AllMaps, Map),
    all(Edges1, recorded(maxavgmap, map_edges(Map,Edges1), _), AllInstrEdges),
%    length(AllInstrEdges, Len),
%    random(1,Len,N),
%    nth(N,AllInstrEdges, Edges),
     sort(AllInstrEdges, AllInstrEdges1),
%    write('All Instr Edges: '), write(AllInstrEdges1), nl, 
     AllInstrEdges1 = [Edges | _],
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
     
     







merge_var_lists([b | Vars1], [b | Vars2], [b | Vars]) :- 
    union(Vars1, Vars2, Vars),
    !. 		   
merge_var_lists([b | Vars1], Vars2, [b | Vars]) :- 
    union(Vars1, Vars2, Vars),
    !. 		   
merge_var_lists(Vars1, [b | Vars2], [b | Vars]) :- 
    union(Vars1, Vars2, Vars),
    !. 		   
merge_var_lists(Vars1, Vars2, Vars) :- 
    union(Vars1, Vars2, Vars).





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




multiplyTables([Vars1 | Table1], [[x, y | Vars2] | Table2], [[x, y | ProductVars] | ProductTable]) :- 
    merge_var_lists(Vars1, Vars2, ProductVars),
    findall([Value, MaxBlock | ProductRow], (member(Row1, Table1), member([Value, MaxBlock | Row2], Table2), once(multiplyRows(Row1, Row2, ProductVars, Vars1, Vars2, ProductRow))), ProductTable).
    



aggregate_avg_variable_over_block1(Block, [], Count, Sum, [], ResValMap) :-
%    (Block == [t2,s1], write('ONE  --  '), write('Sum = '), write(Sum), write(' Count = '), write(Count), nl ; true), 				 					 
    ResValMap is Sum/Count,
    !.
aggregate_avg_variable_over_block1(Block, [[ValMap, _, BValue | Block1] | Rest], Count, Sum, RemBlocks, ResValMap) :- 
%    once((Block == [t2,s1], write('BValue='),write(BValue),write(' Map='),write(ValMap),write(' count='),write(Count),write(' sum='),write(Sum),nl ; true)),  
    ground(Block1),
    Block=Block1,
%    once((Block == [t2,s1], write('TWO  --  '), nl ; true)),
    Sum1 is Sum + ValMap,
    Count1 is Count + 1,					 
    aggregate_avg_variable_over_block1(Block, Rest, Count1, Sum1, RemBlocks, ResValMap),
    !.
aggregate_avg_variable_over_block1(Block, [[ValMap, Valuation, BValue | Block1] | Rest], Count, Sum, [[ValMap, Valuation, BValue | Block1] | RemBlocks], ResValMap) :- 
%    once((Block == [t2,s1], write('BValue='),write(BValue),write(' Map='),write(ValMap),write(' Block1='),write(Block1),write(' count='),write(Count),write(' sum='),write(Sum),nl ; true)),  
    not(not((Block1 = Block))),
%    once((Block == [t2,s1], write('THREE  --  '), nl ; true)),
    Sum1 is Sum + ValMap,
    Count1 is Count + 1,					 
    aggregate_avg_variable_over_block1(Block, Rest, Count1, Sum1, RemBlocks, ResValMap),
    !.
aggregate_avg_variable_over_block1(Block, [Val | Rest], Count, Sum, [Val | RemBlocks], ResValMap) :- 
%    (Block == [t2,s1], write('FOUR  --  '), write('different'), nl ; true),
    aggregate_avg_variable_over_block1(Block, Rest, Count, Sum, RemBlocks, ResValMap).





aggregate_avg_variable_over_block(Block, [], Count, Count, Sum, ResValMap) :-
%    (Block == [t1,s1], write('ONE  --  '), write('Sum = '), write(Sum), write(' Count = '), write(Count), nl ; true), 				 					 
    ResValMap is Sum/Count,
    write(ResValMap), nl, 
    !.
aggregate_avg_variable_over_block(Block, [[ValMap, _, BValue | Block1] | Rest], Count, FinalCount, Sum, ResValMap) :- 
%    once((Block == [t1,s1], write('BValue='),write(BValue),write(' Map='),write(ValMap),write(' Block1='),write(Block1),write(' count='),write(Count),write(' sum='),write(Sum),nl ; true)),  
    not(not((Block1 = Block))),
    once((write(Block1), write(' '), write(BValue), write(' '), write(ValMap), nl)),
    Sum1 is Sum + ValMap,
    Count1 is Count + 1,					 
    aggregate_avg_variable_over_block(Block, Rest, Count1, FinalCount, Sum1, ResValMap),
    !.
aggregate_avg_variable_over_block(Block, [Val | Rest], Count, FinalCount, Sum, ResValMap) :- 
%    (Block == [t1,s1], write('FOUR  --  '), write('different'), nl ; true),
    aggregate_avg_variable_over_block(Block, Rest, Count, FinalCount, Sum, ResValMap).




aggregate_avg_variable_over_all_blocks([], _, []) :- 
    !.
aggregate_avg_variable_over_all_blocks([Block | Rest], Table, [[Value, MaxBlock | Block] | ResTable]) :- 
%    aggregate_avg_variable_over_block1(Block, Table, 0, 0.0, RemTable, Value),
     write('Doing Block '), write(Block), nl, 
%    findall(X, (member(X, Block), once((X = dummyvar3579 ; true))), ModifiedBlock),  
    ModifiedBlock = Block,
    aggregate_avg_variable_over_block(ModifiedBlock, Table, 0, Count, 0.0, Value),
    (Count == 1 ->
    	   (member([_, MaxBlock, _ | Block], Table))
	   ;
	   (MaxBlock = [[]])),
    aggregate_avg_variable_over_all_blocks(Rest, Table, ResTable). 






eliminate_avg_variable_from_table([[x, y, b | Vars] | Table], [[x, y | Vars] | ResTable]) :- 
    (all(Row, (member([_, _, _ | Row], Table), once(ground(Row))), AllBlocks1) ; Allblocks1 = []),
    (all(Row, (member([_, _, _ | Row], Table), once(not(member(Row, AllBlocks1)))), AllBlocks2) ; AllBlocks2 = []),
%     all(Row, (member([_, _, _ | Row], Table)), AllBlocks),

%     all(Row, (member([_, _, _ | Row], Table), once(ground(Row))), AllBlocks),
    conc(AllBlocks1, AllBlocks2, AllBlocks),
    write('AllBlocks='), write(AllBlocks), nl,
    aggregate_avg_variable_over_all_blocks(AllBlocks, Table, ResTable),
    !.
eliminate_avg_variable_from_table(Table, Table).






aggregate_max_variable_over_block(Block, VarToElim, [], VarValList, ResMaxBlockSubs, ResValMap) :-
    findall(Value, member(maxval(Value, MaxBlock, VarValue), VarValList), Values),					 
    max_list(Values, ResValMap),
    all([VarToElim:MaxVarVal | MaxBlockMember], (member(maxval(ResValMap, MaxBlock, MaxVarVal), VarValList), member(MaxBlockMember, MaxBlock)), ResMaxBlockSubs),
%    sort(VarValList, SortedVarValList),
%    last(SortedVarValList, maxval(ResValMap, ResMaxBlockSubs, _)), 
    !.
aggregate_max_variable_over_block(Block, VarToElim, [[ValMap, MaxBlockSubs, VarValue | Block1] | Rest], VarValList, ResMaxBlockSubs, ResValMap) :- 
    not(not(Block = Block1)),	
    write(Block1), nl,				 
    aggregate_max_variable_over_block(Block, VarToElim, Rest, [maxval(ValMap,MaxBlockSubs,VarValue) | VarValList], ResMaxBlockSubs, ResValMap),
    !.
aggregate_max_variable_over_block(Block, VarToElim, [_ | Rest], VarValList, ResMaxBlockSubs, ResValMap) :- 
    aggregate_max_variable_over_block(Block, VarToElim, Rest, VarValList, ResMaxBlockSubs, ResValMap).






aggregate_max_variable_over_all_blocks([], _, _, []) :- 
    !.
aggregate_max_variable_over_all_blocks([Block | Rest], VarToElim, Table, [[Value, MaxBlockSubs | Block] | ResTable]) :- 
    write('Aggregating over '), write(Block), nl, 					      
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
    (nth(N, Vars, VarToElim, Vars1) ->
        (shift_var_to_head(Table, N, [], AllBlocks, Table1),    
    	 aggregate_max_variable_over_all_blocks(AllBlocks, VarToElim, Table1, ResTable))
	;
	(Vars1 = Vars,
	 ResTable = Table)).




eliminate_all_max_variables_from_table([], Table, Table) :-
    !.
eliminate_all_max_variables_from_table([Var | Rest], Table, ResTable) :- 
    eliminate_max_variable_from_table(Var, Table, NextTable),			    
    eliminate_all_max_variables_from_table(Rest, NextTable, ResTable).





optimizeTable(Index, [[x,y | TableVars] | Table], OptimizedTable) :- 
    recorded(varToElim, varToElim(Index, VarsToElim), _),
%    write('Vars to elim: '), write(VarsToElim), nl,
    (member(b, VarsToElim) ->
    	        (del(b, VarsToElim, VarsToElim1), 
    	         (member(b, TableVars) -> 
     			   (eliminate_avg_variable_from_table([[x,y | TableVars] | Table], AvgVarElimTable))
			    ;
			    (AvgVarElimTable = [[x,y | TableVars] | Table])),
      	         write('AvgVarElimTable='), write(AvgVarElimTable), nl, 
       	         eliminate_all_max_variables_from_table(VarsToElim1, AvgVarElimTable, OptimizedTable))
     		 ;
     		 (OptimizedTable = [[x,y | TableVars] | Table])).




projectRowToVarList(Row, TableVars, [], []) :- 
    !.
projectRowToVarList(Row, TableVars, [Var | ProjectedVars], [Elem | ProjRow]) :- 
    nth(N, TableVars, Var),			 
    nth(N, Row, Elem),
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow),
    !.
projectRowToVarList(Row, TableVars, [_ | ProjectedVars], [_ | ProjRow]) :- 
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow).





projectTableToVarList([], TableVars, ProjectedVars, []) :- 
    !.
projectTableToVarList([[Value, MaxBlock | Row] | Table], TableVars, ProjectedVars, [[Value, MaxBlock | ProjRow] | ProjTable]) :- 
    projectRowToVarList(Row, TableVars, ProjectedVars, ProjRow),
    projectTableToVarList(Table, TableVars, ProjectedVars, ProjTable).
    			       



mergeTables([[x,y | _]], Table, Table) :- 
    !.	      
mergeTables(Table, [[x,y | _]], Table) :- 
    !.	      
mergeTables([[x, y | Vars1] | Table1], [[x, y | Vars2] | Table2], [[x, y | MergedVars] | MergedTable]) :- 
    merge_var_lists(Vars1, Vars2, MergedVars),
    projectTableToVarList(Table1, Vars1, MergedVars, ProjTable1),
    projectTableToVarList(Table2, Vars2, MergedVars, ProjTable2),
    conc(ProjTable1, ProjTable2, MergedTable).
        
 
 
    
getPredTablesFromLit(Pred, NegPred, _, _, _, LitTable, NegLitTable) :- 
    once((Pred \== eq)),			   
    recorded(predTable, predTable(Pred, LitTable), _),
    recorded(predTable, predTable(NegPred, NegLitTable), _),
    !.
getPredTablesFromLit(eq, not_eq, AllVarObjs, [X1,X2], I, [Vars | LitTable], [Vars | NegLitTable]) :- 
    member([eq | PredLits], I),
    member([not_eq | NegPredLits], I),
    member(varobj(X1, TypeObjs), AllVarObjs),
    (X2 = b ->
     (Vars = [X2,X1],
      findall([Y2,Y1], (member(eq(Y1,Y2), PredLits), once(member(Y1,TypeObjs)), once(member(Y2,TypeObjs))), LitTable),
      findall([Y2,Y1], (member(not_eq(Y1,Y2), NegPredLits), once(member(Y1,TypeObjs)), once(member(Y2,TypeObjs))), NegLitTable))
     ;
     (Vars = [X1,X2],
      findall([Y1,Y2], (member(eq(Y1,Y2), PredLits), once(member(Y1,TypeObjs)), once(member(Y2,TypeObjs))), LitTable),
      findall([Y1,Y2], (member(not_eq(Y1,Y2), NegPredLits), once(member(Y1,TypeObjs)), once(member(Y2,TypeObjs))), NegLitTable))),
    !.
getPredTablesFromLit(Pred, NegPred, AllVarObjs, LitArgs, I, [Vars | LitTable], [Vars | NegLitTable]) :- 
    member([Pred | PredLits], I),
    member([NegPred | NegPredLits], I),
    (memberchk(b, LitArgs) ->
        (nth(N, LitArgs, b, Vars1),
	 Vars = [b | Vars1],
	 findall(Row, (member(Lit, PredLits), once((Lit =.. [_ | Row1], nth(N,Row1,X,Row2), Row = [X | Row2]))), LitTable),
	 findall(Row, (member(Lit, NegPredLits), once((Lit =.. [_ | Row1], nth(N,Row1,X,Row2), Row = [X | Row2]))), NegLitTable))
	;
	(Vars = LitArgs, 
	 findall(Row, (member(Lit, PredLits), once((Lit =.. [_ | Row]))), LitTable),
	 findall(Row, (member(Lit, NegPredLits), once((Lit =.. [_ | Row]))), NegLitTable))),
    recorda(predTable, predTable(Pred, [Vars | LitTable]), _),
    recorda(predTable, predTable(NegPred, [Vars | NegLitTable]), _).
		   

    

fodd_inference_by_variable_elimination(_, _, []) :- 
    !.
fodd_inference_by_variable_elimination(I, AllVarObjs, [node(Index, V, -1, -1) | D]) :- 
    recorda(varElimTable, varElimTable(Index, [[x,y],[V,[[]]]]), _),
    fodd_inference_by_variable_elimination(I, AllVarObjs, D),
    !.    
fodd_inference_by_variable_elimination(I, AllVarObjs, [node(Index, Lit, L, R) | D]) :- 
    Lit =.. [LitPred | LitArgs],
    getNeg(LitPred, NegLitPred),
    getPredTablesFromLit(LitPred, NegLitPred, AllVarObjs, LitArgs, I, LitTable, NegLitTable),
    recorded(varElimTable, varElimTable(L, LTable), _),
    recorded(varElimTable, varElimTable(R, RTable), _),
    multiplyTables(LitTable, LTable, LLitTable),
    write(Index), nl, write('LTable  '), write(LTable), nl, write('RTable  '), write(RTable), nl,
    multiplyTables(NegLitTable, RTable, RLitTable),
    mergeTables(LLitTable, RLitTable, NodeTable),
    write('NodeTable  '), write(NodeTable), nl,
%    (Index = 11, trace ; true),
    optimizeTable(Index, NodeTable, OptimizedTable),
    write('OptimizedTable  '), write(OptimizedTable), nl,
    recorda(varElimTable, varElimTable(Index, OptimizedTable), _),
    fodd_inference_by_variable_elimination(I, AllVarObjs, D).
 


     





fodd_infer(I, D, Edges, Res) :- 
    D = [node(RootIndex, _, _, _) | _],
    getDVars(D,Vars),
    recorda(varToElim, varToElim(RootIndex,[b | Vars]), _),
    mark_variables_to_eliminate(D),
    reverse(D,Drev),
    recorded(rewdPred, RewdPred, _),  
    interpretationObjs3579(RewdPred, Dom),
    get_var_type_objs([b | Vars], D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    fodd_inference_by_variable_elimination(I, AllVarObjs, Drev),
    eraseall(varToElim),
    recorded(varElimTable, varElimTable(RootIndex, [[x,y],[Res,MaxBlock]]), _),
    eraseall(varElimTable),
    eraseall(predTable),
    findall(X:Y, (member(X, [b | Vars]), once((member([X:Y], MaxBlock) ; copy_term(Z,Y)))), MaxBlockKey),
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






perform_gfodd_reduction_by_ve([], _, _, _, _, Edges, Edges, []) :- 
    !.
perform_gfodd_reduction_by_ve([I | Ints], AllVarObjs, D, Drev, DVars, EdgesSoFar, Edges, [Map | AllMaps]) :-   
    fodd_inference_by_variable_elimination(I, AllVarObjs, Drev),
    recorded(varElimTable, varElimTable(RootIndex, [[x,y],[Map,MaxBlock]]), _),
    eraseall(varElimTable),
    eraseall(predTable),
%    findall(X:Y, (member(X, [b | DVars]), once((member([X:Y], MaxBlock) ; copy_term(Z,Y)))), MaxBlockKey),    
    write('Map='), write(Map), 
%    write('  MaxBlockKey = '), write(MaxBlockKey), nl,
%    get_edges_from_maxBlock(RootIndex, D, MaxBlockKey, I, EdgesI),	
     get_edges_from_all_maxBlocks(D, [b | DVars], MaxBlock, I, EdgesI),
    write(' EdgesI: '), write(EdgesI), nl, 
    batchInsert(EdgesI, EdgesSoFar, EdgesSoFar1),
    perform_gfodd_reduction_by_ve(Ints, AllVarObjs, D, Drev, DVars, EdgesSoFar1, Edges, AllMaps).
 




gfodd_reduction_by_ve(D, R) :-
    D = [node(RootIndex, _, _, _) | _],
    getDVars(D,Vars),
    recorda(varToElim, varToElim(RootIndex,[b | Vars]), _),
    mark_variables_to_eliminate(D),
    reverse(D,Drev),
    recorded(rewdPred, RewdPred, _),  
    interpretations3579(RewdPred, Ints),
    interpretationObjs3579(RewdPred, Dom),
    !,
    get_var_type_objs([b | Vars], D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    !,
    perform_gfodd_reduction_by_ve(Ints, AllVarObjs, D, Drev, Vars, [], Edges, AllMaps),
    eraseall(varToElim),
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
     R = D
     ;	       	       
     insert(node(0,0.0,-1,-1),D, D0),
     replace_edge_target(D0, EdgesToRemove, D1),
     D1 = [D1Root | _],
     recorda(indexCount,2,_),    
     apply1(D1Root, D1, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
     eraseall(indexCount),
     all(X, recorded(apply, arc(_,X), _), Res),
     eraseall(apply),
     insertsort(Res, R)).



 
testbf(Res) :- 	   
    D = [node(7,p(v1),6,5), node(6,q(v1,v2),3,5), node(5,r(v2,b),4,2), node(4,10.0,-1,-1), node(3,5.0,-1,-1), node(2,2.0,-1,-1)], 	
    I = [[p,p(a)],[not_p,not_p(b)],[q,q(a,a),q(b,a),q(b,b)],[not_q,not_q(a,b)],[r,r(a,d),r(b,d)],[not_r,not_r(a,c),not_r(b,c)]],
    Dom = [objects(loc,[a,b]),objects(tr,[c,d])],
    getDVars(D, DVars),
    AvgVar = b,
    get_var_type_objs([AvgVar | DVars], D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    getSubsKey([AvgVar | DVars], [AvgVar:AvgVarVar | Key]),
    variablizeDD(D, [AvgVar:AvgVarVar | Key], VarD),
    fodd_eval(VarD, I, Key, AllVarObjs, AvgVarVar, AvgVarObjs, Edges, Res).



testve(Edges, Res) :- 
    D = [node(7,p(v1),6,5), node(6,q(v1,v2),3,5), node(5,r(v2,b),4,2), node(4,10.0,-1,-1), node(3,5.0,-1,-1), node(2,2.0,-1,-1)], 	
    I = [[p,p(a)],[not_p,not_p(b)],[q,q(a,a),q(b,a),q(b,b)],[not_q,not_q(a,b)],[r,r(a,d),r(b,d)],[not_r,not_r(a,c),not_r(b,c)]],
    fodd_infer(I, D, Edges, Res).
    



run_n_times(0) :- 
    !.
run_n_times(N) :- 
   N1 is N - 1,
   testve(X),	
   run_n_times(N1).
