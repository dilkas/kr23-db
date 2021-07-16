   


path_traversed(_, []) :-
    !,
    fail.
path_traversed(path(_,_,_,Path), [I | Rest]) :- 		     
    not(not(subsumption(Path, I, noconv, 1))),
    !.
path_traversed(Path, [_ | Rest]) :- 
    path_traversed(Path, Rest).		     





replace_edge_target([], _, []) :- 
    !.
replace_edge_target([node(I, Lit, L, R) | Rest], Edges, [node(I, Lit, 0, R) | Rest1]) :-
    member(I-t, Edges),
    replace_edge_target(Rest, Edges, Rest1),
    !.
replace_edge_target([node(I, Lit, L, R) | Rest], Edges, [node(I, Lit, L, 0) | Rest1]) :-
    member(I-f, Edges),
    replace_edge_target(Rest, Edges, Rest1),
    !.
replace_edge_target([Node | Rest], Edges, [Node | Rest1]) :-
    replace_edge_target(Rest, Edges, Rest1).		  




%%%%%%%%%%%%%%%%%%%%%%%%%%%%% R12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_table_size([], Rows, Rows) :- 
    !.
get_table_size([varobj(_, TypeObjs) | Rest], RowsSoFar, Rows) :- 
    length(TypeObjs, Len),	
    RowsSoFar1 is RowsSoFar * Len,
    get_table_size(Rest, RowsSoFar1, Rows).





perform_r12(_, _, _, _, [], Edges, Edges, []) :- 
    !.
perform_r12(D, MaxKey, AvgKey, AllVarObjs, [I | Rest], EdgesSoFar, Edges, [Map | Maps]) :- 
    fodd_eval(D, I, MaxKey, AvgKey, AllVarObjs, EdgesI, Map),
%    write('Map='), write(Map), write(' EdgesI: '), write(EdgesI), nl, 
    batchInsert(EdgesI, EdgesSoFar, EdgesSoFar1),        
    perform_r12(D, MaxKey, AvgKey, AllVarObjs, Rest, EdgesSoFar1, Edges, Maps).



       

r12(D, R) :- 
    recorded(rewdPred, RewdPred, _),   
    interpretationObjs3579(RewdPred, Dom),
    recorded(rewdLit,RewdLit, _),
    getDVars(D, DVars),
    separateAvgMaxVars(DVars, AvgDVars, MaxDVars),
    
%    length(DVars, DVarsLen),
%    write('Reducing a diagram with '), write(DVarsLen), write(' variables'), nl,  
    get_var_domains(DVars, D, Dom, AllVarObjs),
%    get_table_size(AllVarObjs, 1, Rows),
%    write('Table of '),write(Rows),write(' rows and '),write(DVarsLen),write(' columns'),nl,
    getSubsKey(AvgDVars, AvgKey),
    getSubsKey(MaxDVars, MaxKey),
    conc(AvgKey,MaxKey,Key),
    variablizeDD(D, Key, VarD),
    interpretations3579(RewdPred, Ints),
    perform_r12(VarD, MaxKey, AvgKey, AllVarObjs, Ints, [], Edges, AllMaps),
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
      r12_node(Res1, R),
      erase(NodeRedRef))
     ;
     (R = Res1)). 
     
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reduce Max Eq %%%%%%%%%%%%%%%%%%%%


reduce_max_eq_root(D, Root, Res) :- 
    del(node(Root,eq(a,_),_,0), D, D1),
    D1 = [node(Root1,_,_,_) | _],
    reduce_max_eq_root(D1,Root1,Res),	
    !.
reduce_max_eq_root(D, Root, Res) :- 
    del(node(Root,eq(a,_),0,_), D, D1),
    D1 = [node(Root1,_,_,_) | _],
    reduce_max_eq_root(D1,Root1,Res),	
    !.
reduce_max_eq_root(D, _, D). 


    
reduce_max_eq(D, I, Res) :-
    member(node(I,eq(a,_),L,0), D),
    parentUpdate(D, L, I, D1, _),
    reduce_max_eq(D1, L, Res),
    !.
reduce_max_eq(D, I, Res) :-
    member(node(I,eq(a,_),0,R), D),
    parentUpdate(D, R, I, D1, _),
    reduce_max_eq(D1, R, Res),
    !.     
reduce_max_eq(D, I, Res) :-
    member(node(I,eq(a,_),L,R), D),
    reduce_max_eq(D, L, D1),
    reduce_max_eq(D1, R, Res),    
    !.     
reduce_max_eq(D, _, D).



reduce_max_eq(D, Res) :- 
    D = [node(I,_,_,_) | _],
    reduce_max_eq_root(D, I, D1),
    reduce_max_eq(D1, I, Res).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Reduction Before Max over Actions %%%%%%%%%%%%%%%%%%%%%

get_dominance_info([], [], [], []) :- 
    !.
get_dominance_info([Map1 | Maps], [Map2 | AllMaps], [Map1 | Other], [gt | Rest]) :- 
    Map1  > Map2,
    get_dominance_info(Maps, AllMaps, Other, Rest),
    !.
get_dominance_info([Map1 | Maps], [Map2 | AllMaps], [Map2 | Other], [lt | Rest]) :- 
    Map1  < Map2,
    get_dominance_info(Maps, AllMaps, Other, Rest),
    !.
get_dominance_info([Map1 | Maps], [_ | AllMaps], [Map1 | Other], [eq | Rest]) :- 
    get_dominance_info(Maps, AllMaps, Other, Rest).




get_updated_DomActionList(_, [], [], []) :- 
    !.
get_updated_DomActionList(NewAction, [gt | Rest], [Action | Rest1], [Action | Other]) :- 
    get_updated_DomActionList(NewAction, Rest, Rest1, Other),
    !.
get_updated_DomActionList(NewAction, [_ | Rest], [_ | Rest1], [NewAction | Other]) :- 
    get_updated_DomActionList(NewAction, Rest, Rest1, Other).






get_dominating_ops(_, [], DomActionList, DomActionList) :- 
    !.
get_dominating_ops(Maps, [action_values(Action, AllMaps) | Rest], DomActionListSoFar, DomActionList) :- 
    get_dominance_info(Maps, AllMaps, UpdatedMaps, DomInfo),
    (not(member(gt, DomInfo)),
     length(Maps, LenMaps), 
     mklist(Action, LenMaps, [], DomActionListSoFar1)
     ;
     not(member(lt, DomInfo)),
     DomActionListSoFar1 = DomActionListSoFar
     ;
     get_updated_DomActionList(Action, DomInfo, DomActionListSoFar, DomActionListSoFar1)),	
    get_dominating_ops(UpdatedMaps, Rest, DomActionListSoFar1, DomActionList).	    




remove_dominated_actions([], [], DomActionList, []) :- 
    !.
remove_dominated_actions([_ | Rest], [Op | Rest1], DomActionList, [-1 | Other]) :- 
    not(memberchk(Op, DomActionList)),
    remove_dominated_actions(Rest, Rest1, DomActionList, Other),
    !.
remove_dominated_actions([ActionDiag | Rest], [_ | Rest1], DomActionList, [ActionDiag | Other]) :- 
    remove_dominated_actions(Rest, Rest1, DomActionList, Other).





reduce_action_list([]) :- 
    !.
reduce_action_list(ActionList, ReducedActionList) :- 
    findall(action_values(Action, AllMaps), recorded(max_over_actions_values, action_values(Action,AllMaps), _), [action_values(FirstAction, FirstAllMaps) | AllActionValues]),
%    write([action_values(FirstAction, FirstAllMaps) | AllActionValues]), nl,
    length(FirstAllMaps, LenAllMaps),
    mklist(FirstAction, LenAllMaps, [], DomActionList),
    get_dominating_ops(FirstAllMaps, AllActionValues, DomActionList, FinalDomActionList),
%    write('Dom Action List: '), write(FinalDomActionList), nl, 
    ops(OpList),
    reverse(OpList, OpList1),
    remove_dominated_actions(ActionList, OpList1, FinalDomActionList, ReducedActionList).





update_action_diagram_maps(_, _, []) :- 
    !.
update_action_diagram_maps(Action, Count, [Map | Maps]) :- 
    (recorded(max_over_actions_value(Count), max_over_actions_value(Action1, Map1), Ref) ->
        (Map > Map1 ->			     
	     (erase(Ref),
	      recorda(max_over_actions_value(Count), max_over_actions_value(Action, Map), _),
	      recorda(not_dominated, not_dominated, _)
	     )
	     ;
	     (true)
	)
	;
	(recorda(max_over_actions_value(Count), max_over_actions_value(Action, Map), _))),
    Count1 is Count + 1,
    update_action_diagram_maps(Action, Count1, Maps).




r12_for_max_over_actions(D, Action, R) :- 
    recorded(rewdPred, RewdPred, _),   
    interpretationObjs3579(RewdPred, Dom),
    recorded(rewdLit,RewdLit, _),
    RewdLit =.. [_ | [AvgVar]],
    getDVars(D, DVars),
    get_var_type_objs([AvgVar | DVars], D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    getSubsKey([AvgVar | DVars], [AvgVar:AvgVarVar | Key]),
    variablizeDD(D, [AvgVar:AvgVarVar | Key], VarD),
    interpretations3579(RewdPred, Ints),
    perform_r12(VarD, Key, AllVarObjs, AvgVarVar, AvgVarObjs, Ints, [], Edges, Maps),
%    write('Action Maps for : '), write(Action), write(': '), write(Maps),nl,
    update_action_diagram_maps(Action, 1, Maps),
    (not(recorded(not_dominated, not_dominated, _)) ->
     (R = -1)
     ;
     eraseall(not_dominated),
     (get_all_edges(D, AllEdges),
     diff(AllEdges, Edges, EdgesToRemove),
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
      insertsort(Res, R)))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NODE REDUCTION %%%%%%%%%%%%%%%%%%%%%%%%%

inverse_key([]) :- 
    !.
inverse_key([X:X | Rest]) :-
    inverse_key(Rest).


r12_node_test_candidate(D, Key, AllVarObjs, AvgVarVar, AvgVarObjs, [], []) :- 
    !.
r12_node_test_candidate(D, Key, AllVarObjs, AvgVarVar, AvgVarObjs, [OldMap | AllMaps], [I | Ints]) :- 
    fodd_eval(D2, I, Key, AllVarObjs, AvgVarVar, AvgVarObjs, _, Map),
    !,
    Map = OldMap,
    !,
    r12_node_test_candidate(D, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints).





r12_node_test_all_candidates(D, [], Key, _, _, _, _, _, D) :- 
    inverse_key(Key),
    !.
r12_node_test_all_candidates(D, [CandNode | Rest], Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res) :- 
    once((del(node(CandNode, _, L, R), D, D1),
    	  parentUpdate(D1, L, CandNode, DL, _))),
    r12_node_test_candidate(DL, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints),
    !,
    DL = [DLRoot | _],	       
    recorda(indexCount,2,_),    
    apply1(DLRoot, DL, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
    eraseall(indexCount),
    all(X, recorded(apply, arc(_,X), _), DL1),
    eraseall(apply),
    insertsort(DL1, DL2),
    !,
    r12_node_test_all_candidates(DL2, Rest, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res),
    !.
r12_node_test_all_candidates(D, [CandNode | Rest], Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res) :- 
    once((del(node(CandNode, _, L, R), D, D1),
    	  parentUpdate(D1, R, CandNode, DR, _))),
    r12_node_test_candidate(DR, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints),
    !,
    DR = [DRRoot | _],	       
    recorda(indexCount,2,_),    
    apply1(DRRoot, DR, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
    eraseall(indexCount),
    all(X, recorded(apply, arc(_,X), _), DR1),
    eraseall(apply),
    insertsort(DR1, DR2),
    !,
    r12_node_test_all_candidates(DR2, Rest, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res),
    !.
r12_node_test_all_candidates(D, [CandNode | Rest], Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res) :-
    r12_node_test_all_candidates(D, Rest, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res).







r12_node(D, Res) :- 
    recorded(rewdPred, RewdPred, _),   
    interpretationObjs3579(RewdPred, Dom),
    recorded(rewdLit,RewdLit, _),
    RewdLit =.. [_ | [AvgVar]],
    getDVars(D, DVars),
    get_var_type_objs([AvgVar | DVars], D, Dom, [AvgVarObjs | VarObjs], VarEqList),
    get_rem_var_type_objs(VarEqList, [AvgVarObjs | VarObjs], D, Dom, AllVarObjs),
    getSubsKey([AvgVar | DVars], [AvgVar:AvgVarVar | Key]),
    variablizeDD(D, [AvgVar:AvgVarVar | Key], VarD),
    interpretations3579(RewdPred, Ints),	    
    findall(Index, member(node(Index,eq(_,_),_,_), D), Candidates),    
    recorded(node_reduction, node_reduction(AllMaps), _),
    r12_node_test_all_candidates(VarD, Candidates, Key, AllVarObjs, AvgVarVar, AvgVarObjs, AllMaps, Ints, Res).


