 :- dynamic
    debug/1.

:- use_module(library(ordsets)).
:- use_module(library(system)).
:- use_module(library(random)).





vi(X) :-
    rewardList(RL),
    length(RL, RLlen), !,
    (integer(X),mklist(X,RLlen,[],I),!;I=X),
    length(I, RLlen),
     all_rewds(I, RL).



getIListSubset(IList, Count, IListSubset) :-
    random_permutation(IList, Count, IListSubset).



%get_actions_with_sp_preconds(Actions) :-
%    special_predicates(SpPreds),				      
%    ops(Ops),
%    findall(, (member(Action,Ops), 
%member(Sp, SpPreds), 
%once(functor(Sp,SpPred,_)),
%tvd(Action,_,Pred,Args,_,_,D),
%once((Pred \== SpPred)), 
%member(node(_,Lit,_,_),D),
%once((Lit =.. [SpPred | Args1])),
%once((Args1 \== Args)))), Actions).

    
all_rewds([], []) :- 
    !.
all_rewds([X | Other], [Rewd | Rest]) :-
    recorda(plan,X,RefPlan),
    recorda(rewd,Rewd,RefRewd),
    Rewd = reward(RewdD),
    RewdD = [node(_, RewdLit, _, _) | _],
    recorda(rewdLit,RewdLit,RefRewdLit),
    functor(RewdLit,RewdPred,RewdArgCount),    
    recorda(rewdPred,RewdPred,RefRewdPred),
    getAvgDVars(RewdD,RewdLitVars),
    recorda(avgVars,avgVars,RefAvgVars),
    valueIterate(X, _),
    erase(RefRewd),
    erase(RefPlan),
    erase(RefRewdLit),
    erase(RefRewdPred),
    erase(RefAvgVars),
    all_rewds(Other, Rest).




valueIterate(N, VF) :- 
    recorded(rewd,reward(Rewd),_),
    (N = 1 -> (InitCount = 1) ; (ops(OpList),length(OpList, InitCount))),
    recorda(varCount,InitCount,_),
    stdApart(Rewd, Rewd1),
    write('Reward Function:'), nl, pp(Rewd1),
%    get_actions_with_sp_preconds(Actions),
%    recorda(actions_sp_preconds, actions_sp_preconds(Actions), SpPrecondsRef),
    getVF(Rewd1, N, VF),
%    erase(SpPrecondsRef),
    eraseall(varCount).




value_function_converged([], [], _) :-
    !.
value_function_converged([OldX | RestOld], [NewX | RestNew], Epsilon) :-
    Diff = OldX - NewX,
    !,
    abs(Diff) < Epsilon,
    !,
    value_function_converged(RestOld, RestNew, Epsilon).



value_function_converged(Round) :-
    PrevRound is Round + 1,
    recorded(max_over_actions_values, action_values(vf(PrevRound),OldMaps), Ref1),
    recorded(max_over_actions_values, action_values(vf(Round),NewMaps), Ref2),
    erase(Ref1),
    epsilon(Epsilon),
    !,
    value_function_converged(OldMaps, NewMaps, Epsilon).




getVF(VF, 0, VF) :- 
    !.
getVF(VF, Rounds, FinalVF) :-     
    write(Rounds), write(' to go !!'), nl,
    system("date"),
    getMaxDVars(VF, VFvars),
    (debug(main) -> (write('Entering Regression over Nature Actions'), nl) ; true),

    exo_ops(ExoOpList),   
    regress_exo(VF, VFvars, ExoOpList, ExoRegVF),
    !,
    (debug(main) -> (write('Regression over Exo Action: '), nl, pp(ExoRegVF)) ; true),
    (debug(main) -> (write('Entering Regression over Agent Actions'), nl) ; true),
    ops(OpList),    
    regress(Rounds, ExoRegVF, VFvars, [], [], OpList, RegVF),
    (unify_avg_var_at_iteration_boundary ->
    	(unify_max_avg_variable_for_sp_predicate(RegVF, NextVF1))
	;
	(NextVF1 = RegVF)),
    
    (recorded(converged, converged, RefConv) ->
	(erase(RefConv),
	 Rounds1 is 0)
	;
        (Rounds1 is Rounds - 1)),

    (Rounds1 = 1 -> (InitCount = 1) ; (length(OpList, InitCount))),
    eraseall(varCount),
    recorda(varCount,InitCount,_),
    stdApart(NextVF1, NextVF),
    !,
    pp(NextVF),
%    write(NextVF),nl,
    getVF(NextVF, Rounds1, FinalVF).






objMax(VF, Action, ActParams, NewAction, ObjMaxVF) :- 
    sort(ActParams, ActParams1),
    length(ActParams1, Len),    
    recorded(varCount, Start, RefVarCount),
    erase(RefVarCount),
    getNewVars(Start, Len, NewVars),
    Start1 is Start + Len,
    recorda(varCount,Start1,_),
    getGroundKey(ActParams1, NewVars, Key),
    variablizeTerm(Action, Key, NewAction), 
    variablizeDD(VF, Key, ObjMaxVF).
 




reduce(D, uni, _, ReducedD) :-
    (variableElimination -> 
        (gfodd_reduction_by_ve(D, ReducedD))
	;
	r12(D, ReducedD)),
    !.
reduce(D, bi, high_first, ReducedD) :-
    recorda(maxblock_prefer_high, maxblock_prefer_high, MBPreferHighRef),
    (variableElimination -> 
        (gfodd_reduction_by_ve(D, ReducedD1))
	;
	r12(D, ReducedD1)),	
    erase(MBPreferHighRef),
    (variableElimination -> 
        (gfodd_reduction_by_ve(ReducedD1, ReducedD))
	;
	r12(ReducedD1, ReducedD)),
    !.
reduce(D, bi, low_first, ReducedD) :-
    (variableElimination -> 
        (gfodd_reduction_by_ve(D, ReducedD1))
	;
	r12(D, ReducedD1)),	
    recorda(maxblock_prefer_high, maxblock_prefer_high, MBPreferHighRef),
    (variableElimination -> 
        (gfodd_reduction_by_ve(ReducedD1, ReducedD))
	;
	r12(ReducedD1, ReducedD)),
    erase(MBPreferHighRef).


    	

reduce(D, ReducedD) :- 
    modelchecking,
    !,
    (bidirectional_reduction ->
        (bidirectional_reduction_high_first ->
	    (reduce(D,bi,high_first,ReducedD))
	    ;
	    (reduce(D,bi,low_first,ReducedD)))
	;	
    	(reduce(D,uni,1,ReducedD))),
    !.	
reduce(D, D).







max_over_actions(Round, [], VF1, Rewd, VF) :-
    Rewd = [RewdRoot | _],
    recorded(rewdPred,RewdPred,_),
    (discountfactor(RewdPred, Op, DDD); DDD= 0.999),
    VF1 = [VF1Root | _],	
    apply(VF1Root, VF1, node(2, DDD, -1, -1), [node(2, DDD, -1, -1)], *, [DiscRoot | DiscountedVF]),
    (debug(main) -> (write('Multiplied Discount factor:'), nl, pp([DiscRoot | DiscountedVF])) ; true),
    (absorbingstate ->	
	apply(DiscRoot, [DiscRoot | DiscountedVF], RewdRoot, Rewd, max, VF2)
	;
        apply(DiscRoot, [DiscRoot | DiscountedVF], RewdRoot, Rewd, +, VF2)),
    (debug(main) -> (write('Added Rewd Function:'), nl, pp(VF2)) ; true),	
    recorda(max_over_actions_reduction, vf(Round), _),
    reduce(VF2, VF),	
    !.
max_over_actions(Round, [-1 | ActionList], SoFar, Rewd, VF) :-
    max_over_actions(ActionList, SoFar, Rewd, VF),
    !.
max_over_actions(Round, [First | ActionList], SoFar, Rewd, VF) :-
    (debug(main2) -> (!, tryeval2(First, 0, 'First')) ; (true)),
    (debug(main2) -> (!, tryeval2(SoFar, 0, 'SoFar')) ; (true)),
    SoFar = [Rootsf | VFsofar],
    First = [Root | ReducedVF],
    (debug(main) -> (write('First: '), nl, pp(First), nl) ; true),
    (debug(main) -> (write('SoFar: '), nl, pp(SoFar), nl) ; true),
    apply(Rootsf, [Rootsf | VFsofar], Root, [Root | ReducedVF], max, NewVFsofar1), 
    (debug(main2) -> (!, tryeval2(NewVFsofar1, 0, 'New SoFar')) ; (true)),			
    (debug(main) -> (write('Max Over Actions in Progress'), nl, pp(NewVFsofar1), nl) ; true), 
    (debug(main) -> write('So Far now: '), nl, write(NewVFsofar1), nl  ; true),
     !,
    reduce(NewVFsofar1, NewVFsofar2),
    (debug(main) -> write('Max Reduced: '), nl, pp(NewVFsofar2), nl ; true),
    reduce_max_eq(NewVFsofar2,NewVFsofar),
    (debug(main2) -> (!, tryeval2(NewVFsofar, 0, 'New SoFar Reduced')) ; (true)),
    (debug(main) -> write('Max Eq Reduced: '), nl, pp(NewVFsofar), nl ; true),
    max_over_actions(Round, ActionList, NewVFsofar, Rewd, VF).
    









regress_exo(VF, _, [], VF) :- 
    !.
regress_exo(VF, VFvars, [ExoOp | ExoOpList], ExoRegVF) :- 
    special_predicates(SpList),
    all(VarSp, (member(Sp,SpList), once((Sp =.. [_ | SpVars], getSubsKey(SpVars, Key), variablizeTerm(Sp,Key,VarSp))), member(node(_, VarSp, _, _), VF)), AllSpecial),
    write('All Special :'), write(AllSpecial), nl,  	
    (debug(main) -> (write(ExoOp), nl) ; true),
    exo_operator(ExoOp, ActParams, EventList),
    recorda(regr_exo_op, regr_exo_op, RegrExoOpRef),
    regressOverOp(VF, VFvars, ExoOp, [node(0, 0.0, -1, -1)], EventList, ExoRegVF1),
    erase(RegrExoOpRef),
    !,
    (debug(main) -> (write('Regression over Exo Action before Reduction'), nl, write(ExoRegVF1), nl) ; true),
    reduce(ExoRegVF1, ExoRegVF2),
    (debug(main) -> (write('Regression over Exo Action after Reduction'), nl, write(ExoRegVF2), nl) ; true),	
    regress_exo(ExoRegVF2, VFvars, ExoOpList, ExoRegVF).
    

outputPolicy([]) :- 
     !.
outputPolicy([[FinalAction1, FinalActionParams1, FinalVF1] | ActionRules]) :-
     FinalVF1 = [node(FinalRoot, _, _, _) | _],
     getDVars(FinalVF1, FinalVars),
     getSubsKey(FinalActionParams1, ActParamsKey),
     variablizeTerm(FinalAction1, ActParamsKey, ActionVar),
     variablizeDD(FinalVF1, ActParamsKey, FinalVarVF),
     outpolicyfile3579(OPF),
     open(OPF, append, S), 
     tell(S),
     write(actionDiag(ActionVar, FinalVarVF)),write('.'), nl, nl, nl,
     close(S).


regress(Round, VF, _, ActionList, ActionRules, [], RegVF) :-
    (Round == 1 -> 
        (recorded(rewdPred,RewdPred,_),
        (discountfactor(RewdPred, Op, DDD); DDD= 0.999),	
	VF = [VFRoot | _],
        apply(VFRoot, VF, node(2, DDD, -1, -1), [node(2, DDD, -1, -1)], *, DiscountedVF),
        outpolicyfile3579(OPF),
        open(OPF, append, S), 
        tell(S),
        write(actionDiag(noop, DiscountedVF)),write('.'), nl, nl, nl,
        close(S),
        RegVF = [node(0, 0.0, -1, -1)]) 
       ;
       ((empirical_max_reduction ->
     	  (reduce_action_list(ActionList, ActionList1))
	  ; 
          (ActionList1 = ActionList)),
        !,     
        (del(First, ActionList1, ActionList2), First \== -1),
        recorded(rewd,reward(Rewd),_),
        stdApart(Rewd, Rewd1),  
        eraseall(varCount),
        recorda(varCount, 1, _),
        !,  
        max_over_actions(Round, ActionList2, First, Rewd1, RegVF),
%(Round == 5,trace ; true),

	(value_function_converged(Round) ->
		(outputPolicy(ActionRules),
		 recorda(converged, converged, _))
		;
		(true)))),
    (debug(main1) -> (!, tryeval2(VF, Round, 'VF')) ; (true)),
    !.
regress(Round, VF, VFvars, Actionsofar, ActionRulesSoFar, [Op | OpList], RegVF) :- 
    operator(Op, ActParams, [CostRoot | Cost], EventList),
    actionDiagParams(AllActParams),
    Action =.. [Op | ActParams],
    (debug(main) -> write(Op), nl ; true),
    recorded(varCount,Start,_),
    !,  
    regressOverOp(VF, VFvars, Op, [node(0, 0.0, -1, -1)], EventList, RegOpVF),

    (debug(main1) -> (!, tryeval(RegOpVF, Round, Op, 'RegOpVF')) ; (true)),

    RegOpVF = [RegOpRoot | _],
    most_action_variants(MAV),
    length(VFvars, VFvarsLen),
    Start1 is Start + (MAV * VFvarsLen),
    eraseall(varCount), 
    recorda(varCount,Start1,_),
    (debug(main) -> (write(Op), write(' variants added:'), nl, pp(RegOpVF)) ; true),

    apply(RegOpRoot, RegOpVF, CostRoot, [CostRoot | Cost], -, CostRedVF), 
    (debug(main) -> (write(Op), write(' cost subtracted:'), nl, pp(CostRedVF)) ; true),

    CostRedVF = [CostRedRoot | _],
    recorded(rewdPred,RewdPred,_),
    (discountfactor(RewdPred, Op, DDD); DDD= 0.999),	
    apply(CostRedRoot, CostRedVF, node(2, DDD, -1, -1), [node(2, DDD, -1, -1)], *, [DiscRoot | DiscountedVF]),
    (debug(main1) -> (!, tryeval([DiscRoot | DiscountedVF], Round, Op, 'Discounted')) ; (true)),
    (debug(main) -> (write('Multiplied Discount factor:'), nl, pp([DiscRoot | DiscountedVF])) ; true),
    recorded(rewd,reward(Rewd),_),
    Rewd = [RewdRoot | _],
    
    (absorbingstate ->	
	apply(DiscRoot, [DiscRoot | DiscountedVF], RewdRoot, Rewd, max, FinalVF1)
	;
        stdApart(Rewd, [RewdRoot1 | Rfn]),
        apply(DiscRoot, [DiscRoot | DiscountedVF], RewdRoot1, [RewdRoot | Rfn], +, FinalVF1)),
    (debug(main1) -> (!, tryeval(FinalVF1, Round, Op, 'FinalVF')) ; (true)),	
    (debug(main) -> (write('Added Rewd Function:'), nl, pp(FinalVF1)) ; true),	
		
     FinalAction1 = Action,
     FinalActionParams1 = ActParams,
     
     objMax(CostRedVF, Action, AllActParams, NewAction, ObjMaxVF),
     (debug(main1) -> (!, tryeval1(ObjMaxVF, Round, Op, NewAction, 'ObjMaxVF')) ; (true)),
     !,
     FinalAction2 = NewAction,
     NewAction =.. [_ | FinalActionParams2],
     (debug(main) -> (write('Object Maximized: '), nl, pp(ObjMaxVF), nl, write(ObjMaxVF), nl) ; true),
%     recorda(max_over_actions_reduction, Op, _),
     reduce(ObjMaxVF,ReducedVF),
     (debug(main) -> write('Reduced: '), nl, pp(ReducedVF), nl ; true),
     FinalVF2 = ReducedVF,
    
    (once((Round == 1)),
     FinalVF1 = [node(FinalRoot, _, _, _) | _],     
     getDVars(FinalVF1, FinalVars),
     getSubsKey(FinalActionParams1, ActParamsKey),
     variablizeTerm(FinalAction1, ActParamsKey, ActionVar),
     variablizeDD(FinalVF1, ActParamsKey, FinalVarVF),
     outpolicyfile3579(OPF),
     open(OPF, append, S), 
     tell(S),
     write(actionDiag(ActionVar, FinalVarVF)),write('.'), nl, nl, nl,
     close(S)
     ;
     true),
    (debug(main) -> write('RedVF: '), pp(FinalVF), nl ; true),
    eraseall(varCount), 
    recorda(varCount,Start,_),
    !,
    regress(Round, VF, VFvars, [ReducedVF | Actionsofar], [[FinalAction1, FinalActionParams1, FinalVF1] | ActionRulesSoFar], OpList, RegVF).
    


    

regressOverOp(_, _, _, RegOpVF, [], RegOpVF) :- 
    !.
regressOverOp(VF, VFvars, Op, [Rootsf | VFsofar], [ev(Event, [ProbRoot | ProbDD]) | EventList], RegOpVF) :- 
    VF = [node(I, _, _, _) | _],
    recorda(allnodekeys, [], _),
    getRegressedVF(I, VF, Op, Event, [Root1 | RegVF1]),
    !,     
    recorded(allnodekeys, AllNodeKeys, _),
    all(_, (member(NodeKey, AllNodeKeys), eraseall(NodeKey)), _),
    (debug(main1) -> (write(Event), nl) ; true), 
   (debug(main) -> (write(Op), write(' '), write(Event), write('  regressed'), nl, pp([Root1 | RegVF1])) ; true),
    apply(Root1, [Root1 | RegVF1], ProbRoot, [ProbRoot | ProbDD], *, [Root | RegVF]),
    !,
    (debug(main) -> (write('probability multiplied'), nl, pp([Root | RegVF])) ; true),
    apply(Rootsf, [Rootsf | VFsofar], Root, [Root | RegVF], +, NewVFsofar),
    (debug(main) -> (write(Op), write(' intermediate variants added'), nl, pp(NewVFsofar)) ; true),
    regressOverOp(VF, VFvars, Op, NewVFsofar, EventList, RegOpVF).
    






negateTVD([], []) :- 
    !.
negateTVD([node(1, 1.0, -1, -1) | Rest], [node(0, 0.0, -1, -1) | NegRest]) :-
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(0, 0.0, -1, -1) | Rest], [node(1, 1.0, -1, -1) | NegRest]) :-
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, 0, 1) | Rest], [node(I, Lit, 1, 0) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, 1, 0) | Rest], [node(I, Lit, 0, 1) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, 0, R) | Rest], [node(I, Lit, 1, R) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, 1, R) | Rest], [node(I, Lit, 0, R) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, L, 0) | Rest], [node(I, Lit, L, 1) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, L, 1) | Rest], [node(I, Lit, L, 0) | NegRest]) :- 
    negateTVD(Rest, NegRest),
    !.
negateTVD([node(I, Lit, L, R) | Rest], [node(I, Lit, L, R) | NegRest]) :- 
    negateTVD(Rest, NegRest).
    





getRegressedVF(I, VF, _, _, [node(I, N, -1, -1)]) :- 
    member(node(I, N, -1, -1), VF),
    !.
getRegressedVF(I, _, _, _, RegVF) :- 
    recorded(I, RegVF, _),
    !.
getRegressedVF(I, VF, Op, Event, RegVF) :- 
%    write(I),nl,	
    member(node(I, Lit, L, R), VF),
    Lit =.. [Pr | LitParams],
    special_predicates(SpList),
%    getArgOrder(LitParams, ArgOrder),
    ((recorded(regr_exo_op,regr_exo_op,_),member(Sp,SpList),functor(Sp,Pr,_),Sp \== Lit) ->
          (TVD = [node(2,Lit,1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])
	  ;
    	  ((tvd(Op, Event, Pr, ObjParams, ArgOrder, _, TVD1) ; (write(Op), write(' '), write(Event), write(' '), write(Pr), write(' not found'), nl, nl), !, fail),!,
    	  getGroundKey(ObjParams, LitParams, Key),
    	  variablizeDD(TVD1, Key, TVDunordered),
    	  reorder_diagram(TVDunordered, TVD),
    	  TVD = [node(Root, RootLit, RootL, RootR) | _])),
    getRegressedVF(L, VF, Op, Event, [Lroot | LRegVF]),
    getRegressedVF(R, VF, Op, Event, [Rroot | RRegVF]),
    !,
    negateTVD(TVD, NegTVD),
    NegTVD = [node(NegRoot, NegRootLit, NegRootL, NegRootR) | _], 
    apply(node(Root, RootLit, RootL, RootR), TVD, Lroot, [Lroot | LRegVF], *, [Lr | BBL]),
    apply(node(NegRoot, NegRootLit, NegRootL, NegRootR), NegTVD, Rroot, [Rroot | RRegVF], *, [Rr | BBR]),
    apply(Lr, [Lr | BBL], Rr, [Rr | BBR], +, RegVF),
    recorded(allnodekeys, AllNodeKeys, _),
    eraseall(allnodekeys),
    recorda(allnodekeys, [I | AllNodeKeys], _),
    recorda(I, RegVF, _).
    
    
















%%%%%%%%%%% DEPRECATED %%%%%%%%%%%%%%

equalizeGoalLits(_, [], []) :- 
    !.
equalizeGoalLits(GoalVar, [[GoalVar, Paths] | Rest], [Paths | Other]) :- 
    equalizeGoalLits(GoalVar, Rest, Other).

mergeAllPaths([], MergedPaths, MergedPaths) :- 
    !.
mergeAllPaths([Paths | Rest], MergedPathsSoFar, MergedPaths) :- 
    merge(MergedPathsSoFar, Paths, MergedPathsSoFar1),
    mergeAllPaths(Rest, MergedPathsSoFar1, MergedPaths).

post_process_policy_paths([], _, []) :- 
       !.
post_process_policy_paths([path(_, _, _, Path) | Rest], Goal, Res) :- 
       once(copy_term(Goal, Goal1)),
       subsumes2(Goal1, Path),
       post_process_policy_paths(Rest, Goal, Res),
       !.
post_process_policy_paths([path(N, L, EL, Path) | Rest], Goal, [path(N, L, EL, Path) | Res]) :- 
       post_process_policy_paths(Rest, Goal, Res).

extractDomConsts([], [], []) :- 
    !.
extractDomConsts([X | Rest], [X | RestDC], RestVarPars) :- 
    domConst(X),
    extractDomConsts(Rest, RestDC, RestVarPars),
    !.
extractDomConsts([X | Rest], RestDC, [X | RestVarPars]) :- 
    extractDomConsts(Rest, RestDC, RestVarPars).

getOrderNums(Count, Count, [Count]) :- 
    !.
getOrderNums(Start, Count, [Start | Rest]) :- 
    Start1 is Start + 1,
    getOrderNums(Start1, Count, Rest).

getArgOrder([], []) :- 
    !.
getArgOrder([X], [1]) :- 
    !.
getArgOrder(LitParams, ArgOrder) :- 
    extractDomConsts(LitParams, DomConsts, VarPars),
    sort(DomConsts, SortedDomConsts),
    sort(VarPars, SortedVarPars),
    conc(SortedDomConsts, SortedVarPars, SortedLitParams),
    length(SortedLitParams, Len),
    getOrderNums(1, Len, Nums),
    getGroundKey(SortedLitParams, Nums, Key),
    variablizeNumList(LitParams, Key, ArgOrder).
    



