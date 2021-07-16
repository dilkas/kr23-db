
:- dynamic 
    actionList/3,
    nf3579/2,
    testDomain/1,
    initState/1,
    testOps/1,
    goal/2,
    goal/1,
    rewdConj/1.

%:- use_module(library(random)).


    


get_highest_leaf_reached([], _, 0.0) :- 
    !.
get_highest_leaf_reached([path(Map, _, _, Path, _) | Rest], State, Map) :-
    not(not(subsumption(Path, State, noconv, 1))),
    !.
get_highest_leaf_reached([_ | Rest], State, Map) :- 
    get_highest_leaf_reached(Rest, State, Map).



get_highest_leaf_reached1([], _, _, 0.0) :- 
    !.
get_highest_leaf_reached1([path(Map, _, _, Path, Action) | Rest], State, Action, Map) :-
    not(not(subsumption(Path, State, noconv, 1))),
    !.
get_highest_leaf_reached1([_ | Rest], State, Action, Map) :- 
    get_highest_leaf_reached1(Rest, State, Action, Map).




get_log_of(0, 0.0) :- 
    !.
get_log_of(0.0, 0.0) :- 
    !.
get_log_of(X, Y) :- 
    Y is log(X).



get_ground_action_map1(P, Action, State, Goal, Map) :-
%    (Action = deliver(ball1,box1, loc1), trace ; true),
    findall(Leaf, (member(goal(RankValue, Lit), Goal), once((actionList(Lit, D), get_highest_leaf_reached1(D, State, Action, Leaf1), Leaf is Leaf1 * RankValue))), Maps),
%    write('Maps: '), write(Maps), nl, 
    sum_list(Maps, Map).



get_ground_action_map(P, Action, State, Goal, Map) :-
    findall(Leaf, (member(goal(RankValue, Lit), Goal), once((actionList(Action, Lit, D), get_highest_leaf_reached(D, State, Leaf1), Leaf is Leaf1 * RankValue))), Maps),
%    write('Maps: '), write(Maps), nl, 
    sum_list(Maps, Map).





get_best_ground_actions(ActSet, State, Goal, bestAct(HighestMap, ActList)) :-
    get_ground_action_maps(ActSet, State, Goal, ActList1),
    (ActList1 = [],
     HighestMap = 0.0,
     ActList = []
     ;
     (all(Leaf, member(act(Leaf, _), ActList1), Leaves) ; Leaves = []),
     maxNumList(Leaves, HighestMap),
     (all(Action, member(act(HighestMap, Action), ActList1), ActList) ; ActList = [])).
    
        
    




get_map(D, State, Map) :-
    get_leaves(D, LI1),
    insertsort(LI1, LeafIndices),
    get_highest_leaf_reached(LeafIndices, D, State, Map).




getTuple(D, 0, []).
getTuple(D, Count, [X | Rest]) :- 
    del(X, D, D1),
    Count1 is Count - 1,
    getTuple(D1, Count1, Rest).





getGroundTuple([], [], _, []) :-
    !.
getGroundTuple([X | Rest], [_ | RestTuple], Objs, [X | Rest1]) :-
    once(ground(X)),
    getGroundTuple(Rest, RestTuple, Objs, Rest1).
getGroundTuple([X | Rest], [Y | RestTuple], Objs, [X | Rest1]) :-
    once(member(objects(Y, ObjList), Objs)),
%    length(ObjList, Len),
%    random(0, Len, Rand),
%    nth0(Rand, ObjList, X), 
    member(X, ObjList),
    getGroundTuple(Rest, RestTuple, Objs, Rest1).






groundAction(Action, Action) :-
    once(ground(Action)),
    !.
groundAction(Action, GroundAction) :-
    once((Action =.. [P | Vars],
	  testOps(TestOpList),
	  member(op(P, Tuple, _), TestOpList),
	  testDomain(Objs))),
    getGroundTuple(Vars, Tuple, Objs, GroundTuple),
    once((GroundAction =.. [P | GroundTuple])).




get_best_actions([], BestActionsSoFar, BestActions) :- 
    all(Action, member(act(_, Action), BestActionsSoFar), BestActions),
    !.
get_best_actions([act(N, A) | Rest], [act(BestSoFarN, _) | RestBestSoFar], BestActions) :- 
    N > BestSoFarN,
    get_best_actions(Rest, [act(N, A)], BestActions),
    !.
get_best_actions([act(N, A) | Rest], [act(BestSoFarN, BestSoFarA) | RestBestSoFar], BestActions) :- 
    N is BestSoFarN,
    get_best_actions(Rest, [act(N, A), act(BestSoFarN, BestSoFarA) | RestBestSoFar], BestActions),
    !.
get_best_actions([_ | Rest], BestSoFar, BestActions) :- 
    get_best_actions(Rest, BestSoFar, BestActions).






getAction(State, Goal, FinalAction) :-
    
%write('--- here ---\n'),
%write('MyState('),write(State),write(').'),nl,
%write(Goal),nl,
    testOps(TestOps),
%    write('Getting Action'), nl, 
    all(act(Map, Action), (member(op(P, _, class(Action, _, preconds(PreConds), _)), TestOps), once(subsumption_all(PreConds, State, Action, ActLits)), member(Action, ActLits), groundAction(Action, Action1), once(get_ground_action_map1(P, Action1, State, Goal, Map))), AllActionSet),
    write('All Actions: '),write(AllActionSet), nl, 
    (debug(8) -> write('Optimal Actions: '), write(HighestActionSet), nl ; true),
    AllActionSet = [act(FirstN, FirstA) | RestActionSet],
    get_best_actions(RestActionSet, [act(FirstN, FirstA)], HighestActionSet),
    write('Optimal Actions: '), write(HighestActionSet), nl,    
    length(HighestActionSet, Len),
    random(0, Len, Rand),
    !,
    nth0(Rand, HighestActionSet, FinalAction).







    
    
fix_action_diagram([], []) :-
    !.
fix_action_diagram([node(0, _, -1, -1) | Rest], [node(0, 0.0, -1, -1) | Rest1]) :-
    fix_action_diagram(Rest, Rest1),
    !.
fix_action_diagram([node(1, _, -1, -1) | Rest], [node(1, 1.0, -1, -1) | Rest1]) :-
    fix_action_diagram(Rest, Rest1),
    !.
fix_action_diagram([N | Rest], [N | Rest1]) :-
    fix_action_diagram(Rest, Rest1).







get_all_leaf_node_formula([], _, []) :- 
    !.
get_all_leaf_node_formula([leaf(Map, I) | Rest], D, [leafNF(Map, LeafNF) | Other]) :- 
    collectNodeFormula(I, D, LeafNF),
    get_all_leaf_node_formula(Rest, D, Other).






storeActions1([], _, _, []) :- 
    !.
storeActions1([litD(Lit, ActionD1) | Rest], V, VKey, [litD(VarLit, Key, ActionD) | Other]) :-
    fix_action_diagram(ActionD1, ActionD2),
    get_leaves(ActionD2, L1),
    insertsort(L1, L),    
    Lit =.. [LitP | LitVars],
    getDVars(ActionD2, DVars1),
    sort(DVars1, DVars),
    batchInsertRev(LitVars, DVars, LitDVars),
    diff(LitDVars, V, Vars),
    getSubsKey(Vars, VarKey),      
    conc(VarKey, VKey, Key),
    write('Lit: '), write(Lit), nl, 
    variablizeTerm(Lit, Key, VarLit),
    variablizeDD(ActionD2, Key, ActionD3),
    write('V = '), write(V), nl, 
    !,
%    (V = [xa, xb], Lit = ont(a), trace ; true),
    get_all_leaf_node_formula(L, ActionD2, ActionD),
    retractall(nf3579(_, _)),
    write('five'), nl,   
    storeActions1(Rest, V, VKey, Other).


storeActions([]) :- 
      !.
storeActions([actionList(Action, LitD) | Rest]) :-
      Action =.. [_ | V],
      getSubsKey(V, VKey),
      variablizeTerm(Action, VKey, VarAction),
    write('Action:'), write(Action), nl, 
      storeActions1(LitD, V, VKey, ActionList),      
    write('two'), nl, 
      asserta(actionList(VarAction, ActionList)),
      storeActions(Rest).
    


 




getLiteral(_, [], _, []).
getLiteral(P, [T | Tuple], TestDom, [V | Vars]) :-
    member(objects(T, ObjList), TestDom),
    member(V, ObjList),
    getLiteral(P, Tuple, TestDom, Vars).






completeStateDescription(State, TestDom, CompleteState) :-
%    testDomain(TestDom),
    testPreds(TestPreds),
%    all(X, (member(objects(_, Objs1), TestDom), member(X, Objs1)), Objs),
    (all(eq(X,X), (member(objects(_, Objs), TestDom), member(X, Objs)), BasicEq) ; BasicEq = []),
    (all(not_eq(X, Y), (member(objects(_, Objs), TestDom), member(X, Objs), member(Y, Objs), once(X \= Y)), BasicInEq) ; BasicInEq = []),
    (all(NP, (member(pred(P, Tuple), TestPreds), getLiteral(P, Tuple, TestDom, Vars), once(NP =.. [P | Vars])), AllNeg) ; AllNeg = []),
    (all(NP, (member(P, State), once(getNeg(P, NP))), StateNeg) ; StateNeg = []),
    diff(AllNeg, StateNeg, Negs),
    flatten([State, Negs, BasicEq, BasicInEq], CompleteState).





getOutcome([Outcome], _, Outcome) :-
    !.
getOutcome([outcome(X, addList(A), delList(D)) | Rest], N, outcome(N, addList(A), delList(D))) :-
    X >= N,
    !.
getOutcome([outcome(X, addList(A), delList(D)) | Rest], N, Outcome) :-
    N1 is N - X,
    getOutcome(Rest, N1, Outcome).






getOutcome([Outcome], _, Outcome) :-
    !.
getOutcome([outcome(X, addList(A), delList(D)) | Rest], N, outcome(N, addList(A), delList(D))) :-
    X >= N,
    !.
getOutcome([outcome(X, addList(A), delList(D)) | Rest], N, Outcome) :-
    N1 is N - X,
    getOutcome(Rest, N1, Outcome).










takeAction(State, Action, Cost, NextState) :-
    Action =.. [P | Vars],
    testOps(TestOps),
    once(member(op(P, _, class(Action, Cost, preconds(PreConds), effects(Effects))), TestOps)), 
    !, 
    subsumption(PreConds, State, noconv, 1),
    member(cond(Cond, OList), Effects),
    subsumption(Cond, State, noconv, 1),
    random(N),
%    write('random: '), write(N), nl, 
    getOutcome(OList, N, outcome(_, addList(AddList), delList(DelList))),
%    diff(State, DelList, State1),
%    conc(State1, AddList, NextState).
    add_Lits_to_conv_State(AddList, State, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState).




rank_goals([], _, []) :- 
    !.
rank_goals([Goal | Rest], RankValue, [goal(RankValue, Goal) | Other]) :- 
     wgoparam(WGOP),
     RankValue1 is RankValue * WGOP,
%     RankValue1 is RankValue * 1.0,
    rank_goals(Rest, RankValue1, Other).



diff_goal_state([], _, []) :- 
    !.
diff_goal_state([Lit | Rest], State, Other) :- 
    dc_subsumption([Lit], State), 
    !,
    diff_goal_state(Rest, State, Other),
    !.
diff_goal_state([Lit | Rest], State, [Lit | Other]) :- 
    diff_goal_state(Rest, State, Other).






execute(_, PlanLenSoFar, -1.0, Rewd, Rewd, _) :-
    maxplanningsteps3579(MPS),
    PlanLenSoFar > MPS,
    !.
execute(State, PlanLen, PlanLen, Rewd, Rewd, Goal) :-
    subsumption(Goal, State, noconv, 1),  
    (debug(8) -> write('State: '), write(State), nl ; true),
    !.
execute(State, PlanLenSoFar, PlanLen, RewdSoFar, Rewd, Goal) :-
    diff_goal_state(Goal, State, Goal1),
%    write('Diff Goal: '), write(Goal1), nl,
    rank_goals(Goal1, 1.0, Goal2),
    write(Goal2), nl, 
    !,
    getAction(State, Goal2, Action),
    write('Action: '), write(Action), nl,
    (debug(8) -> write('Action: '), write(Action), nl, nl ; true),
    takeAction(State, Action, Cost, NextState),
    RewdSoFar1 is RewdSoFar + Cost,
    PlanLenSoFar1 is PlanLenSoFar + 1,
    !,
    execute(NextState, PlanLenSoFar1, PlanLen, RewdSoFar1, Rewd, Goal).






plan(InitState, Goal, GoalRewd, PlanLen, Rewd) :-
    testDomain(TestDom),
    completeStateDescription(InitState, TestDom, ExPath1),
    convert_Ex(ExPath1, ExPath),
    !,
    order_goals(Goal, OrderedGoal),
    !,
    execute(ExPath, 0, PlanLen, 0.0, Rewd, OrderedGoal).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

takeAllNatureAspectActions(State, [], State) :- 
    !.
takeAllNatureAspectActions(State, [act(Action, Effects) | Rest], NextState) :- 
    member(cond(Cond, OList), Effects),
    subsumption(Cond, State, noconv, 1),
    random(N),
%    write('random: '), write(N), nl, 
    getOutcome(OList, N, outcome(_, addList(AddList), delList(DelList))),
%    diff(State, DelList, State1),
%    conc(State1, AddList, NextState).
    add_Lits_to_conv_State(AddList, State, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState2),
    takeAllNatureAspectActions(NextState2, Rest, NextState).


takeAllNatureActions(State, [], State) :- 
    !.
takeAllNatureActions(State, [op(P, _, class(Action, Cost, preconds(PreConds), effects(Effects))) | Rest], NextState) :- 
    all(act(Action, Effects), subsumption(PreConds, State, noconv, 1), AllActions),
    takeAllNatureAspectActions(State, AllActions, NextState1),
    takeAllNatureActions(NextState1, Rest, NextState). 


takeNatureAction(State, NextState) :- 
    exoOps(ExoOps),
    takeAllNatureActions(State, ExoOps, NextState).
    


takeUserAction(State, Action, NextState) :-
    Action =.. [P | Vars],
    userOps(UserOps),
    once(member(op(P, _, class(Action, Cost, preconds(PreConds), effects(Effects))), UserOps)), 
    !, 
    subsumption(PreConds, State, noconv, 1),
    member(cond(Cond, OList), Effects),
    subsumption(Cond, State, noconv, 1),
    random(N),
%    write('random: '), write(N), nl, 
    getOutcome(OList, N, outcome(_, addList(AddList), delList(DelList))),
%    diff(State, DelList, State1),
%    conc(State1, AddList, NextState).
    add_Lits_to_conv_State(AddList, State, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState).






extract_best_actions([], BestActionsSoFar, BestActions) :- 
    all(Action, member(act(_, Action), BestActionsSoFar), BestActions),
    !.
extract_best_actions([act(N, A) | Rest], [act(BestSoFarN, _) | RestBestSoFar], BestActions) :- 
    N > BestSoFarN,
    extract_best_actions(Rest, [act(N, A)], BestActions),
    !.
extract_best_actions([act(N, A) | Rest], [act(BestSoFarN, BestSoFarA) | RestBestSoFar], BestActions) :- 
    N is BestSoFarN,
    extract_best_actions(Rest, [act(N, A), act(BestSoFarN, BestSoFarA) | RestBestSoFar], BestActions),
    !.
extract_best_actions([_ | Rest], BestSoFar, BestActions) :- 
    extract_best_actions(Rest, BestSoFar, BestActions).




get_best_action(State, AllActionObjs, BestAction) :-
    userOps(UserOps),
    all(act(Map, Action), (member(op(P, _, class(Action, _, preconds(PreConds), _)), UserOps), once((member(actionDVarObjs(P, AvgVar, AvgVarObjs, AllVarObjs), AllActionObjs), subsumption_all(PreConds, State, Action, ActLits))), member(Action, ActLits), once((actionDiag(A, _, [AvgVar:AvgVarVar], Key, D), functor(A, P, _), fodd_eval(D, State, Key, AllVarObjs, AvgVarVar, AvgVarObjs, _, Map)))), AllActionSet),
%    write('All Actions: '),write(AllActionSet), nl, 
    (debug(8) -> write('Optimal Actions: '), write(HighestActionSet), nl ; true),
    AllActionSet = [act(FirstN, FirstA) | RestActionSet],
    extract_best_actions(RestActionSet, [act(FirstN, FirstA)], HighestActionSet),
%    write('Optimal Actions: '), write(HighestActionSet), nl,    
    length(HighestActionSet, Len),
    random(0, Len, Rand),
    !,
    nth0(Rand, HighestActionSet, BestAction).






execute_policy(S, Dom, AllActionObjs, NextState) :- 
    get_best_action(S, AllActionObjs, BestAction),
    takeUserAction(S, BestAction, NextState1),
    takeNatureAction(NextState1, NextState).





rounds(S, Dom, 0, AllActionObjs, Rewd, Rewd) :- 
    !.	  
rounds(S, Dom, H, AllActionObjs, RewdSoFar, Rewd) :- 
    recorded(rewd, rewd(RewdVar, PosRewd, NegRewd), _),
    getNeg(RewdVar, NegRewdVar),
    functor(RewdVar, RewdPred, _),
    functor(NegRewdVar, NegRewdPred, _),
    findall(PosRewd, (once(member([RewdPred | AllRewdLits], S)), member(RewdVar, AllRewdLits)), PosRewdList),
    findall(NegRewd, (once(member([NegRewdPred | AllNegRewdLits], S)), member(NegRewdVar, AllNegRewdLits)), NegRewdList),
    conc(PosRewdList, NegRewdList, AllRewdList),
    sum_list(AllRewdList, TotalStateRewd),
    length(AllRewdList, AllRewdLen),
    RewdSoFar1 is RewdSoFar + (TotalStateRewd/AllRewdLen),
    execute_policy(S, Dom, AllActionObjs, NextState),
    H1 is H - 1,
    rounds(NextState, Dom, H1, AllActionObjs, RewdSoFar1, Rewd).
    



apply_key([], []) :- 
    !.
apply_key([Var:Var | Rest], [Var | Other]) :- 
    apply_key(Rest, Other).




solve_all([]) :-
    !.
solve_all([Problem | Rest]) :-
    testDomain(Problem, Dom),
    initState(Problem, S),
    write(Problem), nl, 
    horizon(Problem, H),
    completeStateDescription(S, Dom, CS),
    convert_Ex(CS, State),
    all(actionDVarObjs(Pred, AvgVar, AvgVarObjs, AllVarObjs), (actionDiag(Action, _, [AvgVar:AvgVarVar], ActVarsKey, D), once((functor(Action, Pred,_), apply_key([AvgVar:AvgVarVar | ActVarsKey], DVars), get_var_type_objs(DVars, D, Dom, VarObjs, VarEqList), VarObjs = [AvgVarObjs | _], get_rem_var_type_objs(VarEqList, VarObjs, D, Dom, AllVarObjs)))), AllActionDVarObjs),
    rounds(State, TD, H, AllActionDVarObjs, 0.0, TotalRewd),
    write('Total Reward: '), write(TotalRewd), nl, nl, 
    solve_all(Rest).




solve :-
    rewardList([reward(Rewd)]),
    Rewd = [node(_, RewdLit, L, R) | _],
    member(node(L, PosRewd, -1, -1), Rewd),
    member(node(R, NegRewd, -1, -1), Rewd),
    RewdLit =.. [RewdPred | RewdArgs],
    getSubsKey(RewdArgs, RewdKey), 
    variablizeTerm(RewdLit, RewdKey, VarRewdLit),
    recorda(rewd, rewd(VarRewdLit,PosRewd,NegRewd), _), 
    problems(P),  
    solve_all(P),
    eraseall(rewd).

