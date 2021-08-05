
%:- use_module(library(random)).


getLiteral(_, [], _, []).
getLiteral(P, [T | Tuple], TestDom, [V | Vars]) :-
    member(objects(T, ObjList), TestDom),
    member(V, ObjList),
    getLiteral(P, Tuple, TestDom, Vars).





completeStateDescription(State, TestDom, CompleteState) :-
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
getOutcome([Outcome | Rest], N, Outcome) :-
    once((Outcome =.. [outcome | [X | _]])),
    X >= N,
    !.
getOutcome([Outcome1 | Rest], N, Outcome) :-
    Outcome1 =.. [outcome | [X | _]],    
    N1 is N - X,
    getOutcome(Rest, N1, Outcome).
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_new_object(Type, Obj) :- 
    recorded(dynamic_type, count_index(Type, Count), Ref),
    erase(Ref),	
    name(Count, CN),
    name(Obj, [112 | CN]),	
    Count1 is Count + 1,
    recorda(dynamic_type, count_index(Type, Count1), _).
    	







add_new_object_to_state_and_domain(Obj, Type, State, NewState) :- 
    testPreds(TestPreds),
    recorded(domain,Dom,RefDom),
    del(objects(Type, TypeObjs),Dom,Dom1),
    NewDom = [objects(Type,[Obj | TypeObjs]) | Dom1],
    (all(not_eq(Obj,X), member(X,TypeObjs), NegEq1) ; NegEq1 = []),
    (all(not_eq(X,Obj), member(X,TypeObjs), NegEq2) ; NegEq2 = []),
    (all(NP, (member(pred(P, Tuple), TestPreds), getLiteral(P, Tuple, NewDom, Vars), once(member(Obj, Vars)), once((NP =.. [P | Vars]))), AllNeg) ; AllNeg = []),
    flatten([NegEq1, NegEq2, AllNeg], LitsToAdd),
    add_Lits_to_conv_State(LitsToAdd, State, NewState),
    erase(RefDom),
    recorda(domain, NewDom, _).




delete_object_from_state_and_domain(Obj, Type, State, NewState) :- 
    recorded(domain,Dom,RefDom),
    del(objects(Type, TypeObjs),Dom,Dom1),
    del(Obj, TypeObjs, TypeObjs1), 
    NewDom = [objects(Type,TypeObjs1) | Dom1],    
    erase(RefDom),
    recorda(domain, NewDom, _),
    (all(Lit, (member(LitList, State), member(Lit, LitList), once((Lit =.. [_ | LitArgs])), once(member(Obj,LitArgs))), LitsToRemove) ; LitsToRemove = []),
    del_Lits_from_conv_State(LitsToRemove, State, NewState).
    


process_outcome(outcome(_, addList(AddList), delList(DelList)), State, NextState) :-
    add_Lits_to_conv_State(AddList, State, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState),
    !.
process_outcome(outcome(_, create(Obj,Type), addList(AddList), delList(DelList)), State, NextState) :-
    get_new_object(Type, Obj),
    add_new_object_to_state_and_domain(Obj, Type, State, NewState),	
    add_Lits_to_conv_State(AddList, NewState, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState),
    !.
process_outcome(outcome(_, destroy(Obj,Type), addList(AddList), delList(DelList)), State, NextState) :-
    add_Lits_to_conv_State(AddList, State, NextState1),
    del_Lits_from_conv_State(DelList, NextState1, NextState2),
    delete_object_from_state_and_domain(Obj, Type, NextState2, NextState).




takeAllNatureAspectActions(State, [], State) :- 
    !.
takeAllNatureAspectActions(State, [act(Action, OList) | Rest], NextState) :- 
%    random(N),
    N is random,
    getOutcome(OList, N, Outcome),
    process_outcome(Outcome, State, NextState1),
    takeAllNatureAspectActions(NextState1, Rest, NextState),
    !.
takeAllNatureAspectActions(State, [_ | Rest], NextState) :- 
    takeAllNatureAspectActions(State, Rest, NextState).





takeAllNatureActions(State, [], State) :- 
    !.
takeAllNatureActions(State, [op(P, _, class(Action, Cost, preconds(PreConds), effects(Effects))) | Rest], NextState) :- 
    (all(act(Action, Outcomes), (member(cond(Cond, Outcomes), Effects), once(conc(PreConds, Cond, PreConds1)), dc_subsumption(PreConds1, State)), AllActions) ; AllActions = []),
    takeAllNatureAspectActions(State, AllActions, NextState1),
    takeAllNatureActions(NextState1, Rest, NextState),
    !.	
takeAllNatureActions(State, [op(P, _, class(Action, Cost, dynamic_obj(DO), preconds(PreConds), effects(Effects))) | Rest], NextState) :-
%	write('creating objs'), nl, 
    create_objects(State,DO,State1),
    (all(act(Action, Outcomes), (member(cond(Cond, Outcomes), Effects), once(conc(PreConds, Cond, PreConds1)), dc_subsumption(PreConds1, State1)), AllActions) ; AllActions = []),    
    (all(update(X,Type), (member(cond(Cond, Outcomes), Effects), once(conc(PreConds, Cond, PreConds1)), dc_subsumption(PreConds1, State1), member(destroy(X,Type),DO), once(ground(X))), AllObjsToDestroy) ; AllObjsToDestroy = []),
    takeAllNatureAspectActions(State1, AllActions, NextState1),
    takeAllNatureActions(NextState1, Rest, NextState2),	
%	write('destroying objs'), nl, 
    destroy_objects(NextState2, AllObjectsToDestroy, NextState).





    


takeNatureAction(State, NextState) :- 
    exoOps(ExoOps),
    takeAllNatureActions(State, ExoOps, NextState).
    


takeUserAction(State, Action, NextState) :-
    Action =.. [P | Vars],
    userOps(UserOps),
    once(member(op(P, _, class(Action, _, preconds(PreConds), effects(Effects))), UserOps)), 
    !, 
    member(cond(Cond, OList), Effects),
    once(conc(PreConds, Cond, PreConds1)),
    subsumption(PreConds1, State, noconv, 1),    
    random(N),    
%    write('random: '), write(N), nl, 
    getOutcome(OList, N, outcome(_, addList(AddList), delList(DelList))),
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




get_best_action(State, BestAction) :-
    userOps(UserOps),
    recorded(domain, Dom, _),
    get_readable_state_conv(State, ReadableState),
    (recorded(best_action, best_action(ReadableState,HighestActionSet), _) -> 
     (true)
     ;
     (all(act(Map, Action), (member(op(P, _, class(Action, _, preconds(PreConds), effects(Effects))), UserOps), member(cond(Cond, _),Effects), once((conc(PreConds, Cond, PreConds1), subsumption_all(PreConds1, State, Action, Actions))), member(Action, Actions), once((actionDiag(Action, D), ((random_policy -> (Map = 0.0)) ; gfodd_evaluation_by_ve(D, State, Dom, Map))))), AllActionSet),
     (debug(map) -> (write('All Actions: '),write(AllActionSet), nl) ; true), 
     AllActionSet = [act(FirstN, FirstA) | RestActionSet],
     extract_best_actions(RestActionSet, [act(FirstN, FirstA)], HighestActionSet),
     recorda(best_action, best_action(ReadableState, HighestActionSet),_))),	
    (debug(map) -> (write('Optimal Actions: '), write(HighestActionSet), nl) ; true),    
    length(HighestActionSet, Len),
    random(0, Len, Rand),
    !,
    nth0(Rand, HighestActionSet, BestAction).






execute_policy(S, NextState) :- 
    get_readable_state_conv(S, ReadableS),
    (debug(map) -> (write('State: '), write(ReadableS), nl) ; true),
    recorded(domain, Dom, _),
    get_best_action(S, BestAction),
    (debug(map) -> (write('Action: '), write(BestAction), nl, nl) ; true), 	
    !,
    takeUserAction(S, BestAction, NextState1),
    !,
    takeNatureAction(NextState1, NextState).





execute_one_round(S, HMax, HMax, Disc, RewdSoFar, Rewd) :- 
    get_readable_state_conv(S, ReadableS),
    (debug(map) -> (write('State: '), write(ReadableS), nl) ; true),	
    recorded(domain, Dom, _),
    rewardList([reward(RewdFn)]),	
    gfodd_evaluation_by_ve(RewdFn, S, Dom, StateRewd),
    Rewd is RewdSoFar + StateRewd,
    (debug(map) -> (write('Reward: '), write(StateRewd), nl, nl) ; true),	
    !.	  
execute_one_round(S, H, HMax, Disc, RewdSoFar, Rewd) :- 
    get_readable_state_conv(S, ReadableS),
    (debug(map) -> (write('State: '), write(ReadableS), nl) ; true),	
    recorded(domain,Dom,_),		
    rewardList([reward(RewdFn)]),
    gfodd_evaluation_by_ve(RewdFn, S, Dom, StateRewd),
    (debug(map) -> (write('Reward: '), write(StateRewd), nl) ; true),	
    RewdSoFar1 is (RewdSoFar + (StateRewd * exp(Disc,H))),
    !, 
    execute_policy(S, NextState),
    H1 is H + 1,
    execute_one_round(NextState, H1, HMax, Disc, RewdSoFar1, Rewd).
    



execute_all_rounds(_, _, 0, _, []) :- 
    !.
execute_all_rounds(Problem, State, R, HMax, [RoundRewd | RestRewd]) :- 
    (debug(mapr) -> (write('Round '), write(R), nl) ; true), 
    testDomain(Problem, Dom),
    recorda(domain, Dom, RefDom),
    discountfactor(_, _, Disc),	
    execute_one_round(State, 0, HMax, Disc, 0.0, RoundRewd),
    eraseall(domain),
    R1 is R - 1,	
    execute_all_rounds(Problem, State, R1, HMax, RestRewd).	




solve_all([]) :- 
    !.
solve_all([Problem | Rest]) :- 
    initState(Problem, S),
%    write(Problem), nl, 
    horizon(Problem, H),
    HAdjusted is H - 1,
    testDomain(Problem, Dom),
    completeStateDescription(S, Dom, CS),
    convert_Ex(CS, State),
    get_readable_state_conv(State, ReadableS),
%    write('State: '), write(ReadableS), nl,	
    !,
    testRounds(R),  
%    statistics(cputime, [T1,_]),
    execute_all_rounds(Problem, State, R, HAdjusted, AllRoundRewds),    
%    statistics(cputime, [T2, _]),
%    T3 is ((T2 - T1)/R)/1000,
%    write(T3),nl,
    get_mean(AllRoundRewds, AvgRewd),
    get_stdev(AllRoundRewds, Stdev),
%    write('Avg Reward Per Round is '), 
    write(AvgRewd), tab(5), 
%    write('Standard Deviation is '), 
    write(Stdev), nl,
    solve_all(Rest).




solve :- 
    (variableElimination -> (write('VARIABLE ELIMINATION ON'), nl, nl) ; (true)),  
	setrand(rand(1,10,100)),
    	srandom(0),
    (debug(map) -> (findall(_, (actionDiag(_,D), once((length(D,L), write('Q Fn Size: '), write(L), nl))), _)) ; (true)),	
    (dynamic_types(DT) ->
	(findall(_, (member(Type,DT), once(recorda(dynamic_type, count_index(Type,1), _))), _))
	;
	(true)),
    problems(P),
    solve_all(P),
    eraseall(dynamic_type).








 