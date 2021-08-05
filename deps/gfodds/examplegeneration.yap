:- dynamic
    ns3579/1,
    interpretations3579/2,
    currstates3579/1,
    firststate3579/2,
    alldomactions3579/1.
    


:- use_module(library(timeout)).


%get_readable_state(State, ReadableState) :-
%    all(A, (member(A, [at(_), connected(_,_), doorconnected(_,_,_), red(_), blue(_), in(_,_), reported(_), searched(_), zone(_), po(_,_), delivered(_), dtarget(_)]), member(A, State)), ReadableState).




actionLeadsToState(Action, State, Special, Marked, NoRetState, TestOps, NewSpecial, State3, NewMarked) :-
%    write(Action), write('    '),
%    (Action = deliver(b,bx1,l1), trace ; true),
    once((functor(Action, Name, _), 
	  member(op(Name, _, class(Action, _, preconds(PreConds1), effects(Effects))), TestOps))), 
    member(cond(Cond, OList), Effects),
    member(outcome(_, addList(AddList), delList(DelList)), OList),
    once(not((AddList = [], DelList = []))),
    once((conc(PreConds1, Cond, PreConds),
	  conc(PreConds, AddList, PrAddList), 
	  diff(PrAddList, DelList, SuperState))),     
    once(subsumes2(SuperState, State)), 
    once((brw_ir_sp_lits,
	  !,
	  my_intersection(SuperState, Special, Int1),
	  Int1 \== []
	  ;
	  true)),
    once((NewSpecial = PreConds)),
    once(my_intersection(AddList, StatePreConds, NewMarked1)),
    once(conc(Marked, NewMarked1, NewMarked)),

    once((batchInsert(DelList, State, State2), 
	  diff(State2, AddList, State3))),
    once((brw_ir_marked,
	  !,
	  my_intersection(State3, Marked, State3Marked),
	  State3Marked == []
	  ;
	  true)),
    once(diff(State3, NoRetState, NewStuff)),
    once(NewStuff \== []),
    once((getBasicBkgd(State3, [], Objs, ExPath1),
	  floodState(ExPath1, Objs, ExPath2))),
    once(pathSatisfiable(ExPath2, ExPath2)).

    



%%%%%%%%%%%%%%%%% BACKWARD RANDOM WALK %%%%%%%%%%%%%%%%%%%%%%%

random_walk_rev(_, _, _, _, 0, _, _) :-
    !.
random_walk_rev(State, PreConds, Marked, NoRetState, Count, TestOps, AllPossActions) :-
%    write('Step # '), write(Count), nl,
    get_readable_state(State, State1),
    sort(State1, State2),
%    write('State: '), write(State1), nl, write('PreConds: '), write(PreConds), nl, write('Marked: '), write(Marked), nl, nl,
    (ns3579(State2) ; asserta(ns3579(State2))),
    !,
    all([ActPreConds, PrevState, NewMarked], (member(Action, AllPossActions), actionLeadsToState(Action, State, PreConds, Marked, NoRetState, TestOps, ActPreConds, PrevState, NewMarked)), PossNextStates),
    length(PossNextStates, Len),
%    write('Len = '), write(Len), nl, 
    random(0, Len, X),
%    write('random # is '), write(X), nl,
    !,
    nth0(X, PossNextStates, [NextPreConds, NextState, NewMarked]),
    Count1 is Count - 1,
    random_walk_rev(NextState, NextPreConds, NewMarked, State, Count1, TestOps, AllPossActions).
    
    
    
explore_regress(_, _, _, _, _, _, 0) :-
    !.
explore_regress(State, PreConds, Marked, MaxSteps, TestOps, AllPossActions, Count) :-
%    write('Exploration # '), write(Count), nl, 
    !,
    (random_walk_rev(State, PreConds, Marked, [], MaxSteps, TestOps, AllPossActions) ; true),
    Count1 is Count - 1,
    !,
    explore_regress(State, PreConds, Marked, MaxSteps, TestOps, AllPossActions, Count1).



generate_IList_for_problems([], _, _, _) :- 
    !.
generate_IList_for_problems([rg(Goal, PreConds, Marked) | RewGoals], Dom, Rounds, MaxSteps) :-
    testOps(TestOps),
    completeStateDescription(Goal, Dom, State),
    (all(ActLit, (member(op(P, Tuple, _), TestOps), getLiteral(P, Tuple, Dom, LitVars), once(ActLit =.. [P | LitVars])), ActLits) ; ActLits = []),
    !,
    explore_regress(State, PreConds, Marked, MaxSteps, TestOps, ActLits, Rounds),
    generate_IList_for_problems(RewGoals, Dom, Rounds, MaxSteps).
    


brw_for_rewards([], _, _) :- 
    !.
brw_for_rewards([Rewd | Rest], Rounds, MaxSteps) :-    
    asserta(Rewd),
    getRewdConjVar([RewdConj], _),
    functor(RewdConj, RewdPred, _),    
    trainingdomain3579(RewdPred, Dom),    
    firststate3579(RewdPred, FS), 
    write('%generating IList'), nl, 
    generate_IList_for_problems(FS, Dom, Rounds, MaxSteps),
    write('interpretationObjs3579('), write(RewdPred), write(', '), write(Dom), write(').'), nl, nl, 
    write('interpretations3579('), write(RewdPred), write(', '), 
    all(NS, ns3579(NS), AllNS),
    write(AllNS), write(').'), nl, nl, nl,
    retract(Rewd), 
    retractall(ns3579(_)),
    brw_for_rewards(Rest, Rounds, MaxSteps).
    
    
    
brw(Rounds, MaxSteps, IFILE) :- 
    setrand(rand(1098,133,26)),    
    tell(IFILE),
    write('%% Examples for model checking reduciton by backward random walk'), nl, nl, nl,
    told,
    rewardList(RewdList),
    open(IFILE, append, S),
    tell(S),
    brw_for_rewards(RewdList, Rounds, MaxSteps),
    close(S).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INSTANCE REGRESSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



generate_next_level_examples(Dom) :- 
    once((interpretations3579(AllStates),
    retract(currstates3579(States)),
    alldomactions3579(AllActions),
    testOps(TestOps))),
    all(rg(_,PrevState, ActPreConds, NewMarked), (member(rg(_,State,PreConds,Marked), States), member(Action, AllActions), actionLeadsToState(Action, State, PreConds, Marked, [], TestOps, ActPreConds, PrevState1, NewMarked), once(completeStateDescription(PrevState1, Dom, PrevState2)), once(sort(PrevState2, PrevState)), once(not(member(rg(_,PrevState, _, _), States))), once(not(member(PrevState, AllStates)))), AllNextStates),
    all(Ex, member(rg(_,Ex, _, _), AllNextStates), AllEx),
    conc(AllStates, AllEx, NewAllStates),
    retractall(interpretations3579(_)),
    asserta(interpretations3579(NewAllStates)),
    asserta(currstates3579(AllNextStates)).
    



generate_examples_for_rounds(0, _) :-
    !.
generate_examples_for_rounds(X, TrDom) :- 
%    write('Round: '), write(X), nl, 
    generate_next_level_examples(TrDom),
%    print_examples,
    Y is X - 1,
    !,
    generate_examples_for_rounds(Y, TrDom).



    
initialize_example_generation(RewdPred, Dom) :- 
    write('interpretationObjs3579('), write(RewdPred), write(', '), write(Dom), write(').'), nl, nl, write('interpretations3579('), write(RewdPred), write(', ['), 
    testOps(TestOps),
    (all(ActLit, (member(op(P, Tuple, _), TestOps), getLiteral(P, Tuple, Dom, LitVars), once(ActLit =.. [P | LitVars])), ActLits) ; ActLits = []),
    asserta(alldomactions3579(ActLits)),
    retract(firststate3579(RewdPred, FS)),
    all(rg(_,CompFirstState, PreConds, Marked), (member(rg(FirstState, PreConds, Marked), FS), once(completeStateDescription(FirstState, Dom, CompFirstState))), CompleteFS),
    all(CompFirstState, member(rg(_, CompFirstState, _, _), CompleteFS), CompleteFS1),
    asserta(interpretations3579(CompleteFS1)),
    asserta(currstates3579(CompleteFS)).
    
    



ir_for_reward([], _) :- 
    !.
ir_for_reward([Rewd | Rest], X) :-
    asserta(Rewd),
    getRewdConjVar([RewdConj], _),
    functor(RewdConj, RewdPred, _),    
    trainingdomain3579(RewdPred, Dom),    
    initialize_example_generation(RewdPred, Dom),
    (generate_examples_for_rounds(X, Dom) ; true),
    interpretations3579([First | Other]),
    get_readable_state(First, ReadableFirst),
    write(ReadableFirst), 
    all(_, (member(Y,Other), once(get_readable_state(Y,Z)), once((write(', '), write(Z)))), _),
    write(']).'), nl, nl, nl,
    retract(Rewd),
    ir_for_reward(Rest, X).





ir(X, IFILE) :-
    tell(IFILE), 
    write('%% Examples for modelchecking reductions'), nl, nl, nl, 
    told,
    rewardList(Rewds),
    open(IFILE, append, S),
    tell(S),
    ir_for_reward(Rewds, X),
    close(S).




print_examples :-
    write('Examples:'), nl, nl,
    interpretations3579(X),
    all(_, (member(Y,X), get_readable_state(Y,Z), write(Z), nl), _).



%%%%%%%%%%%% FORWARD RANDOM WALK %%%%%%%%%%%%%%%%%%%%%



frwTakeAllNatureActions(State, [], State) :- 
    !.
frwTakeAllNatureActions(State, [[AddList, DelList] | Rest], NextState) :-
    diff(State, DelList, State1),
    conc(State1, AddList, State2),
    frwTakeAllNatureActions(State2, Rest, NextState).




frwTakeNatureAction(State, [ExoOp | Rest], NextState) :- 
    ExoOp = op(_, _, class(_,_,preconds(PreConds),effects(Effects))), 
    all([AddList,DelList], (subsumes2(PreConds, State),member(cond(Cond,OList),Effects),subsumes2(Cond,State),once((length(OList, OLen),randseq(OLen, OLen, Oseq),member(N, Oseq),nth(N, OList, outcome(_, addList(AddList), delList(DelList))),not((AddList == [])),not((DelList == []))))), AddDelLists),
    frwTakeAllNatureActions(State, AddDelLists, NextState).





frwTakeAction(State, Action, Cost, NextState) :-
    Action =.. [P | Vars],
    userOps(TestOps),
    member(op(P, _, class(Action, Cost, preconds(PreConds), effects(Effects))), TestOps), 
    subsumption(PreConds, State, noconv, 1),
    !,
    member(cond(Cond, OList), Effects),
    subsumption(Cond, State, noconv, 1),
    random(N),
    getOutcome(OList, N, outcome(_, addList(AddList), delList(DelList))),
    once((diff(State, DelList, State1),
    	  conc(State1, AddList, NextState1))),
    exoOps(ExoOps),
    frwTakeNatureAction(NextState1, ExoOps,NextState).
	


perform_forward_random_walk(State, 0) :-
    !.
perform_forward_random_walk(State, WalkLength) :-
    userOps(TestOps),
    all(Action, (member(op(P, _, class(Action, _, preconds(PreConds), _)), TestOps), subsumption(PreConds, State, noconv, 1)), AllActionSet),    
    length(AllActionSet, Len),
    random(0, Len, Rand),
    nth0(Rand, AllActionSet, ActionToTake),
    takeUserAction(State, ActionToTake, NextState1),
    takeNatureAction(NextState1, NextState),
%    frwTakeAction(State, ActionToTake, 0.0, NextState),
    get_readable_state_conv(NextState, ReadableNextState),
    sort(ReadableNextState, SortedNextState),
    (recordaifnot(ns,SortedNextState,_), write(','),write(NextState) ; true),  
    WalkLength1 is WalkLength - 1,
    perform_forward_random_walk(NextState, WalkLength1).
    



forward_random_walk(_, 0, _) :-
    !.
forward_random_walk(State, NumEpisodes, EpisodeLength) :-
%    write(NumEpisodes), write(' episodes to go'), nl, 
    get_readable_state_conv(State, State1),
    sort(State1, SortedState),	
    ((recordaifnot(ns,SortedState,_), write(State)) ; true),
    !,
    (perform_forward_random_walk(State, EpisodeLength) ; true),
    NumEpisodes1 is NumEpisodes - 1,
    !,
    forward_random_walk(State, NumEpisodes1, EpisodeLength).
    
    
    
generate_frw_for_problems([], _, _, _) :- 
    !.
generate_frw_for_problems([rg(State) | Rest], Dom, Rounds, MaxSteps) :- 
    completeStateDescription(State, Dom, CompState), 
    convert_Ex(CompState, ConvCompState),
    forward_random_walk(ConvCompState, Rounds, MaxSteps),
    generate_frw_for_problems(Rest, Dom, Rounds, MaxSteps).




frw_for_rewards([], _, _) :- 
    !.     
frw_for_rewards([Rewd | Rest], Rounds, MaxSteps) :- 
    Rewd = reward([node(_,RewdConj, _, _) | _]),		      
    functor(RewdConj, RewdPred, _),    
    trainingdomain3579(RewdPred, Dom),    
    startstate3579(RewdPred, FS), 
    !,
    write('interpretationObjs3579('), write(RewdPred), write(', '), write(Dom), write(').'), nl, nl, 
    write('interpretations3579('), write(RewdPred), write(', ['), 
    generate_frw_for_problems(FS, Dom, Rounds, MaxSteps),
%    all(NS, recorded(ns,NS,_), AllNS),
%    write(AllNS), write(').'), nl, nl, nl,
    write(']).'),nl, nl, nl,
    frw_for_rewards(Rest, Rounds, MaxSteps).



frw(NumEpisodes, EpisodeLength, IFILE) :-
    setrand(rand(23,1098,477)),
    tell(IFILE),
    write('%% Examples for model checking reduciton by forward random walk'), nl, nl, nl,
    told,
    rewardList(RewdList),
    open(IFILE, append, S),
    tell(S), 
    frw_for_rewards(RewdList, NumEpisodes, EpisodeLength),
    close(S).






%%%%%%%%%%%% FORWARD WAVE + BACKWARD TRACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recordTraj([]) :- 
    !.
recordTraj([State | Rest]) :- 
    (recordaifnot(ns, State, _) ; true),
    recordTraj(Rest).

fw_exhaustive_dfs(_, 0, _) :- 
    !.
fw_exhaustive_dfs(State, _, Trajectory) :- 
    getRewdConjVar(RewdConj, _),
    subsumes2(RewdConj, State), 
    recordTraj([State | Trajectory]),
    !.
fw_exhaustive_dfs(State, Depth, Trajectory) :- 	 
    once((write('At Depth: '), write(Depth), nl)),			 
    once((Trajectory = [F | _], write('State: '), write(F), nl)),			 	      
    
    once(testOps(TestOps)),
    member(op(P, _, class(Action, _, preconds(PreConds), _)), TestOps), 
    subsumes2(PreConds, State), 
    frwTakeAction(State, Action, 0.0, NextState), 
    once((get_readable_state(NextState, NextState1), 
    	  sort(NextState1, SortedNextState))),
    once(not(member(SortedNextState, Trajectory))),
    once((Depth1 is Depth - 1)),
    fw_exhaustive_dfs(NextState, Depth1, [SortedNextState | Trajectory]). 
    

fw(State, Depth) :- 
    get_readable_state(State, State1), 
    sort(State1, SortedState),	  
    (fw_exhaustive_dfs(State, Depth, [SortedState]), fail ; true).


	    

%%%%%%%%%%%% MIXED IR and FRW %%%%%%%%%%%%%%%%%%%%%


getex_for_rewards([Rewd | Rest], IR, NumEpisodes, EpisodeLength) :- 
    asserta(Rewd),
    getRewdConjVar([RewdConj], _),
    functor(RewdConj, RewdPred, _),    
    trainingdomain3579(RewdPred, Dom),    
    initialize_example_generation(RewdPred, Dom),
    (generate_examples_for_rounds(IR, Dom) ; true).
    



getex(IR, NumEpisodes, EpisodeLength, IFILE) :- 
    setrand(rand(23,1098,477)),
    tell(IFILE),
    write('%% Examples for model checking reduciton by IR and forward random walk'), nl, nl, nl,
    told,
    rewardList(RewdList),
    open(IFILE, append, S),
    tell(S),
    getex_for_rewards(RewdList, IR, NumEpisodes, EpisodeLength),
    close(S).
    




