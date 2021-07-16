
get_unbound_Vars([], []) :- 
    !.
get_unbound_Vars([X | Rest], [X | Other]) :- 
    not(ground(X)),
    get_unbound_Vars(Rest, Other),
    !.
get_unbound_Vars([_ | Rest], Other) :- 
    get_unbound_Vars(Rest, Other).



get_relative_goal_order(Goal1, Goal2, b) :- 
    once(testOps(TestOps)), 
    member(op(_, _, class(Action, _, preconds(PreConds1), effects(Effects))), TestOps),
    member(cond(Cond, OutComeList), Effects),
    member(outcome(_, addList(AddList), _), OutComeList),
    member(Goal1, AddList),
    once((conc(PreConds1, Cond, PreConds),
	  Action =.. [_ | ActionVars],
	  get_unbound_Vars(ActionVars, VarVars),
	  length(VarVars, Len),
	  getNewVars(1, Len, NewVars),
	  VarVars = NewVars, 
	  Goal2 =.. [_ | Vars2],
	  getBasicBkgd(PreConds, Vars2, Objs, ExPath1),
	  floodState(ExPath1,Objs, ExPath),
	  getNeg(Goal2, NegGoal2))),
    member(NegGoal2, ExPath),
    !.
get_relative_goal_order(Goal1, Goal2, a) :- 
    once(testOps(TestOps)), 
    member(op(_, _, class(Action, _, preconds(PreConds1), effects(Effects))), TestOps),
    member(cond(Cond, OutComeList), Effects),
    member(outcome(_, addList(AddList), _), OutComeList),
    member(Goal2, AddList),
    once((conc(PreConds1, Cond, PreConds),
	 Action =.. [_ | ActionVars],
	  get_unbound_Vars(ActionVars, VarVars),
	 length(VarVars, Len),
	 getNewVars(1, Len, NewVars),
	 VarVars = NewVars, 
	  Goal1 =.. [_ | Vars1],
	 getBasicBkgd(PreConds, Vars1, Objs, ExPath1),
	 floodState(ExPath1,Objs, ExPath),
	 getNeg(Goal1, NegGoal2))),
    member(NegGoal2, ExPath),
    !.
get_relative_goal_order(_, _, e).

    
    
    





get_relative_goal_order_list(_, [], []) :- 
    !.
get_relative_goal_order_list(Goal, [Goal1 | Rest], [ord(G1, G2) | Other]) :- 
    once(get_relative_goal_order(Goal, Goal1, Ord)),
    Ord \== e,
    !,
    (Ord == b,
     G1 = Goal, 
     G2 = Goal1
     ;
     G1 = Goal1,
     G2 = Goal),
    !,
    get_relative_goal_order_list(Goal, Rest, Other),
    !.
get_relative_goal_order_list(Goal, [Goal1 | Rest], RelGoalOrder) :- 
    get_relative_goal_order_list(Goal, Rest, RelGoalOrder).




get_relative_goal_order_info([], InfoGoalOrd, InfoGoalOrd) :- 
    !.
get_relative_goal_order_info([Goal | Rest], InfoListSoFar, InfoGoalOrder) :- 
    get_relative_goal_order_list(Goal, Rest, InfoList),
    conc(InfoList, InfoListSoFar, InfoListSoFar1),
    !,
    get_relative_goal_order_info(Rest, InfoListSoFar1, InfoGoalOrder).
    
    




flood_goal_ordering_info(RelOrd, ExpRelOrd) :- 
    all(ord(G1,G3), (member(ord(G1,G2),RelOrd), member(ord(G2,G3),RelOrd), once(not(member(ord(G1,G3),RelOrd)))), ExpRelOrd1),
    conc(RelOrd, ExpRelOrd1, ExpRelOrd2),
    flood_goal_ordering_info(ExpRelOrd2, ExpRelOrd),
    !.
flood_goal_ordering_info(RelOrd, RelOrd).





recover_goal_order(RelOrd, [G | Rest]) :- 
    member(ord(G,_),RelOrd),
    not(member(ord(_,G), RelOrd)),
    all(ord(G,X), member(ord(G,X), RelOrd), GRelOrdList),
    diff(RelOrd, GRelOrdList, RelOrd1),
    recover_goal_order(RelOrd1, Rest),
    !.
recover_goal_order(_, []).




    
    


order_goals(Goals, OrderedGoals) :- 
    get_relative_goal_order_info(Goals, [], InfoGoalOrdering),
%    write('Ord'), write(InfoGoalOrdering), nl,  
    flood_goal_ordering_info(InfoGoalOrdering, RelOrd),
%    write('FloodedGoalOrd: '), write(RelOrd), nl, 
    recover_goal_order(RelOrd, OrderedGoals1),
%    write('Partial Order: '), write(OrderedGoals1), nl,
    diff(Goals, OrderedGoals1, RemGoals),
    conc(OrderedGoals1, RemGoals, OrderedGoals).

 




