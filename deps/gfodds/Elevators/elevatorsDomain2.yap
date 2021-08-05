
templates([waiting(_x1,_x2), not_waiting(_x3,_x4), pin(_x5), not_pin(_x6), at(_x7), not_at(_x8), eq(_x9, _x10), not_eq(_x11, _x12)]).

arg_types([pred(waiting, [floor,floor]), pred(pin, [floor]), pred(at, [floor])]).

preds([pred(waiting, 2), pred(pin, 1), pred(at, 1), pred(eq, 2)]).

predOrder([eq, waiting, pin, at]).

%%%% USER and EXO OPS have conditional effects. Atmost one condition should be applicable. 


userOps([
op(move, [floor, floor], class(move(F1,F2), 0.0, preconds([at(F1),not_at(F2)]), effects([
cond([waiting(F1,F3),pin(F1)], [outcome(1.0, addList([at(F2),not_at(F1),not_waiting(F1,F3),pin(F3),not_pin(F1)]), delList([at(F1),not_at(F2),waiting(F1,F3),pin(F1),not_pin(F3)]))]),
cond([waiting(F1,F3)], [outcome(1.0, addList([at(F2),not_at(F1),not_waiting(F1,F3),pin(F3)]), delList([at(F1),not_at(F2),waiting(F1,F3),not_pin(F3)]))]),
cond([pin(F1)], [outcome(1.0, addList([at(F2),not_at(F1),not_pin(F1)]), delList([at(F1),not_at(F2),pin(F1)]))]),
cond([], [outcome(1.0, addList([at(F2),not_at(F1)]), delList([at(F1),not_at(F2)]))])
])))
]).


exoOps([
op(arrive, [floor,floor], class(arrive(F1,F2), 0.0, preconds([not_eq(F1,F2)]), effects([
cond([at(F1)], [outcome(0.0, addList([waiting(F1,F2)]),delList([not_waiting(F1,F2)])), outcome(1.0, addList([]),delList([]))]),
cond([not_at(F1)], [outcome(0.0, addList([waiting(F1,F2)]),delList([not_waiting(F1,F2)])), outcome(1.0, addList([]),delList([]))])
])))
%op(get_in, [floor,floor], class(get_in(F3), 0.0, preconds([at(F3),waiting(F3,F4)]), effects([cond([], [outcome(1.0, addList([not_waiting(F3,F4),pin(F4)]), delList([waiting(F3,F4),not_pin(F4)]))])]))),
%op(get_off, [floor,floor], class(get_off(F5), 0.0, preconds([at(F5),pin(F5)]), effects([cond([], [outcome(1.0,addList([not_pin(F5)]),delList([pin(F5)]))])])))
]).


testPreds([pred(not_waiting, [floor,floor]), pred(not_at, [floor]), pred(not_pin, [floor])]).


special_predicates([waiting(b1,b2)]).
rewardList([reward([node(5,waiting(b1,b2),3,4),node(4,pin(b1),1,2),node(3,pin(b1),0,1),node(2,2.0,-1,-1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(waiting, _, 0.99).

ops([move]).

exo_ops([arrive]).




operator(move, [xf1, xf2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(arrive, [b1,b2], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).



most_action_variants(1).
actionDiagParams([xf1, xf2]).
domainConsts([a]).

%%%%%%%%%%%% TVD: ARRIVE SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(arrive, succ, waiting, [b1,b2], [1,2], 2, [node(2,at(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(arrive, succ, pin, [b2], [1,2], 4, [node(4,pin(b2),3,2),node(3,at(b2),0,1),node(2,at(b1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARRIVE FAILURE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(arrive, fail, waiting, [b1,b2], [1,2], 3, [node(3,waiting(b1,b2),2,0),node(2,at(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(arrive, fail, pin, [b2], [1,2], 3, [node(3,pin(b2),2,0),node(2,at(b2),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


%%%%%%%%%%%% TVD: MOVE SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%tvd(move, succ, waiting, [y1,y2], [1,2], 3, [node(3,waiting(y1,y2),2,0),node(2,at(y1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%tvd(move, succ, pin, [y1], [1], 5, [node(5,pin(y1),4,3),node(4,at(y1),0,1),node(3,at(xf1),2,0),node(2,waiting(xf1,y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(move, succ, at, [y1], [1], 6, [node(6,at(y1),5,3),node(5,eq(y1,xf1),4,1),node(4,eq(y1,xf2),1,0),node(3,at(xf1),2,0),node(2,eq(y1,xf2),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: TRIVIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(_,_,waiting,[y1,y2],_,2,[node(2,waiting(y1,y2),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,pin,[y1],_,2,[node(2,pin(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,at,[y1],_,2,[node(2,at(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,eq,[y1,y2],_,2,[node(2,eq(y1,y2),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


:- ensure_loaded('elevators_saket_Bkgd.yap').


