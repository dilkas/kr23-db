
:- dynamic 
    actionList/1.


templates([at(_x1), not_at(_x2), pup(_x3, _x4), not_pup(_x5, _x6), pdown(_x7, _x8), not_pdown(_x9, _x10), above(_x11, _x12), not_above(_x13, _x14), pinup(_x15), not_pinup(_x16), pindown(_x17), not_pindown(_x18), up, not_up, eq(_x19, _x20), not_eq(_x21, _x22)]).

arg_types([pred(at, [floor]), pred(pup, [person, floor]), pred(pdown, [person, floor]), pred(above, [floor,floor]), pred(pinup, [person]), pred(pindown, [person]), pred(up, [])]).


preds([pred(at, 1), pred(pup, 2), pred(pdown, 2), pred(pinup, 1), pred(pindown, 1), pred(above, 2), pred(up, 2), pred(eq, 2)]).
predOrder([eq, at, above, pup, pdown, pinup, pindown, up]).

dynamic_types([person]).

userOps([
op(move, [floor, floor], class(move(A,B), 0.0, preconds([at(A),not_at(B)]), effects([
cond([up,above(B,A)], [outcome(1.0, addList([at(B),not_at(A)]), delList([at(A),not_at(B)]))]), 
cond([not_up,above(A,B)], [outcome(1.0, addList([at(B),not_at(A)]), delList([at(A),not_at(B)]))])
]))),
op(change_dir, [], class(change_dir, 0.0, preconds([]), effects([
cond([up], [outcome(1.0, addList([not_up]), delList([up]))]),
cond([not_up], [outcome(1.0, addList([up]), delList([not_up]))])
])))
]).




exoOps([
op(arr_up, [floor], class(arr_up(F1), 0.0, preconds([not_eq(F1,top)]), effects([cond([],[outcome(0.3, create(P1, person), addList([pup(P1, F1)]), delList([not_pup(P1, F1)])), outcome(0.7, addList([]), delList([]))])]))),

op(arr_down, [floor], class(arr_down(F2), 0.0, preconds([not_eq(F2,bottom)]), effects([cond([],[outcome(0.3, create(P2, person), addList([pdown(P2, F2)]), delList([not_pdown(P2, F2)])), outcome(0.7, addList([]), delList([]))])]))),

op(climb_on, [floor], class(climb_on(F3), 0.0, preconds([at(F3)]), effects([
cond([pup(P3,F3),up], [outcome(1.0, addList([not_pup(P3,F3),pinup(P3)]), delList([pup(P3,F3),not_pinup(P3)]))]), 
cond([pdown(P3,F3),not_up], [outcome(1.0, addList([not_pdown(P3,F3),pindown(P3)]), delList([pdown(P3,F3),not_pindown(P3)]))])
]))),

op(get_off, [], class(get_off, 0.0, preconds([]), effects([
cond([at(top), pinup(P4)], [outcome(1.0, destroy(P4, person), addList([not_pinup(P4)]), delList([pinup(P4)]))]),
cond([at(bottom), pindown(P4)], [outcome(1.0, destroy(P4, person), addList([not_pindown(P4)]), delList([pindown(P4)]))])
])))
]).


testPreds([pred(not_at, [floor]), pred(not_pup, [person, floor]), pred(not_pdown, [person, floor]), pred(not_above, [floor, floor]), pred(not_pinup, [person]), pred(not_pindown, [person]), pred(not_up, [])]).



special_predicates([pup(b1,b2),pdown(b1,b2)]).

%rewardList([reward([node(14,pinup(b1),10,13),node(13,pindown(b1),9,12),node(12,pup(b1,b2),8,11),node(11,pdown(b1,b2),3,2),node(10,pindown(b1),7,9),node(9,pup(b1,b2),6,8),node(8,pdown(b1,b2),5,3),node(7,pup(b1,b2),4,6),node(6,pdown(b1,b2),1,5),node(5,2.0,-1,-1),node(4,pdown(b1,b2),0,1),node(3,3.0,-1,-1),node(2,4.0,-1,-1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

rewardList([reward([node(5,pinup(b1),0,4),node(4,pindown(b1),0,3),node(3,pup(b1,b2),0,2),node(2,pdown(b1,b2),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).



discountfactor(pup, _, 0.99).

ops([move,change_dir]).
exo_ops([arr_up,arr_down]).

operator(move, [xf1, xf2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(change_dir, [], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(arr_up, [b1,b2], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).
exo_operator(arr_down, [b1,b2], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).

most_action_variants(1).

actionDiagParams([xf1, xf2]).

domainConsts([top, bottom, a]).

%%%%%%%%%%%% TVD: ARR_UP SUCCESS %%%%%%%%%%%%%%%

tvd(arr_up, succ, pup, [b1,b2], [1], 4, [node(4,eq(b2,top),0,3),node(3,at(b2),2,1),node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(arr_up, succ, pinup, [b1], [], 5, [node(5,pinup(b1),4,3),node(4,at(top),0,1),node(3,at(b2),2,0),node(2,up,1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_UP FAILURE %%%%%%%%%%%%%%%

tvd(arr_up, fail, pinup(b1), [b1], [], 3, [node(3,pinup(b1),2,0),node(2,at(top),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_DOWN SUCCESS %%%%%%%%%%%%%%%

tvd(arr_down, succ, pdown, [b1,b2], [1], 4, [node(4,eq(b2,bottom),0,3),node(3,at(b2),2,1),node(2,up,1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).

tvd(arr_down, succ, pindown, [b1], [], 5, [node(5,pindown(b1),4,3),node(4,at(bottom),0,1),node(3,at(b2),2,0),node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_DOWN FAILURE %%%%%%%%%%%%%%%

tvd(arr_down, fail, pindown, [b1], [], 3, [node(3,pindown(b1),2,0),node(2,at(bottom),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


%%%%%%%%%%%% TVD: MOVE SUCCESS %%%%%%%%%%%%%%%

tvd(move, succ, at, [y1], [1], 13, [node(13,at(y1),11,12), node(12,eq(y1,xf2),10,0), node(11,eq(y1,xf1),9,1), node(10,at(xf1),8,0), node(9,up,4,5), node(8,up,6,7), node(7,above(xf1,y1),1,0), node(6,above(y1,xf1),1,0), node(5,above(y1,xf2),0,1), node(4,above(xf2,y1),0,1), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: CHANGE_DIR SUCCESS %%%%%%%%%%%%%%%

tvd(change_dir, succ, up, [], [], 2, [node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DEFAULT %%%%%%%%%%%%%%%

tvd(_,_,at,[y1],_,2,[node(2,at(y1),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,above,[y1,y2],_,2,[node(2,above(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pup,[y1,y2],_,2,[node(2,pup(y1,y2),1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pdown,[y1,y2],_,2,[node(2,pdown(y1,y2),1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pinup,[y1],_,2,[node(2,pinup(y1),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pindown,[y1],_,2,[node(2,pindown(y1),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,up,[],_,2,[node(2,up,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,eq,[y1,y2],_,2,[node(2,eq(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).



:- ensure_loaded('elevatorsBkgd3.yap').




constant_test :- 
    D = [node(2,at(bottom),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)],
    Dom = [objects(floor, [top,bottom,f1]), objects(maxobj, [a,a1])],
    I1 = [at(bottom),pinup,above(top,f1),above(f1,bottom),above(top,bottom),up],
    completeStateDescription(I1, Dom, I2),
    convert_Ex(I2, I),
    gfodd_evaluation_by_ve(D, I, Dom, Map),
    write('Map = '), write(Map), nl.
