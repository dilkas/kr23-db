
:- dynamic 
    actionList/1.


templates([at(_x1), not_at(_x2), pup(_x3), not_pup(_x4), pdown(_x5), not_pdown(_x6), above(_x7, _x8), not_above(_x9, _x10), pinup, not_pinup, pindown, not_pindown, up, not_up, eq(_x11, _x12), not_eq(_x13, _x14)]).

arg_types([pred(at, [floor]), pred(pup, [floor]), pred(pdown, [floor]), pred(above, [floor,floor]), pred(pinup, []), pred(pindown, []), pred(up, [])]).


preds([pred(at, 1), pred(pup, 1), pred(pdown, 1), pred(pinup, 0), pred(pindown, 0), pred(above, 2), pred(up, 2), pred(eq, 2)]).
predOrder([eq, at, above, pup, pdown, pinup, pindown, up]).



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
op(arr_up, [floor], class(arr_up(F1), 0.0, preconds([not_pup(F1),not_eq(F1,top)]), effects([cond([],[outcome(0.3, addList([pup(F1)]), delList([not_pup(F1)])), outcome(0.7, addList([]), delList([]))])]))),
op(arr_down, [floor], class(arr_down(F2), 0.0, preconds([not_pdown(F2), not_eq(F2,bottom)]), effects([cond([],[outcome(0.3, addList([pdown(F1)]), delList([not_pdown(F1)])), outcome(0.7, addList([]), delList([]))])]))),
op(climb_on, [floor], class(climb_on(F3), 0.0, preconds([at(F3)]), effects([
cond([pup(F3),up], [outcome(1.0, addList([not_pup(F3),pinup]), delList([pup(F3),not_pinup]))]), 
cond([pdown(F3),not_up], [outcome(1.0, addList([not_pdown(F3),pindown]), delList([pdown(F3),not_pindown]))])
]))),
op(get_off, [], class(get_off, 0.0, preconds([]), effects([
cond([at(top), pinup], [outcome(1.0, addList([not_pinup]), delList([pinup]))]),
cond([at(bottom), pindown], [outcome(1.0, addList([not_pindown]), delList([pindown]))])
])))
]).


testPreds([pred(not_at, [floor]), pred(not_pup, [floor]), pred(not_pdown, [floor]), pred(not_above, [floor, floor]), pred(not_pinup, []), pred(not_pindown, []), pred(not_up, [])]).



special_predicates([pup(b1),pdown(b1)]).

rewardList([reward([node(14,pinup,10,13),node(13,pindown,9,12),node(12,pup(b1),8,11),node(11,pdown(b1),3,2),node(10,pindown,7,9),node(9,pup(b1),6,8),node(8,pdown(b1),5,3),node(7,pup(b1),4,6),node(6,pdown(b1),1,5),node(5,2.0,-1,-1),node(4,pdown(b1),0,1),node(3,3.0,-1,-1),node(2,4.0,-1,-1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).


discountfactor(pup, _, 0.99).

ops([move,change_dir]).
exo_ops([arr_up,arr_down]).

operator(move, [xf1, xf2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(change_dir, [], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(arr_up, [b1], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).
exo_operator(arr_down, [b1], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).

most_action_variants(1).

actionDiagParams([xf1, xf2]).

domainConsts([top, bottom, a]).

%%%%%%%%%%%% TVD: ARR_UP SUCCESS %%%%%%%%%%%%%%%

tvd(arr_up, succ, pup, [b1], [1], 4, [node(4,eq(b1,top),0,3),node(3,at(b1),2,1),node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(arr_up, succ, pinup, [], [], 5, [node(5,pinup,4,3),node(4,at(top),0,1),node(3,at(b1),2,0),node(2,up,1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_UP FAILURE %%%%%%%%%%%%%%%

tvd(arr_up, fail, pinup, [], [], 3, [node(3,pinup,2,0),node(2,at(top),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_DOWN SUCCESS %%%%%%%%%%%%%%%

tvd(arr_down, succ, pdown, [b1], [1], 4, [node(4,eq(b1,bottom),0,3),node(3,at(b1),2,1),node(2,up,1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).

tvd(arr_down, succ, pindown, [], [], 5, [node(5,pindown,4,3),node(4,at(bottom),0,1),node(3,at(b1),2,0),node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: ARR_DOWN FAILURE %%%%%%%%%%%%%%%

tvd(arr_down, fail, pindown, [], [], 3, [node(3,pindown,2,0),node(2,at(bottom),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


%%%%%%%%%%%% TVD: MOVE SUCCESS %%%%%%%%%%%%%%%

tvd(move, succ, at, [y1], [1], 13, [node(13,at(y1),11,12), node(12,eq(y1,xf2),10,0), node(11,eq(y1,xf1),9,1), node(10,at(xf1),8,0), node(9,up,4,5), node(8,up,6,7), node(7,above(xf1,y1),1,0), node(6,above(y1,xf1),1,0), node(5,above(y1,xf2),0,1), node(4,above(xf2,y1),0,1), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: CHANGE_DIR SUCCESS %%%%%%%%%%%%%%%

tvd(change_dir, succ, up, [], [], 2, [node(2,up,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DEFAULT %%%%%%%%%%%%%%%

tvd(_,_,at,[y1],_,2,[node(2,at(y1),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,above,[y1,y2],_,2,[node(2,above(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pup,[y1],_,2,[node(2,pup(y1),1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pdown,[y1],_,2,[node(2,pdown(y1),1,0),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pinup,[],_,2,[node(2,pinup,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pindown,[],_,2,[node(2,pindown,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,up,[],_,2,[node(2,up,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,eq,[y1,y2],_,2,[node(2,eq(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).



:- ensure_loaded('elevatorsBkgd1.yap').




constant_test :- 
    D = [node(2,at(bottom),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)],
    Dom = [objects(floor, [top,bottom,f1]), objects(maxobj, [a,a1])],
    I1 = [at(bottom),pinup,above(top,f1),above(f1,bottom),above(top,bottom),up],
    completeStateDescription(I1, Dom, I2),
    convert_Ex(I2, I),
    gfodd_evaluation_by_ve(D, I, Dom, Map),
    write('Map = '), write(Map), nl.
