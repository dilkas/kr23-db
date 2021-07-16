
:- dynamic 
    actionList/1.


templates([at(_x1), not_at(_x2), pup(_x3), not_pup(_x4), pdown(_x5), not_pdown(_x6), above(_x7, _x8), not_above(_x9, _x10), pinup, not_pinup, pindown, not_pindown, open, not_open, up, not_up, eq(_x11, _x12), not_eq(_x13, _x14)]).

arg_types([pred(at, [floor]), pred(pup, [floor]), pred(pdown, [floor]), pred(above, [floor,floor]), pred(pinup, []), pred(pindown, []), pred(open, []), pred(up, [])]).


preds([pred(at, 1), pred(pup, 1), pred(pdown, 1), pred(pinup, 0), pred(pindown, 0), pred(above, 2), pred(open, 0), pred(up, 2), pred(eq, 2)]).
predOrder([eq, at, above, pup, pdown, pinup, pindown, open, up]).



userOps([
op(move, [floor, floor], class(move(A,B), 0.0, preconds([at(A), not_open]), effects([
cond([up,above(B,A)], [outcome(1.0, addList([at(B),not_at(A)]), delList([at(A),not_at(B)]))]), 
cond([not_up,above(A,B)], [outcome(1.0, addList([at(B),not_at(A)]), delList([at(A),not_at(B)]))])
]))), 
op(open_up, [floor], class(open_up(C), 0.0, preconds([at(C),not_open]), effects([cond([], [outcome(1.0, addList([up, open]), delList([not_up, not_open]))])]))), 
op(open_down, [floor], class(open_down(D), 0.0, preconds([at(D),not_open]), effects([cond([], [outcome(1.0, addList([not_up, open]), delList([up, not_open]))])]))), 
op(close, [], class(close, 0.0, preconds([open]), effects([cond([], [outcome(1.0, addList([not_open]), delList([open]))])])))
]).




exoOps([
op(arr_up, [floor], class(arr_up(F1), 0.0, preconds([not_pup(F1),not_eq(F1,top)]), effects([cond([],[outcome(0.3, addList([pup(F1)]), delList([not_pup(F1)])), outcome(0.7, addList([]), delList([]))])]))),
op(arr_down, [floor], class(arr_down(F2), 0.0, preconds([not_pdown(F2), not_eq(F2,bottom)]), effects([cond([],[outcome(0.3, addList([pdown(F1)]), delList([not_pdown(F1)])), outcome(0.7, addList([]), delList([]))])]))),
op(climb_on, [floor], class(climb_on(F3), 0.0, preconds([at(F3),open]), effects([
cond([pup(F3),up], [outcome(1.0, addList([not_pup(F3),pinup]), delList([pup(F3),not_pinup]))]), 
cond([pdown(F3),not_up], [outcome(1.0, addList([not_pdown(F3),pindown]), delList([pdown(F3),not_pindown]))])
]))),
op(get_off, [], class(get_off, 0.0, preconds([]), effects([
cond([at(top), pinup], [outcome(1.0, addList([not_pinup]), delList([pinup]))]),
cond([at(bottom), pindown], [outcome(1.0, addList([not_pindown]), delList([pindown]))])
])))
]).


testPreds([pred(not_at, [floor]), pred(not_pup, [floor]), pred(not_pdown, [floor]), pred(not_above, [floor, floor]), pred(not_pinup, []), pred(not_pindown, []), pred(not_open, []), pred(not_up, [])]).



special_predicates([pup(b),pdown(b)]).
rewardList([reward([node(14,pup(b),12,13),node(13,pdown(b),10,11),node(12,pdown(b),9,10),node(11,pinup,6,5),node(10,pinup,7,6),node(9,pinup,8,7),node(8,pindown,4,3),node(7,pindown,3,2),node(6,pindown,2,1),node(5,pindown,1,0),node(4,4.0,-1,-1),node(3,3.0,-1,-1),node(2,2.0,-1,-1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).



discountfactor(pup, _, 0.99).

ops([move,open_up,open_down,close]).
exo_ops([arr_up,arr_down]).

operator(move, [xf1, xf2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(open_up, [xf1], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(open_down, [xf1], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(close, [], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(arr_up, [b], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).
exo_operator(arr_down, [b], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).

most_action_variants(1).

actionDiagParams([xf1, xf2]).

domainConsts([top, bottom, a]).

%%%%%%%%%%%% TVD: ARR_UP SUCCESS %%%%%%%%%%%%%%%

tvd(arr_up, succ, pup, [y1], [1], 2, [node(2, eq(b,top), 0, 1), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_up, _, pdown, [y1], [1], 2, [node(2, pdown(y1), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_up, _, pinup, [], [], 2, [node(2, pinup, 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_up, _, pindown, [], [], 2, [node(2, pindown, 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: ARR_DOWN SUCCESS %%%%%%%%%%%%%%%

tvd(arr_down, succ, pdown, [y1], [1], 2, [node(2, eq(b,bottom), 0, 1), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_down, _, pup, [y1], [1], 2, [node(2, pup(y1), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_down, _, pinup, [], [], 2, [node(2, pinup, 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(arr_down, _, pindown, [], [], 2, [node(2, pindown, 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: MOVE SUCCESS %%%%%%%%%%%%%%%

tvd(move, succ, at, [y1], [1], 13, [node(13,at(y1),11,12), node(12,eq(y1,xf2),10,0), node(11,eq(y1,xf1),9,1), node(10,at(xf1),8,0), node(9,up,4,5), node(8,up,6,7), node(7,above(xf1,y1),3,0), node(6,above(y1,xf1),3,0), node(5,above(y1,xf2),2,1), node(4,above(xf2,y1),2,1), node(3,open,0,1), node(2,open,1,0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: OPEN_UP SUCCESS %%%%%%%%%%%%%%%

tvd(open_up,succ,pinup,[],[],6,[node(6,pinup,2,5),node(5,at(xf1),4,0),node(4,pup(xf1),3,0),node(3,open,0,1),node(2,at(top),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(open_up,succ,open,[],[],3,[node(3,open,1,2),node(2,at(xf1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(open_up,succ,up,[],[],4,[node(4,up,1,3),node(3,at(xf1),2,0),node(2,open,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: OPEN_DOWN SUCCESS %%%%%%%%%%%%%%%

tvd(open_down,succ,pindown,[],[],6,[node(6,pindown,2,5),node(5,at(xf1),4,0),node(4,pdown(xf1),3,0),node(3,open,0,1),node(2,at(bottom),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(open_down,succ,open,[],[],3,[node(3,open,1,2),node(2,at(xf1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(open_down,succ,up,[],[],4,[node(4,up,1,3),node(3,at(xf1),2,0),node(2,open,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: CLOSE SUCCESS %%%%%%%%%%%%%%%

tvd(close,succ,open,[],[],0,[node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DEFAULT %%%%%%%%%%%%%%%

tvd(_,_,at,[y1],_,2,[node(2,at(y1),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,above,[y1,y2],_,2,[node(2,above(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pup,[y1],_,5,[node(5,pup(y1),4,0),node(4,at(y1),3,1),node(3,up,2,1),node(2,open,0,1),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pdown,[y1],_,5,[node(5,pdown(y1),4,0),node(4,at(y1),3,1),node(3,up,1,2),node(2,open,0,1),node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).

tvd(_,_,pinup,[],_,3,[node(3,at(top),0,2), node(2,pinup,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,pindown,[],_,3,[node(3,at(bottom),0,2), node(2,pindown,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,open,[],_,2,[node(2,open,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,up,[],_,2,[node(2,up,1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).
tvd(_,_,eq,[y1,y2],_,2,[node(2,eq(y1,y2),1,0), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).



:- ensure_loaded('elevatorsBkgd.yap').
