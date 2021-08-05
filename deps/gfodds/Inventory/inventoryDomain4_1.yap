
:- dynamic     
   actionList/1.


templates([empty(_x1), not_empty(_x2), tfull(_x3), not_tfull(_x4), tin(_x5, _x6), not_tin(_x7, _x8), shop(_x9), not_shop(_x10), depot(_x11), not_depot(_x12), eq(_x45, _x46), not_eq(_x47, _x48)]).

arg_types([pred(empty, [location]), pred(tfull, [truck]), pred(tin, [truck, location]), pred(shop, [location]), pred(depot, [location])]).

preds([pred(empty, 1), pred(tfull, 1), pred(tin, 2), pred(shop, 1), pred(depot, 1), pred(eq, 2)]).

predOrder([tin, tfull, shop, depot, empty, eq]).
userOps([
op(load, [truck, location], class(load(T1,L1), 0.0, preconds([not_tfull(T1), tin(T1,L1), depot(L1)]), effects([cond([], [outcome(1.0, addList([tfull(T1)]), delList([not_tfull(T1)]))])]))), 
op(unload, [truck], class(unload(T2), 0.0, preconds([tin(T2,L2), tfull(T2), shop(L2), empty(L2)]), effects([cond([], [outcome(1.0, addList([not_empty(L2), not_tfull(T2)]), delList([tfull(T2), empty(L2)]))])]))),
op(drive, [truck, location], class(drive(T3, L4), 0.0, preconds([tin(T3,L3), not_tin(T3,L4)]), effects([cond([], [outcome(1.0, addList([tin(T3,L4), not_tin(T3,L3)]), delList([tin(T3, L3), not_tin(T3, L4)]))])]))),
op(noop, [], class(noop, 0.0, preconds([]), effects([cond([], [outcome(1.0, addList([]), delList([]))])])))
]).

exoOps([
op(consume, [location], class(consume(L5), 0.0, preconds([shop(L5), not_empty(L5)]), effects([cond([], [outcome(0.3, addList([empty(L5)]), delList([not_empty(L5)])), outcome(0.7, addList([]), delList([]))])])))
]).

testPreds([pred(not_empty, [location]), pred(not_tfull, [truck]), pred(not_tin, [truck, location]), pred(not_shop, [location]), pred(not_depot, [location])]).


special_predicates([empty(b1)]).
rewardList([reward([node(2,empty(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(empty, _, 0.99).

ops([load, unload, drive]).

exo_ops([consume]).




operator(load, [xh, xl], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(unload, [xh], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(drive, [xh, xl], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(consume, [b1], [ev(succ, [node(2,0.3,-1,-1)]), ev(fail, [node(2,0.7,-1,-1)])]).



most_action_variants(1).
actionDiagParams([xh, xl]).
domainConsts([a]).


%%%%%%%%%%%% TVD: CONSUME SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(consume, succ, empty, [b1], [1], 1, [node(1,1.0,-1,-1)]).

%%%%%%%%%%%% TVD: LOAD_ONTO_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(load, succ, tfull, [y1], [1], 6, [node(6,eq(y1,xh),5,2), node(5,tfull(y1),1,4), node(4,tin(y1,xl),3,0), node(3,depot(xl),1,0),node(2,tfull(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: UNLOAD_FROM_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unload, succ, tfull, [y2], [1], 3, [node(3,tfull(y2),2,0),node(2,eq(y2,xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(unload, succ, empty, [y3], [1], 4, [node(4,empty(y3),3,0), node(3,tin(xh,y3),2,1),node(2,tfull(xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DRIVE_TRUCK_FROM_CITY_TO_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(drive, succ, tin, [y4,y5], [1,2], 6, [node(6,tin(y4,y5),3,5), node(5,eq(y5,xl),4,0), node(4,eq(y4,xh),1,0), node(3,eq(y5,xl),1,2),node(2,eq(y4,xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: TRIVIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(_,_,eq,[y8,y9],_,2,[node(2,eq(y8,y9),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,empty,[y10],_,2,[node(2,empty(y10),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tfull,[y11],_,2,[node(2,tfull(y11),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tin,[y12,y13],_,2,[node(2,tin(y12,y13),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,shop,[y14],_,2,[node(2,shop(y14),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,depot,[y15],_,2,[node(2,depot(y15),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).



:- ensure_loaded('inventoryBkgd.yap').
