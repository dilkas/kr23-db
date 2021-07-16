
:- dynamic     
   actionList/1.


templates([empty(_x1), not_empty(_x2), tfull(_x3), not_tfull(_x4), tin(_x5, _x6), not_tin(_x7, _x8), shop(_x9), not_shop(_x10), depot(_x11), not_depot(_x12), eq(_x45, _x46), not_eq(_x47, _x48)]).

arg_types([pred(empty, [location]), pred(tfull, [truck]), pred(tin, [truck, location]), pred(shop, [location]), pred(depot, [location])]).



preds([pred(empty, 1), pred(tfull, 1), pred(tin, 2), pred(shop, 1), pred(depot, 1), pred(eq, 2)]).

predOrder([eq, tfull, tin, shop, depot, empty]).


userOps([
op(load, [truck, location], class(load(T1,L1), 0.0, preconds([not_tfull(T1), tin(T1,L1), depot(L1)]), effects([cond([], [outcome(1.0, addList([tfull(T1)]), delList([not_tfull(T1)]))])]))), 
op(unload, [truck, location], class(unload(T2,L2), 0.0, preconds([tfull(T2), shop(L2), empty(L2), tin(T2,L2)]), effects([cond([], [outcome(1.0, addList([not_empty(L2), not_tfull(T2)]), delList([tfull(T2), empty(L2)]))])]))),
op(drive, [truck, location, location], class(drive(T3, L3, L4), 0.0, preconds([tin(T3,L3), not_tin(T3,L4)]), effects([cond([], [outcome(1.0, addList([tin(T3,L4), not_tin(T3,L3)]), delList([tin(T3, L3), not_tin(T3, L4)]))])])))
]).

exoOps([
op(consume, [location], class(consume(L5), 0.0, preconds([shop(L5), not_empty(L5)]), effects([cond([], [outcome(0.4, addList([empty(L5)]), delList([not_empty(L5)])), outcome(0.6, addList([]), delList([]))])])))
]).
%exoOps([]).

testPreds([pred(not_empty, [location]), pred(not_tfull, [truck]), pred(not_tin, [truck, location]), pred(not_shop, [location]), pred(not_depot, [location])]).



special_predicates([empty(b1)]).
rewardList([reward([node(2,empty(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(empty, _, 0.99).

ops([load, unload, drive]).
%ops([unload,drive]).

exo_ops([consume]).
%exo_ops([]).




operator(load, [xh, xl], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(unload, [xh, xl], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(drive, [xh, xl, xl1], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(consume, [b1], [ev(succ, [node(2,0.4,-1,-1)]), ev(fail, [node(2,0.6,-1,-1)])]).



most_action_variants(1).
actionDiagParams([xh, xl, xl1]).
discountfactor(empty, _, 0.9).
domainConsts([a]).


%%%%%%%%%%%% TVD: CONSUME SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(consume, succ, empty, [b1], [1], 1, [node(1,1.0,-1,-1)]).

%%%%%%%%%%%% TVD: LOAD_ONTO_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(load, succ, tfull, [y1], [1], 6, [node(6,eq(y1,xh),5,2), node(5,tfull(y1),1,4), node(4,tin(y1,xl),3,0), node(3,depot(xl),1,0),node(2,tfull(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: UNLOAD_FROM_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unload, succ, tfull, [y2], [1], 7, [node(7,eq(y2,xh),6,2), node(6,empty(xl),5,2), node(5,tfull(y2),4,0), node(4,tin(y2,xl),3,1), node(3,shop(xl),0,1),node(2,tfull(y2),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(unload, succ, empty, [y3], [1], 7, [node(7,eq(y3,xl),6,2), node(6,empty(y3),5,0), node(5,tfull(xh),4,1), node(4,tin(xh,y3),3,1), node(3,shop(y3),0,1),node(2,empty(y3),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DRIVE_TRUCK_FROM_CITY_TO_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(drive, succ, tin, [y4,y5], [1,2], 7, [node(7,eq(y4,xh),6,2), node(6,eq(y5,xl),5,4), node(5,eq(y5,xl1),2,0), node(4,eq(y5,xl1),3,2), node(3,tin(y4,xl),1,2),node(2,tin(y4,y5),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(drive, succ, tin, [y6,y7], [2,1], 8, [node(8,eq(y7,xl),7,5), node(7,eq(y7,xl1),2,6), node(6,eq(y6,xh),0,2), node(5,eq(y7,xl1),4,2), node(4,eq(y6,xh),3,2), node(3,tin(y6,xl),1,2),node(2,tin(y6,y7),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: TRIVIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(_,_,eq,[y8,y9],_,2,[node(2,eq(y8,y9),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,empty,[y10],_,2,[node(2,empty(y10),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tfull,[y11],_,2,[node(2,tfull(y11),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tin,[y12,y13],_,2,[node(2,tin(y12,y13),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,shop,[y14],_,2,[node(2,shop(y14),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,depot,[y15],_,2,[node(2,depot(y15),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).



:- ensure_loaded('inventoryBkgd.yap').
