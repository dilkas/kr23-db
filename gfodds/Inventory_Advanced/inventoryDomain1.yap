
:- dynamic     
   actionList/1.


templates([level(_x1,_x13), not_level(_x2,_x14), tfull(_x3), not_tfull(_x4), tin(_x5, _x6), not_tin(_x7, _x8), shop(_x9), not_shop(_x10), depot(_x11), not_depot(_x12), freq(_x15), not_freq(_x16), succ(_x17,_x18), not_succ(_x19,_x20), eq(_x45, _x46), not_eq(_x47, _x48)]).

arg_types([pred(level, [location,invlevel]), pred(tfull, [truck]), pred(tin, [truck, location]), pred(shop, [location]), pred(depot, [location]), pred(freq, [location]), pred(succ, [invlevel,invlevel])]).

preds([pred(level, 2), pred(tfull, 1), pred(tin, 2), pred(shop, 1), pred(depot, 1), pred(freq, 1), pred(succ, 2), pred(eq, 2)]).

predOrder([eq, level, tfull, tin, shop, depot, freq, succ]).


userOps([
op(load, [truck, location], class(load(T1,P1), 0.0, preconds([not_tfull(T1), tin(T1,P1), depot(P1)]), effects([cond([], [outcome(1.0, addList([tfull(T1)]), delList([not_tfull(T1)]))])]))), 
op(unload, [truck, location, invlevel], class(unload(T2,P2,L1), 0.0, preconds([tfull(T2), shop(P2), level(P2,L1), tin(T2,P2), succ(L1,L2)]), effects([cond([], [outcome(1.0, addList([not_level(P2,L1), level(P2,L2), not_tfull(T2)]), delList([tfull(T2), not_level(P2,L2), level(P2,L1)]))])]))),
op(drive, [truck, location, location], class(drive(T3, P3, P4), 0.0, preconds([tin(T3,P3), not_tin(T3,P4)]), effects([cond([], [outcome(1.0, addList([tin(T3,P4), not_tin(T3,P3)]), delList([tin(T3, P3), not_tin(T3, P4)]))])])))
]).

exoOps([
op(consume, [location,invlevel], class(consume(P5,L3), 0.0, preconds([shop(P5), level(P5,L3), succ(L4,L3)]), effects(
[
cond([freq(P5)], [outcome(0.7, addList([level(P5,L4),not_level(P5,L3)]), delList([not_level(P5,L4),level(P5,L3)])), outcome(0.3, addList([]), delList([]))]),
cond([not_freq(P5)], [outcome(0.3, addList([level(P5,L4),not_level(P5,L3)]), delList([not_level(P5,L4),level(P5,L3)])), outcome(0.6, addList([]), delList([]))])
])))
]).
%exoOps([]).

testPreds([pred(not_level, [location, invlevel]), pred(not_tfull, [truck]), pred(not_tin, [truck, location]), pred(not_shop, [location]), pred(not_depot, [location]), pred(not_freq, [location]), pred(not_succ, [invlevel,invlevel])]).



special_predicates([level0(b1),level1(b1)]).
rewardList([reward([node(2,level(b1,0),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(level, _, 0.99).

ops([load, unload, drive]).
%ops([unload,drive]).

exo_ops([consume]).
%exo_ops([]).




operator(load, [xh, xp], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(unload, [xh, xp, xl], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(drive, [xh, xp, xp1], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(consume, [b1,xl], [ev(succ, [node(4,freq(b1),3,2), node(3,0.7,-1,-1), node(2,0.3,-1,-1)]), ev(fail, [node(4,freq(b1),2,3), node(3,0.7,-1,-1), node(2,0.3,-1,-1)])]).



most_action_variants(1).
actionDiagParams([xh, xp, xp1, xl]).
domainConsts([a,0,1,2]).


%%%%%%%%%%%% TVD: CONSUME SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(consume, succ, level, [b1,y1], [1,2], 4, [node(4,level(b1,xl),3,2),node(3,succ(y1,xl),1,0),node(2,level(b1,y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: LOAD_ONTO_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(load, succ, tfull, [y1], [1], 6, [node(6,eq(y1,xh),5,2), node(5,tfull(y1),1,4), node(4,tin(y1,xp),3,0), node(3,depot(xp),1,0),node(2,tfull(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: UNLOAD_FROM_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unload, succ, tfull, [y2], [1], 5, [node(5,eq(y2,xh),4,2), node(4,tin(y2,xp),3,2), node(3,shop(xp),0,2),node(2,tfull(y2),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(unload, succ, level, [y2,y3], [1,2], 8, [node(8,eq(y2,xp),7,2), node(7,level(y2,xl),6,2), node(6,tfull(xh),5,2), node(5,tin(xh,y2),4,2), node(4,shop(y2),3,2),node(3,succ(xl,y3),1,0),node(2,level(y2,y3),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DRIVE_TRUCK_FROM_CITY_TO_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(drive, succ, tin, [y4,y5], [1,2], 7, [node(7,eq(y4,xh),6,2), node(6,eq(y5,xp),5,4), node(5,eq(y5,xp1),2,0), node(4,eq(y5,xp1),3,2), node(3,tin(y4,xp),1,2),node(2,tin(y4,y5),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: TRIVIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(_,_,eq,[y8,y9],_,2,[node(2,eq(y8,y9),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,level,[y10,y101],_,2,[node(2,level(y10,y101),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tfull,[y11],_,2,[node(2,tfull(y11),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tin,[y12,y13],_,2,[node(2,tin(y12,y13),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,shop,[y14],_,2,[node(2,shop(y14),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,depot,[y15],_,2,[node(2,depot(y15),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,freq,[y16],_,2,[node(2,freq(y16),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,succ,[y17,y18],_,2,[node(2,succ(y17,y18),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).



:- ensure_loaded('inventoryBkgd.yap').
