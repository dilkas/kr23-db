
:- dynamic     
   actionList/1.


templates([sc(_x49), not_sc(_x50), level0(_x1), not_level0(_x2), level1(_x13), not_level1(_x14), tfull(_x3), not_tfull(_x4), tin(_x5, _x6), not_tin(_x7, _x8), shop(_x9), not_shop(_x10), depot(_x11), not_depot(_x12), freq(_x15), not_freq(_x16), eq(_x45, _x46), not_eq(_x47, _x48)]).

arg_types([pred(sc, [location]), pred(level0, [location]), pred(level1, [location]), pred(tfull, [truck]), pred(tin, [truck, location]), pred(shop, [location]), pred(depot, [location]), pred(freq, [location])]).

preds([pred(sc, 1), pred(level0, 1), pred(level1, 1), pred(tfull, 1), pred(tin, 2), pred(shop, 1), pred(depot, 1), pred(freq, 1), pred(eq, 2)]).

predOrder([eq, level0, level1, tfull, tin, shop, depot, freq, sc]).


%%%% USER and EXO OPS have conditional effects. Atmost one condition should be applicable. 


userOps([
op(load, [truck, location], class(load(T1,P1), 0.0, preconds([not_tfull(T1), tin(T1,P1), depot(P1)]), effects([cond([], [outcome(1.0, addList([tfull(T1)]), delList([not_tfull(T1)]))])]))), 

op(unload, [truck], class(unload(T2), 0.0, preconds([tfull(T2), tin(T2,P2), shop(P2)]), effects([
cond([level0(P2)], [outcome(1.0, addList([not_level0(P2), level1(P2), not_tfull(T2), not_sc(P2)]), delList([tfull(T2), not_level1(P2), level0(P2), sc(P2)]))]),
cond([level1(P2)], [outcome(1.0, addList([not_level1(P2), not_tfull(T2), not_sc(P2)]), delList([tfull(T2), level1(P2), sc(P2)]))])
%cond([not_level0(P2),not_level1(P2)], [outcome(1.0, addList([not_tfull(T2), not_sc(P2)]), delList([tfull(T2), sc(P2)]))])
]))),

op(drive, [truck, location], class(drive(T3, P4), 0.0, preconds([tin(T3,P3),not_tin(T3,P4)]), effects([cond([], [outcome(1.0, addList([tin(T3,P4), not_tin(T3,P3)]), delList([tin(T3, P3), not_tin(T3, P4)]))])]))),

op(noop, [], class(noop, 0.0, preconds([]), effects([cond([], [outcome(1.0, addList([]), delList([]))])])))
]).



exoOps([
op(consume, [location], class(consume(P5), 0.0, preconds([shop(P5), sc(P5)]), effects(
[
cond([freq(P5),level1(P5)], [outcome(0.4, addList([level0(P5),not_level1(P5)]), delList([not_level0(P5),level1(P5)])), outcome(0.6, addList([]), delList([]))]),
cond([freq(P5),not_level0(P5),not_level1(P5)], [outcome(0.4, addList([level1(P5)]), delList([not_level1(P5)])), outcome(0.6, addList([]), delList([]))]),
cond([not_freq(P5),level1(P5)], [outcome(0.3, addList([level0(P5),not_level1(P5)]), delList([not_level0(P5),level1(P5)])), outcome(0.7, addList([]), delList([]))]),
cond([not_freq(P5),not_level0(P5),not_level1(P5)], [outcome(0.3, addList([level1(P5)]), delList([not_level1(P5)])), outcome(0.7, addList([]), delList([]))])
]))),
op(removesc, [location], class(removesc(P6), 0.0, preconds([shop(P6), not_sc(P6)]), effects([cond([], [outcome(1.0, addList([sc(P6)]), delList([not_sc(P6)]))])])))
]).
%exoOps([]).

testPreds([pred(not_level0, [location]), pred(not_level1, [location]), pred(not_tfull, [truck]), pred(not_tin, [truck, location]), pred(not_shop, [location]), pred(not_depot, [location]), pred(not_freq, [location])]).



special_predicates([level0(b1),level1(b1)]).
rewardList([reward([node(2,level0(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(level, _, 0.99).

ops([load, unload, drive]).

exo_ops([consume]).




operator(load, [xh, xp], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(unload, [xh], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(drive, [xh, xp], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

exo_operator(consume, [b1], [ev(succ, [node(4,freq(b1),3,2), node(3,0.4,-1,-1), node(2,0.3,-1,-1)]), ev(fail, [node(4,freq(b1),2,3), node(3,0.7,-1,-1), node(2,0.6,-1,-1)])]).



most_action_variants(1).
actionDiagParams([xh, xp]).
domainConsts([a]).


%%%%%%%%%%%% TVD: CONSUME SUCCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(consume, succ, level0, [b1], [1], 3, [node(3,level0(b1),1,2),node(2,level1(b1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(consume, succ, level1, [b1], [1], 3, [node(3,level0(b1),0,2),node(2,level1(b1),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: LOAD_ONTO_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(load, succ, tfull, [y1], [1], 6, [node(6,eq(y1,xh),5,2), node(5,tfull(y1),1,4), node(4,tin(y1,xp),3,0), node(3,depot(xp),1,0),node(2,tfull(y1),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: UNLOAD_FROM_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unload, succ, tfull, [y2], [1], 3, [node(3,tfull(y2),2,0),node(2,eq(y2,xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(unload, succ, level0, [y2], [1], 5, [node(5,level0(y2),4,0), node(4,tin(xh,y2),3,1), node(3,shop(y2),2,1),node(2,tfull(xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).

tvd(unload, succ, level1, [y3], [1], 8, [node(8,level1(y3),3,7), node(7,level0(y3),6,0), node(6,tin(xh,y3),5,0), node(5,tfull(xh),4,0), node(4,shop(y3),1,0), node(3,tin(xh,y3),2,1), node(2,tfull(xh),0,1), node(1,1.0,-1,-1), node(0,0.0,-1,-1)]).

%%%%%%%%%%%% TVD: DRIVE_TRUCK_FROM_CITY_TO_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(drive, succ, tin, [y4,y5], [1,2], 6, [node(6,tin(y4,y5),3,5),node(5,eq(y5,xp),4,0),node(4,eq(y4,xh),1,0),node(3,eq(y5,xp),1,2),node(2,eq(y4,xh),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


%%%%%%%%%%%% TVD: TRIVIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tvd(_,_,eq,[y8,y9],_,2,[node(2,eq(y8,y9),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tfull,[y11],_,2,[node(2,tfull(y11),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,tin,[y12,y13],_,2,[node(2,tin(y12,y13),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,shop,[y14],_,2,[node(2,shop(y14),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,depot,[y15],_,2,[node(2,depot(y15),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,freq,[y16],_,2,[node(2,freq(y16),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,level0,[y17],_,2,[node(2,level0(y17),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).
tvd(_,_,level1,[y18],_,2,[node(2,level1(y18),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)]).


:- ensure_loaded('inventoryBkgd.yap').
