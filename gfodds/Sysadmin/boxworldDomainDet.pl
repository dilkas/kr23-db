
:- dynamic 
    bin/2,
    not_bin/2,
    tin/2,
    not_tin/2,
    pin/2,
    not_pin/2,
    ont/2,
    not_ont/2,
    onp/2, 
    not_onp/2,
    dest/2,
    not_dest/2,
    canDrive/2,
    not_canDrive/2,
    canFly/2,
    not_canFly/2,
    xDr1/2,
    not_xDr1/2,
    xDr2/2,
    not_xDr2/2,
    xDr3/2,
    not_xDr3/2,
    eq/2,
    not_eq/2,
    actionList/1.


templates([bin(_x1, _x2), not_bin(_x3, _x4), tin(_x5, _x6), not_tin(_x7, _x8), ont(_x9, _x10), not_ont(_x11, _x12), onp(_x13, _x14), not_onp(_x15, _x16), pin(_x17, _x18), not_pin(_x19, _x20), dest(_x21, _x22), not_dest(_x23, _x24), canDrive(_x25, _x26), not_canDrive(_x27, _x28), canFly(_x29, _x30), not_canFly(_x31, _x32), xDr1(_x33, _x34), not_xDr1(_x35, _x36), xDr2(_x37, _x38), not_xDr2(_x39, _x40), xDr3(_x41, _x42), not_xDr3(_x43, _x44), eq(_x45, _x46), not_eq(_x47, _x48)]).


preds([pred(bin, 2), pred(tin, 2), pred(pin, 2), pred(ont, 2), pred(onp, 2), pred(dest, 2), pred(canDrive, 2), pred(canFly, 2), pred(xDr1, 2), pred(xDr2, 2), pred(xDr3, 2), pred(eq, 2)]).
predOrder([eq, dest, bin, ont, onp, tin, pin, canDrive, canFly, xDr1, xDr2, xDr3]).



testOps([op(loadt, [box, truck, city], class(loadt(A,B,C), 0.0, preconds([bin(A,C), tin(B,C), not_dest(A, C)]), effects([cond([], [outcome(1.0, addList([ont(A,B), not_bin(A,C)]), delList([bin(A,C), not_ont(A,B)]))])]))), op(loadp, [box, plane, city], class(loadp(A,B,C), 0.0, preconds([bin(A,C), pin(B,C), not_dest(A, C)]), effects([cond([], [outcome(1.0, addList([onp(A,B), not_bin(A,C)]), delList([bin(A,C), not_onp(A,B)]))])]))), op(unloadt, [box, truck, city], class(unloadt(D, E, F), 0.0, preconds([tin(E, F), ont(D, E)]), effects([cond([], [outcome(1.0, addList([bin(D, F), not_ont(D, E)]), delList([not_bin(D, F), ont(D, E)]))])]))), op(unloadp, [box, plane, city], class(unloadp(D, E, F), 0.0, preconds([pin(E, F), onp(D, E)]), effects([cond([], [outcome(1.0, addList([bin(D, F), not_onp(D, E)]), delList([not_bin(D, F), onp(D, E)]))])]))), op(drive, [truck, city, city, city, city, city], class(drive(G, H, I), 0.0, preconds([tin(G, H), canDrive(H, I), xDr1(H, J), xDr2(H, K), xDr3(H, L)]), effects([cond([], [outcome(0.8, addList([tin(G, I), not_tin(G, H)]), delList([tin(G, H), not_tin(G, I)])), outcome(0.06666, addList([tin(G, J), not_tin(G, H)]), delList([tin(G, H), not_tin(G, J)])), outcome(0.06666, addList([tin(G, K), not_tin(G, H)]), delList([tin(G, H), not_tin(G, K)])), outcome(0.06666, addList([tin(G, L), not_tin(G, H)]), delList([tin(G, H), not_tin(G, L)]))])]))), op(fly, [plane, city, city], class(fly(M, N, O), 0.0, preconds([pin(M, N), canFly(N, O)]), effects([cond([], [outcome(1.0, addList([pin(M, O), not_pin(M, N)]), delList([pin(M, N), not_pin(M, O)]))])])))]).

testPreds([pred(not_bin, [box, city]), pred(not_tin, [truck, city]), pred(not_pin, [plane, city]), pred(not_ont, [box, truck]), pred(not_onp, [box, plane]), pred(not_dest, [box, city]), pred(not_canDrive, [city, city]), pred(not_canFly, [city, city]), pred(not_xDr1, [city, city]), pred(not_xDr2, [city, city]), pred(not_xDr3, [city, city])]).


rewardList([reward([node(3,bin(a, b),2,0),node(2,500.0,-1,-1),node(0,0.0,-1,-1)])]).

discountfactor(bin, _, 0.99).

ops([loadt, unloadt, loadp, unloadp, drive, fly]).
%ops([loadt]).

operator(loadt, [xb, xt, xc], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(loadp, [xb, xp, xc], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

operator(unloadt, [xb, xt, xc], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(unloadp, [xb, xp, xc], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).

operator(drive, [xt, xc, xc2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).
operator(fly, [xp, xc, xc2], [node(0, 0.0, -1, -1)], [ev(succ, [node(1, 1.0, -1, -1)])]).



most_action_variants(1).

actionDiagParams([xb, xc, xc2, xt, xp]).

discountfactor(bin, _, 0.9).

domainConsts([a, b]).

%%%%%%%%%%%% TVD: LOAD_BOX_ON_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(loadt, succ, bin, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xc), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 0), node(3, tin(xt, xc), 0, 1), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, bin, [y1, y2], [2, 1], 7, [node(7, eq(y2, xc), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 0), node(3, tin(xt, xc), 0, 1), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, ont, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xt), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 2), node(3, tin(xt, xc), 1, 0), node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, ont, [y1, y2], [2, 1], 7, [node(7, eq(y2, xt), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 2), node(3, tin(xt, xc), 1, 0), node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, onp, [y3, y4], [_, _], 2, [node(2, onp(y3, y4), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, tin, [y5, y6], [_, _], 2, [node(2, tin(y5, y6), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadt, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).


%%%%%%%%%%%% TVD: LOAD_BOX_ON_PLANE_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(loadp, succ, bin, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xc), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 0), node(3, pin(xp, xc), 0, 1), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, bin, [y1, y2], [2, 1], 7, [node(7, eq(y2, xc), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 0), node(3, pin(xp, xc), 0, 1), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, onp, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xp), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 2), node(3, pin(xp, xc), 1, 0), node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, onp, [y1, y2], [2, 1], 7, [node(7, eq(y2, xp), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, dest(xb, xc), 2, 4), node(4, bin(xb, xc), 3, 2), node(3, pin(xp, xc), 1, 0), node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, ont, [y3, y4], [_, _], 2, [node(2, ont(y3, y4), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, tin, [y5, y6], [_, _], 2, [node(2, tin(y5, y6), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(loadp, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: UNLOAD_BOX_FROM_TRUCK_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unloadt, succ, bin, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xc), 5, 2), node(5, bin(xb, xc), 1, 4), node(4, ont(xb, xt), 3, 0), node(3, tin(xt, xc), 1, 0), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, bin, [y1, y2], [2, 1], 7, [node(7, eq(y2, xc), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, bin(xb, xc), 1, 4), node(4, ont(xb, xt), 3, 0), node(3, tin(xt, xc), 1, 0), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, ont, [y1, y2], [1, 2], 6, [node(6, eq(y1, xb), 5, 2), node(5, eq(y2, xt), 4, 2), node(4, ont(xb, xt), 3, 0), node(3, tin(xt, xc), 0, 1), node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, ont, [y1, y2], [2, 1], 6, [node(6, eq(y2, xt), 5, 2), node(5, eq(y1, xb), 4, 2), node(4, ont(xb, xt), 3, 0), node(3, tin(xt, xc), 0, 1), node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, onp, [y3, y4], [_, _], 2, [node(2, onp(y3, y4), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, tin, [y5, y6], [_, _], 2, [node(2, tin(y5, y6), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadt, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: UNLOAD_BOX_FROM_PLANE_IN_CITY SUCCESS %%%%%%%%%%%%%%%

tvd(unloadp, succ, bin, [y1, y2], [1, 2], 7, [node(7, eq(y1, xb), 6, 2), node(6, eq(y2, xc), 5, 2), node(5, bin(xb, xc), 1, 4), node(4, onp(xb, xp), 3, 0), node(3, pin(xp, xc), 1, 0), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, bin, [y1, y2], [2, 1], 7, [node(7, eq(y2, xc), 6, 2), node(6, eq(y1, xb), 5, 2), node(5, bin(xb, xc), 1, 4), node(4, onp(xb, xp), 3, 0), node(3, pin(xp, xc), 1, 0), node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, onp, [y1, y2], [1, 2], 6, [node(6, eq(y1, xb), 5, 2), node(5, eq(y2, xp), 4, 2), node(4, onp(xb, xp), 3, 0), node(3, pin(xp, xc), 0, 1), node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, onp, [y1, y2], [2, 1], 6, [node(6, eq(y2, xp), 5, 2), node(5, eq(y1, xb), 4, 2), node(4, onp(xb, xp), 3, 0), node(3, pin(xp, xc), 0, 1), node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, ont, [y3, y4], [_, _], 2, [node(2, ont(y3, y4), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, tin, [y5, y6], [_, _], 2, [node(2, tin(y5, y6), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(unloadp, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: DRIVE SUCCESS %%%%%%%%%%%%%%%

tvd(drive, succ, bin, [y1, y2], [_, _], 2, [node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, ont, [y1, y2], [_, _], 2, [node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, onp, [y1, y2], [_, _], 2, [node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, tin, [y1, y2], [1, 2], 10, [node(10, eq(y1, xt), 9, 2), node(9, eq(y2, xc), 7, 8), node(8, eq(y2, xc2), 5, 2), node(7, tin(xt, xc), 6, 0), node(6, canDrive(xc, xc2), 0, 1), node(5, tin(y1, y2), 1, 4), node(4, tin(xt, xc), 3, 0), node(3, canDrive(xc, xc2), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, tin, [y1, y2], [2, 1], 11, [node(11, eq(y2, xc), 9, 10), node(10, eq(y2, xc2), 8, 2), node(9, eq(y1, xt), 7, 2), node(8, eq(y1, xt), 5, 2), node(7, tin(xt, xc), 6, 0), node(6, canDrive(xc, xc2), 0, 1), node(5, tin(y1, y2), 1, 4), node(4, tin(xt, xc), 3, 0), node(3, canDrive(xc, xc2), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: DRIVE FAIL1 %%%%%%%%%%%%%%%

tvd(drive, fail1, bin, [y1, y2], [_, _], 2, [node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, ont, [y1, y2], [_, _], 2, [node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, onp, [y1, y2], [_, _], 2, [node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, tin, [y1, y2], [1, 2], 8, [node(8, eq(y1, xt), 7, 2), node(7, eq(y2, xwc1), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr1(xc, xwc1), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, tin, [y1, y2], [2, 1], 8, [node(8, eq(y2, xwc1), 7, 2), node(7, eq(y1, xt), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr1(xc, xwc1), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail1, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: DRIVE FAIL2 %%%%%%%%%%%%%%%

tvd(drive, fail2, bin, [y1, y2], [_, _], 2, [node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, ont, [y1, y2], [_, _], 2, [node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, onp, [y1, y2], [_, _], 2, [node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, tin, [y1, y2], [1, 2], 8, [node(8, eq(y1, xt), 7, 2), node(7, eq(y2, xwc2), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr2(xc, xwc2), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, tin, [y1, y2], [2, 1], 8, [node(8, eq(y2, xwc2), 7, 2), node(7, eq(y1, xt), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr2(xc, xwc2), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail2, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

%%%%%%%%%%%% TVD: DRIVE FAIL3 %%%%%%%%%%%%%%%

tvd(drive, fail3, bin, [y1, y2], [_, _], 2, [node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, ont, [y1, y2], [_, _], 2, [node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, onp, [y1, y2], [_, _], 2, [node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, tin, [y1, y2], [1, 2], 8, [node(8, eq(y1, xt), 7, 2), node(7, eq(y2, xwc3), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr3(xc, xwc3), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, tin, [y1, y2], [2, 1], 8, [node(8, eq(y2, xwc3), 7, 2), node(7, eq(y1, xt), 6, 2), node(6, tin(y1, y2), 1, 5), node(5, tin(xt, xc), 4, 0), node(4, canDrive(xc, xc2), 3, 0), node(3, xDr3(xc, xwc3), 1, 0), node(2, tin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, pin, [y7, y8], [_, _], 2, [node(2, pin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(drive, fail3, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).


%%%%%%%%%%%% TVD: FLY SUCCESS %%%%%%%%%%%%%%%


tvd(fly, succ, bin, [y1, y2], [_, _], 2, [node(2, bin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, ont, [y1, y2], [_, _], 2, [node(2, ont(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, onp, [y1, y2], [_, _], 2, [node(2, onp(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, tin, [y7, y8], [_, _], 2, [node(2, tin(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, pin, [y1, y2], [1, 2], 10, [node(10, eq(y1, xp), 9, 2), node(9, eq(y2, xc), 7, 8), node(8, eq(y2, xc2), 5, 2), node(7, pin(xp, xc), 6, 0), node(6, canFly(xc, xc2), 0, 1), node(5, pin(y1, y2), 1, 4), node(4, pin(xp, xc), 3, 0), node(3, canFly(xc, xc2), 1, 0), node(2, pin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, pin, [y1, y2], [2, 1], 11, [node(11, eq(y2, xc), 9, 10), node(10, eq(y2, xc2), 8, 2), node(9, eq(y1, xp), 7, 2), node(8, eq(y1, xp), 5, 2), node(7, pin(xp, xc), 6, 0), node(6, canFly(xc, xc2), 0, 1), node(5, pin(y1, y2), 1, 4), node(4, pin(xp, xc), 3, 0), node(3, canFly(xc, xc2), 1, 0), node(2, pin(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, dest, [y1, y2], [_, _], 2, [node(2, dest(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, canDrive, [y1, y2], [_, _], 2, [node(2, canDrive(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, canFly, [y1, y2], [_, _], 2, [node(2, canFly(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, xDr1, [y1, y2], [_, _], 2, [node(2, xDr1(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, xDr2, [y1, y2], [_, _], 2, [node(2, xDr2(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, xDr3, [y1, y2], [_, _], 2, [node(2, xDr3(y1, y2), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).

tvd(fly, succ, eq, [y7, y8], [_, _], 2, [node(2, eq(y7, y8), 1, 0), node(1, 1.0, -1, -1), node(0, 0.0, -1, -1)]).




%%%%%%%%%%%%% GENERAL BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%

rule(eq(A, B), [1, 1], [eq(B, A)]).
rule(eq(A, B), [1, 1], [eq(A,C), eq(B,D), eq(C, D)]).

rule(not_eq(A, B), [1, 1], [not_eq(B, A)]).
rule(not_eq(A, B), [1, 1], [eq(A,C), eq(B,D), not_eq(C, D)]).


%%%%%%%%%%%%% DOMAIN SPECIFIC BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%


rule(eq(C1, C2), [1, 1], [bin(B1, C1), bin(B2, C2), eq(B1, B2)]).
rule(eq(C1, C2), [1, 1], [tin(T1, C1), tin(T2, C2), eq(T1, T2)]).
rule(eq(T1, T2), [1, 1], [ont(B1, T1), ont(B2, T2), eq(B1, B2)]).
rule(eq(P1, P2), [1, 1], [onp(B1, P1), onp(B2, P2), eq(B1, B2)]).
rule(eq(C1, C2), [1, 1], [dest(B1, C1), dest(B2, C2), eq(B1, B2)]).



rule(not_eq(C1, C2), [1, 1], [bin(B1, C1), not_bin(B2, C2), eq(B1, B2)]).
rule(not_eq(C1, C2), [1, 1], [tin(T1, C1), not_tin(T2, C2), eq(T1, T2)]).
rule(not_eq(C1, C2), [1, 1], [pin(P1, C1), not_pin(P2, C2), eq(P1, P2)]).


rule(not_eq(T1, T2), [1, 1], [ont(B1, T1), not_ont(B2, T2), eq(B1, B2)]).
rule(not_eq(P1, P2), [1, 1], [onp(B1, P1), not_onp(B2, P2), eq(B1, B2)]).
rule(not_eq(T1, T2), [1, 1], [tin(T1, C1), tin(T2, C2), not_eq(C1, C2)]).
rule(not_eq(P1, P2), [1, 1], [pin(P1, C1), pin(P2, C2), not_eq(C1, C2)]).


rule(not_eq(B1, B2), [1, 1], [bin(B1, C1), bin(B2, C2), not_eq(C1, C2)]).
rule(not_eq(B1, B2), [1, 1], [ont(B1, T1), ont(B2, T2), not_eq(T1, T2)]).
rule(not_eq(B1, B2), [1, 1], [onp(B1, P1), onp(B2, P2), not_eq(P1, P2)]).
rule(not_eq(B1, B2), [1, 1], [bin(B1, _), ont(B2, _)]).
rule(not_eq(B1, B2), [1, 1], [bin(B1, _), onp(B2, _)]).

rule(bin(B1, C1), [1, 1], [bin(B2, C2), eq(B1, B2), eq(C1, C2)]).

rule(not_bin(B1, C1), [1, 0], [ont(B2, _), eq(B1, B2), eq(C1, C1)]).
rule(not_bin(B1, C1), [1, 0], [onp(B2, _), eq(B1, B2), eq(C1, C1)]).


rule(not_bin(B1, C1), [1, 1], [bin(B2, C2), eq(B1, B2), not_eq(C1, C2)]).
rule(not_bin(B1, C1), [1, 1], [not_bin(B2, C2), eq(B1, B2), eq(C1, C2)]).

rule(tin(T1, C1), [1, 1], [tin(T2, C2), eq(T1, T2), eq(C1, C2)]).
rule(pin(P1, C1), [1, 1], [pin(P2, C2), eq(P1, P2), eq(C1, C2)]).

rule(not_tin(T1, C1), [1, 1], [tin(T2, C2), eq(T1, T2), not_eq(C1, C2)]).
rule(not_tin(T1, C1), [1, 1], [not_tin(T2, C2), eq(T1, T2), eq(C1, C2)]).

rule(not_pin(P1, C1), [1, 1], [pin(P2, C2), eq(P1, P2), not_eq(C1, C2)]).
rule(not_pin(P1, C1), [1, 1], [not_pin(P2, C2), eq(P1, P2), eq(C1, C2)]).


rule(ont(B1, T1), [1, 1], [ont(B2, T2), eq(B1, B2), eq(T1, T2)]).
rule(onp(B1, P1), [1, 1], [onp(B2, P2), eq(B1, B2), eq(P1, P2)]).

rule(not_ont(B1, T1), [1, 0], [bin(B2, _), eq(B1, B2), eq(T1, T1)]).
rule(not_ont(B1, T1), [1, 1], [not_ont(B2, T2), eq(B1, B2), eq(T1, T2)]).
rule(not_ont(B1, T1), [1, 1], [ont(B2, T2), eq(B1, B2), not_eq(T1, T2)]).
rule(not_ont(B1, T1), [1, 0], [onp(B2, P2), eq(B1, B2), eq(T1, T1)]).

rule(not_onp(B1, P1), [1, 0], [bin(B2, _), eq(B1, B2), eq(P1, P1)]).
rule(not_onp(B1, P1), [1, 1], [not_onp(B2, P2), eq(B1, B2), eq(P1, P2)]).
rule(not_onp(B1, P1), [1, 1], [onp(B2, P2), eq(B1, B2), not_eq(P1, P2)]).
rule(not_onp(B1, P1), [1, 0], [ont(B2, T2), eq(B1, B2), eq(P1, P1)]).

rule(dest(B1, C1), [1, 1], [dest(B2, C2), eq(B1, B2), eq(C1, C2)]).
rule(not_dest(B1, C1), [1, 1], [dest(B2, C2), eq(B1, B2), not_eq(C1, C2)]).




%%%% 8995.383 seconds
