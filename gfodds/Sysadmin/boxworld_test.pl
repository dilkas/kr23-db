%problems([p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12]).

%problems([p06, p07, p08, p09, p10, p11, p12, p13, p14, p15]).

problems([p06]).

%problems([p0002]).


testDomain(p0001, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4])]).

initState(p0001, [dest(b0, c4), dest(b1, c4), dest(b2, c2), dest(b3, c4), dest(b4, c3), dest(b5, c4), dest(b6, c3), dest(b7, c1), dest(b8, c3), dest(b9, c0), bin(b0, c0), bin(b1, c0), bin(b2, c0), bin(b3, c1), bin(b4, c1), bin(b5, c2), bin(b6, c4), bin(b7, c3), bin(b8, c0), bin(b9, c1), tin(t0, c0), tin(t1, c0), tin(t2, c1), tin(t3, c1), pin(p0, c0), pin(p1, c1), canDrive(c0, c1), canDrive(c0, c2), canDrive(c0, c3), canDrive(c0, c4), canDrive(c1, c0), canDrive(c1, c2), canDrive(c1, c3), canDrive(c2, c0), canDrive(c2, c1), canDrive(c2, c3), canDrive(c2, c4), canDrive(c3, c0), canDrive(c3, c1), canDrive(c3, c2), canDrive(c3, c4), canDrive(c4, c0), canDrive(c4, c2), canDrive(c4, c3), canFly(c0, c1), canFly(c1, c0), xDr1(c0, c3), xDr2(c0, c4), xDr3(c0, c2), xDr1(c1, c3), xDr2(c1, c0), xDr3(c1, c2), xDr1(c2, c0), xDr2(c2, c1), xDr3(c2, c4), xDr1(c3, c0), xDr2(c3, c1), xDr3(c3, c2), xDr1(c4, c0), xDr2(c4, c2), xDr3(c4, c3)]).
 
goal(p0001, [bin(b0, c4), bin(b1, c4), bin(b2, c2), bin(b3, c4), bin(b4, c3), bin(b5, c4), bin(b6, c3), bin(b7, c1), bin(b8, c3), bin(b9, c0), dest(b0, c4), dest(b1, c4), dest(b2, c2), dest(b3, c4), dest(b4, c3), dest(b5, c4), dest(b6, c3), dest(b7, c1), dest(b8, c3), dest(b9, c0)], 500.0).




testDomain(p0002, [objects(box, [b0,b1]), objects(truck, [t0]), objects(plane, [p0]), objects(city, [c0,c1,c2])]).
initState(p0002, [dest(b0,c1), bin(b0,c0), tin(t0,c0), pin(p0,c0), canDrive(c0,c1), canDrive(c1,c0), xDr1(c0,c1), xDr2(c0,c1), xDr3(c0,c1), xDr1(c1,c0), xDr2(c1,c0), xDr3(c1,c0)]).
goal(p0002, [bin(b0,c1)], 500.0).






testDomain(p01, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4])]).

initState(p01, [bin(b0,c1), dest(b0,c4), bin(b1,c4), dest(b1,c1), bin(b2,c2), dest(b2,c3), bin(b3,c0), dest(b3,c1), bin(b4,c0), dest(b4,c4), bin(b5,c0), dest(b5,c1), bin(b6,c0), dest(b6,c4), bin(b7,c1), dest(b7,c0), bin(b8,c1), dest(b8,c3), bin(b9,c3), dest(b9,c1), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c1), canDrive(c0,c4), xDr1(c0,c3), xDr2(c0,c1), xDr3(c0,c4), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c3), canDrive(c1,c4), canDrive(c1,c2), xDr1(c1,c0), xDr2(c1,c3), xDr3(c1,c4), canFly(c1,c0), canDrive(c2,c3), canDrive(c2,c4), canDrive(c2,c1), xDr1(c2,c3), xDr2(c2,c4), xDr3(c2,c1), canDrive(c3,c0), canDrive(c3,c1), canDrive(c3,c2), canDrive(c3,c4), xDr1(c3,c0), xDr2(c3,c1), xDr3(c3,c2), canDrive(c4,c0), canDrive(c4,c1), canDrive(c4,c2), canDrive(c4,c3), xDr1(c4,c0), xDr2(c4,c1), xDr3(c4,c2)]).

goal(p01, [bin(b0,c4), bin(b1,c1), bin(b2,c3), bin(b3,c1), bin(b4,c4), bin(b5,c1), bin(b6,c4), bin(b7,c0), bin(b8,c3), bin(b9,c1)], 1.0).

cost(p01, []).



testDomain(p02, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4])]).

initState(p02, [bin(b0,c1), dest(b0,c4), bin(b1,c4), dest(b1,c1), bin(b2,c2), dest(b2,c3), bin(b3,c0), dest(b3,c1), bin(b4,c0), dest(b4,c4), bin(b5,c0), dest(b5,c1), bin(b6,c0), dest(b6,c4), bin(b7,c1), dest(b7,c0), bin(b8,c1), dest(b8,c3), bin(b9,c3), dest(b9,c1), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c1), canDrive(c0,c4), xDr1(c0,c3), xDr2(c0,c1), xDr3(c0,c4), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c3), canDrive(c1,c4), canDrive(c1,c2), xDr1(c1,c0), xDr2(c1,c3), xDr3(c1,c4), canFly(c1,c0), canDrive(c2,c3), canDrive(c2,c4), canDrive(c2,c1), xDr1(c2,c3), xDr2(c2,c4), xDr3(c2,c1), canDrive(c3,c0), canDrive(c3,c1), canDrive(c3,c2), canDrive(c3,c4), xDr1(c3,c0), xDr2(c3,c1), xDr3(c3,c2), canDrive(c4,c0), canDrive(c4,c1), canDrive(c4,c2), canDrive(c4,c3), xDr1(c4,c0), xDr2(c4,c1), xDr3(c4,c2)]).

goal(p02, [bin(b0,c4), bin(b1,c1), bin(b2,c3), bin(b3,c1), bin(b4,c4), bin(b5,c1), bin(b6,c4), bin(b7,c0), bin(b8,c3), bin(b9,c1)], 10.0).

cost(p02, [action(unloadt, 1.0), action(unloadp, 1.0)]).



testDomain(p03, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4])]).

initState(p03, [bin(b0,c1), dest(b0,c4), bin(b1,c4), dest(b1,c1), bin(b2,c2), dest(b2,c3), bin(b3,c0), dest(b3,c1), bin(b4,c0), dest(b4,c4), bin(b5,c0), dest(b5,c1), bin(b6,c0), dest(b6,c4), bin(b7,c1), dest(b7,c0), bin(b8,c1), dest(b8,c3), bin(b9,c3), dest(b9,c1), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c1), canDrive(c0,c4), xDr1(c0,c3), xDr2(c0,c1), xDr3(c0,c4), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c3), canDrive(c1,c4), canDrive(c1,c2), xDr1(c1,c0), xDr2(c1,c3), xDr3(c1,c4), canFly(c1,c0), canDrive(c2,c3), canDrive(c2,c4), canDrive(c2,c1), xDr1(c2,c3), xDr2(c2,c4), xDr3(c2,c1), canDrive(c3,c0), canDrive(c3,c1), canDrive(c3,c2), canDrive(c3,c4), xDr1(c3,c0), xDr2(c3,c1), xDr3(c3,c2), canDrive(c4,c0), canDrive(c4,c1), canDrive(c4,c2), canDrive(c4,c3), xDr1(c4,c0), xDr2(c4,c1), xDr3(c4,c2)]).

goal(p03, [bin(b0,c4), bin(b1,c1), bin(b2,c3), bin(b3,c1), bin(b4,c4), bin(b5,c1), bin(b6,c4), bin(b7,c0), bin(b8,c3), bin(b9,c1)], 500.0).

cost(p03, [action(drive, -5.0), action(fly, -25.0), action(unloadt, 50.0), action(unloadp, 50.0)]).




testDomain(p04, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9])]).

initState(p04, [bin(b0,c0), dest(b0,c1), bin(b1,c5), dest(b1,c9), bin(b2,c1), dest(b2,c5), bin(b3,c6), dest(b3,c3), bin(b4,c3), dest(b4,c6), bin(b5,c3), dest(b5,c6), bin(b6,c4), dest(b6,c7), bin(b7,c6), dest(b7,c7), bin(b8,c1), dest(b8,c4), bin(b9,c2), dest(b9,c3), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c2), xDr1(c0,c3), xDr2(c0,c8), xDr3(c0,c2), canFly(c0,c1), canDrive(c1,c4), canDrive(c1,c7), canDrive(c1,c9), canDrive(c1,c5), xDr1(c1,c4), xDr2(c1,c7), xDr3(c1,c9), canFly(c1,c0), canDrive(c2,c0), canDrive(c2,c6), canDrive(c2,c3), canDrive(c2,c9), xDr1(c2,c0), xDr2(c2,c6), xDr3(c2,c3), canDrive(c3,c0), canDrive(c3,c2), canDrive(c3,c8), canDrive(c3,c6), xDr1(c3,c0), xDr2(c3,c2), xDr3(c3,c8), canDrive(c4,c1), canDrive(c4,c7), canDrive(c4,c9), canDrive(c4,c5), xDr1(c4,c1), xDr2(c4,c7), xDr3(c4,c9), canDrive(c5,c4), canDrive(c5,c1), canDrive(c5,c7), xDr1(c5,c4), xDr2(c5,c1), xDr3(c5,c7), canDrive(c6,c2), canDrive(c6,c9), canDrive(c6,c3), canDrive(c6,c8), xDr1(c6,c2), xDr2(c6,c9), xDr3(c6,c3), canDrive(c7,c1), canDrive(c7,c4), canDrive(c7,c5), canDrive(c7,c9), xDr1(c7,c1), xDr2(c7,c4), xDr3(c7,c5), canDrive(c8,c0), canDrive(c8,c3), canDrive(c8,c6), xDr1(c8,c0), xDr2(c8,c3), xDr3(c8,c6), canDrive(c9,c1), canDrive(c9,c2), canDrive(c9,c4), canDrive(c9,c6), canDrive(c9,c7), xDr1(c9,c1), xDr2(c9,c2), xDr3(c9,c4)]).

goal(p04, [bin(b0,c1), bin(b1,c9), bin(b2,c5), bin(b3,c3), bin(b4,c6), bin(b5,c6), bin(b6,c7), bin(b7,c7), bin(b8,c4), bin(b9,c3)], 1.0).

cost(p04, []).





testDomain(p05, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9])]).

initState(p05, [bin(b0,c0), dest(b0,c1), bin(b1,c5), dest(b1,c9), bin(b2,c1), dest(b2,c5), bin(b3,c6), dest(b3,c3), bin(b4,c3), dest(b4,c6), bin(b5,c3), dest(b5,c6), bin(b6,c4), dest(b6,c7), bin(b7,c6), dest(b7,c7), bin(b8,c1), dest(b8,c4), bin(b9,c2), dest(b9,c3), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c2), xDr1(c0,c3), xDr2(c0,c8), xDr3(c0,c2), canFly(c0,c1), canDrive(c1,c4), canDrive(c1,c7), canDrive(c1,c9), canDrive(c1,c5), xDr1(c1,c4), xDr2(c1,c7), xDr3(c1,c9), canFly(c1,c0), canDrive(c2,c0), canDrive(c2,c6), canDrive(c2,c3), canDrive(c2,c9), xDr1(c2,c0), xDr2(c2,c6), xDr3(c2,c3), canDrive(c3,c0), canDrive(c3,c2), canDrive(c3,c8), canDrive(c3,c6), xDr1(c3,c0), xDr2(c3,c2), xDr3(c3,c8), canDrive(c4,c1), canDrive(c4,c7), canDrive(c4,c9), canDrive(c4,c5), xDr1(c4,c1), xDr2(c4,c7), xDr3(c4,c9), canDrive(c5,c4), canDrive(c5,c1), canDrive(c5,c7), xDr1(c5,c4), xDr2(c5,c1), xDr3(c5,c7), canDrive(c6,c2), canDrive(c6,c9), canDrive(c6,c3), canDrive(c6,c8), xDr1(c6,c2), xDr2(c6,c9), xDr3(c6,c3), canDrive(c7,c1), canDrive(c7,c4), canDrive(c7,c5), canDrive(c7,c9), xDr1(c7,c1), xDr2(c7,c4), xDr3(c7,c5), canDrive(c8,c0), canDrive(c8,c3), canDrive(c8,c6), xDr1(c8,c0), xDr2(c8,c3), xDr3(c8,c6), canDrive(c9,c1), canDrive(c9,c2), canDrive(c9,c4), canDrive(c9,c6), canDrive(c9,c7), xDr1(c9,c1), xDr2(c9,c2), xDr3(c9,c4)]).

goal(p05, [bin(b0,c1), bin(b1,c9), bin(b2,c5), bin(b3,c3), bin(b4,c6), bin(b5,c6), bin(b6,c7), bin(b7,c7), bin(b8,c4), bin(b9,c3)], 500.0).

cost(p05, [action(drive, -8.0), action(fly, -25.0), action(unloadt, 50.0), action(unloadp, 50.0)]).





testDomain(p06, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14])]).

initState(p06, [bin(b0,c8), dest(b0,c1), bin(b1,c4), dest(b1,c12), bin(b2,c6), dest(b2,c7), bin(b3,c1), dest(b3,c14), bin(b4,c12), dest(b4,c8), bin(b5,c12), dest(b5,c11), bin(b6,c9), dest(b6,c1), bin(b7,c7), dest(b7,c3), bin(b8,c9), dest(b8,c5), bin(b9,c4), dest(b9,c7), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c11), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c10), canDrive(c0,c14), xDr1(c0,c11), xDr2(c0,c3), xDr3(c0,c8), canFly(c0,c1), canDrive(c1,c6), canDrive(c1,c7), canDrive(c1,c13), canDrive(c1,c4), xDr1(c1,c6), xDr2(c1,c7), xDr3(c1,c13), canFly(c1,c0), canDrive(c2,c12), canDrive(c2,c9), canDrive(c2,c8), xDr1(c2,c12), xDr2(c2,c9), xDr3(c2,c8), canDrive(c3,c0), canDrive(c3,c11), canDrive(c3,c8), xDr1(c3,c0), xDr2(c3,c11), xDr3(c3,c8), canDrive(c4,c5), canDrive(c4,c13), canDrive(c4,c1), xDr1(c4,c5), xDr2(c4,c13), xDr3(c4,c1), canDrive(c5,c4), canDrive(c5,c13), canDrive(c5,c9), xDr1(c5,c4), xDr2(c5,c13), xDr3(c5,c9), canDrive(c6,c1), canDrive(c6,c7), canDrive(c6,c13), xDr1(c6,c1), xDr2(c6,c7), xDr3(c6,c13), canDrive(c7,c1), canDrive(c7,c6), canDrive(c7,c13), xDr1(c7,c1), xDr2(c7,c6), xDr3(c7,c13), canDrive(c8,c0), canDrive(c8,c2), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c12), xDr1(c8,c0), xDr2(c8,c2), xDr3(c8,c3), canDrive(c9,c2), canDrive(c9,c5), canDrive(c9,c8), canDrive(c9,c13), canDrive(c9,c12), xDr1(c9,c2), xDr2(c9,c5), xDr3(c9,c8), canDrive(c10,c14), canDrive(c10,c11), canDrive(c10,c0), xDr1(c10,c14), xDr2(c10,c11), xDr3(c10,c0), canDrive(c11,c0), canDrive(c11,c3), canDrive(c11,c8), canDrive(c11,c10), canDrive(c11,c14), xDr1(c11,c0), xDr2(c11,c3), xDr3(c11,c8), canDrive(c12,c2), canDrive(c12,c9), canDrive(c12,c8), xDr1(c12,c2), xDr2(c12,c9), xDr3(c12,c8), canDrive(c13,c1), canDrive(c13,c4), canDrive(c13,c5), canDrive(c13,c6), canDrive(c13,c7), canDrive(c13,c9), xDr1(c13,c1), xDr2(c13,c4), xDr3(c13,c5), canDrive(c14,c10), canDrive(c14,c11), canDrive(c14,c0), xDr1(c14,c10), xDr2(c14,c11), xDr3(c14,c0)]).

goal(p06, [bin(b0,c1), bin(b1,c12), bin(b2,c7), bin(b3,c14), bin(b4,c8), bin(b5,c11), bin(b6,c1), bin(b7,c3), bin(b8,c5), bin(b9,c7)], 1.0).

cost(p06, []).






testDomain(p07, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14])]).

initState(p07, [bin(b0,c8), dest(b0,c1), bin(b1,c4), dest(b1,c12), bin(b2,c6), dest(b2,c7), bin(b3,c1), dest(b3,c14), bin(b4,c12), dest(b4,c8), bin(b5,c12), dest(b5,c11), bin(b6,c9), dest(b6,c1), bin(b7,c7), dest(b7,c3), bin(b8,c9), dest(b8,c5), bin(b9,c4), dest(b9,c7), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c11), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c10), canDrive(c0,c14), xDr1(c0,c11), xDr2(c0,c3), xDr3(c0,c8), canFly(c0,c1), canDrive(c1,c6), canDrive(c1,c7), canDrive(c1,c13), canDrive(c1,c4), xDr1(c1,c6), xDr2(c1,c7), xDr3(c1,c13), canFly(c1,c0), canDrive(c2,c12), canDrive(c2,c9), canDrive(c2,c8), xDr1(c2,c12), xDr2(c2,c9), xDr3(c2,c8), canDrive(c3,c0), canDrive(c3,c11), canDrive(c3,c8), xDr1(c3,c0), xDr2(c3,c11), xDr3(c3,c8), canDrive(c4,c5), canDrive(c4,c13), canDrive(c4,c1), xDr1(c4,c5), xDr2(c4,c13), xDr3(c4,c1), canDrive(c5,c4), canDrive(c5,c13), canDrive(c5,c9), xDr1(c5,c4), xDr2(c5,c13), xDr3(c5,c9), canDrive(c6,c1), canDrive(c6,c7), canDrive(c6,c13), xDr1(c6,c1), xDr2(c6,c7), xDr3(c6,c13), canDrive(c7,c1), canDrive(c7,c6), canDrive(c7,c13), xDr1(c7,c1), xDr2(c7,c6), xDr3(c7,c13), canDrive(c8,c0), canDrive(c8,c2), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c12), xDr1(c8,c0), xDr2(c8,c2), xDr3(c8,c3), canDrive(c9,c2), canDrive(c9,c5), canDrive(c9,c8), canDrive(c9,c13), canDrive(c9,c12), xDr1(c9,c2), xDr2(c9,c5), xDr3(c9,c8), canDrive(c10,c14), canDrive(c10,c11), canDrive(c10,c0), xDr1(c10,c14), xDr2(c10,c11), xDr3(c10,c0), canDrive(c11,c0), canDrive(c11,c3), canDrive(c11,c8), canDrive(c11,c10), canDrive(c11,c14), xDr1(c11,c0), xDr2(c11,c3), xDr3(c11,c8), canDrive(c12,c2), canDrive(c12,c9), canDrive(c12,c8), xDr1(c12,c2), xDr2(c12,c9), xDr3(c12,c8), canDrive(c13,c1), canDrive(c13,c4), canDrive(c13,c5), canDrive(c13,c6), canDrive(c13,c7), canDrive(c13,c9), xDr1(c13,c1), xDr2(c13,c4), xDr3(c13,c5), canDrive(c14,c10), canDrive(c14,c11), canDrive(c14,c0), xDr1(c14,c10), xDr2(c14,c11), xDr3(c14,c0)]).

goal(p07, [bin(b0,c1), bin(b1,c12), bin(b2,c7), bin(b3,c14), bin(b4,c8), bin(b5,c11), bin(b6,c1), bin(b7,c3), bin(b8,c5), bin(b9,c7)], 500.0).

cost(p07, [action(drive, -5.0), action(fly, -30.0), action(unloadt, 50.0), action(unloadp, 50.0)]).





testDomain(p08, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9])]).

initState(p08, [bin(b0,c0), dest(b0,c1), bin(b1,c5), dest(b1,c9), bin(b2,c1), dest(b2,c5), bin(b3,c6), dest(b3,c3), bin(b4,c3), dest(b4,c6), bin(b5,c3), dest(b5,c6), bin(b6,c4), dest(b6,c7), bin(b7,c6), dest(b7,c7), bin(b8,c1), dest(b8,c4), bin(b9,c2), dest(b9,c3), bin(b10,c7), dest(b10,c6), bin(b11,c4), dest(b11,c6), bin(b12,c2), dest(b12,c8), bin(b13,c9), dest(b13,c5), bin(b14,c9), dest(b14,c7), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c2), xDr1(c0,c3), xDr2(c0,c8), xDr3(c0,c2), canFly(c0,c1), canDrive(c1,c4), canDrive(c1,c7), canDrive(c1,c9), canDrive(c1,c5), xDr1(c1,c4), xDr2(c1,c7), xDr3(c1,c9), canFly(c1,c0), canDrive(c2,c0), canDrive(c2,c6), canDrive(c2,c3), canDrive(c2,c9), xDr1(c2,c0), xDr2(c2,c6), xDr3(c2,c3), canDrive(c3,c0), canDrive(c3,c2), canDrive(c3,c8), canDrive(c3,c6), xDr1(c3,c0), xDr2(c3,c2), xDr3(c3,c8), canDrive(c4,c1), canDrive(c4,c7), canDrive(c4,c9), canDrive(c4,c5), xDr1(c4,c1), xDr2(c4,c7), xDr3(c4,c9), canDrive(c5,c4), canDrive(c5,c1), canDrive(c5,c7), xDr1(c5,c4), xDr2(c5,c1), xDr3(c5,c7), canDrive(c6,c2), canDrive(c6,c9), canDrive(c6,c3), canDrive(c6,c8), xDr1(c6,c2), xDr2(c6,c9), xDr3(c6,c3), canDrive(c7,c1), canDrive(c7,c4), canDrive(c7,c5), canDrive(c7,c9), xDr1(c7,c1), xDr2(c7,c4), xDr3(c7,c5), canDrive(c8,c0), canDrive(c8,c3), canDrive(c8,c6), xDr1(c8,c0), xDr2(c8,c3), xDr3(c8,c6), canDrive(c9,c1), canDrive(c9,c2), canDrive(c9,c4), canDrive(c9,c6), canDrive(c9,c7), xDr1(c9,c1), xDr2(c9,c2), xDr3(c9,c4)]).

goal(p08, [bin(b0,c1), bin(b1,c9), bin(b2,c5), bin(b3,c3), bin(b4,c6), bin(b5,c6), bin(b6,c7), bin(b7,c7), bin(b8,c4), bin(b9,c3), bin(b10,c6), bin(b11,c6), bin(b12,c8), bin(b13,c5), bin(b14,c7)], 1.0).

cost(p08, []).






testDomain(p09, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9])]).

initState(p09, [bin(b0,c0), dest(b0,c1), bin(b1,c5), dest(b1,c9), bin(b2,c1), dest(b2,c5), bin(b3,c6), dest(b3,c3), bin(b4,c3), dest(b4,c6), bin(b5,c3), dest(b5,c6), bin(b6,c4), dest(b6,c7), bin(b7,c6), dest(b7,c7), bin(b8,c1), dest(b8,c4), bin(b9,c2), dest(b9,c3), bin(b10,c7), dest(b10,c6), bin(b11,c4), dest(b11,c6), bin(b12,c2), dest(b12,c8), bin(b13,c9), dest(b13,c5), bin(b14,c9), dest(b14,c7), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c2), xDr1(c0,c3), xDr2(c0,c8), xDr3(c0,c2), canFly(c0,c1), canDrive(c1,c4), canDrive(c1,c7), canDrive(c1,c9), canDrive(c1,c5), xDr1(c1,c4), xDr2(c1,c7), xDr3(c1,c9), canFly(c1,c0), canDrive(c2,c0), canDrive(c2,c6), canDrive(c2,c3), canDrive(c2,c9), xDr1(c2,c0), xDr2(c2,c6), xDr3(c2,c3), canDrive(c3,c0), canDrive(c3,c2), canDrive(c3,c8), canDrive(c3,c6), xDr1(c3,c0), xDr2(c3,c2), xDr3(c3,c8), canDrive(c4,c1), canDrive(c4,c7), canDrive(c4,c9), canDrive(c4,c5), xDr1(c4,c1), xDr2(c4,c7), xDr3(c4,c9), canDrive(c5,c4), canDrive(c5,c1), canDrive(c5,c7), xDr1(c5,c4), xDr2(c5,c1), xDr3(c5,c7), canDrive(c6,c2), canDrive(c6,c9), canDrive(c6,c3), canDrive(c6,c8), xDr1(c6,c2), xDr2(c6,c9), xDr3(c6,c3), canDrive(c7,c1), canDrive(c7,c4), canDrive(c7,c5), canDrive(c7,c9), xDr1(c7,c1), xDr2(c7,c4), xDr3(c7,c5), canDrive(c8,c0), canDrive(c8,c3), canDrive(c8,c6), xDr1(c8,c0), xDr2(c8,c3), xDr3(c8,c6), canDrive(c9,c1), canDrive(c9,c2), canDrive(c9,c4), canDrive(c9,c6), canDrive(c9,c7), xDr1(c9,c1), xDr2(c9,c2), xDr3(c9,c4)]).

goal(p09, [bin(b0,c1), bin(b1,c9), bin(b2,c5), bin(b3,c3), bin(b4,c6), bin(b5,c6), bin(b6,c7), bin(b7,c7), bin(b8,c4), bin(b9,c3), bin(b10,c6), bin(b11,c6), bin(b12,c8), bin(b13,c5), bin(b14,c7)], 500.0).

cost(p09, [action(drive, -5.0), action(fly, -25.0), action(unloadt, 100.0), action(unloadp, 100.0)]).






testDomain(p10, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14])]).

initState(p10, [bin(b0,c8), dest(b0,c1), bin(b1,c4), dest(b1,c12), bin(b2,c6), dest(b2,c7), bin(b3,c1), dest(b3,c14), bin(b4,c12), dest(b4,c8), bin(b5,c12), dest(b5,c11), bin(b6,c9), dest(b6,c1), bin(b7,c7), dest(b7,c3), bin(b8,c9), dest(b8,c5), bin(b9,c4), dest(b9,c7), bin(b10,c1), dest(b10,c6), bin(b11,c3), dest(b11,c11), bin(b12,c6), dest(b12,c1), bin(b13,c13), dest(b13,c2), bin(b14,c7), dest(b14,c11), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c11), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c10), canDrive(c0,c14), xDr1(c0,c11), xDr2(c0,c3), xDr3(c0,c8), canFly(c0,c1), canDrive(c1,c6), canDrive(c1,c7), canDrive(c1,c13), canDrive(c1,c4), xDr1(c1,c6), xDr2(c1,c7), xDr3(c1,c13), canFly(c1,c0), canDrive(c2,c12), canDrive(c2,c9), canDrive(c2,c8), xDr1(c2,c12), xDr2(c2,c9), xDr3(c2,c8), canDrive(c3,c0), canDrive(c3,c11), canDrive(c3,c8), xDr1(c3,c0), xDr2(c3,c11), xDr3(c3,c8), canDrive(c4,c5), canDrive(c4,c13), canDrive(c4,c1), xDr1(c4,c5), xDr2(c4,c13), xDr3(c4,c1), canDrive(c5,c4), canDrive(c5,c13), canDrive(c5,c9), xDr1(c5,c4), xDr2(c5,c13), xDr3(c5,c9), canDrive(c6,c1), canDrive(c6,c7), canDrive(c6,c13), xDr1(c6,c1), xDr2(c6,c7), xDr3(c6,c13), canDrive(c7,c1), canDrive(c7,c6), canDrive(c7,c13), xDr1(c7,c1), xDr2(c7,c6), xDr3(c7,c13), canDrive(c8,c0), canDrive(c8,c2), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c12), xDr1(c8,c0), xDr2(c8,c2), xDr3(c8,c3), canDrive(c9,c2), canDrive(c9,c5), canDrive(c9,c8), canDrive(c9,c13), canDrive(c9,c12), xDr1(c9,c2), xDr2(c9,c5), xDr3(c9,c8), canDrive(c10,c14), canDrive(c10,c11), canDrive(c10,c0), xDr1(c10,c14), xDr2(c10,c11), xDr3(c10,c0), canDrive(c11,c0), canDrive(c11,c3), canDrive(c11,c8), canDrive(c11,c10), canDrive(c11,c14), xDr1(c11,c0), xDr2(c11,c3), xDr3(c11,c8), canDrive(c12,c2), canDrive(c12,c9), canDrive(c12,c8), xDr1(c12,c2), xDr2(c12,c9), xDr3(c12,c8), canDrive(c13,c1), canDrive(c13,c4), canDrive(c13,c5), canDrive(c13,c6), canDrive(c13,c7), canDrive(c13,c9), xDr1(c13,c1), xDr2(c13,c4), xDr3(c13,c5), canDrive(c14,c10), canDrive(c14,c11), canDrive(c14,c0), xDr1(c14,c10), xDr2(c14,c11), xDr3(c14,c0)]).

goal(p10, [bin(b0,c1), bin(b1,c12), bin(b2,c7), bin(b3,c14), bin(b4,c8), bin(b5,c11), bin(b6,c1), bin(b7,c3), bin(b8,c5), bin(b9,c7), bin(b10,c6), bin(b11,c11), bin(b12,c1), bin(b13,c2), bin(b14,c11)], 1.0).

cost(p10, []).




testDomain(p11, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14])]).

initState(p11, [bin(b0,c8), dest(b0,c1), bin(b1,c4), dest(b1,c12), bin(b2,c6), dest(b2,c7), bin(b3,c1), dest(b3,c14), bin(b4,c12), dest(b4,c8), bin(b5,c12), dest(b5,c11), bin(b6,c9), dest(b6,c1), bin(b7,c7), dest(b7,c3), bin(b8,c9), dest(b8,c5), bin(b9,c4), dest(b9,c7), bin(b10,c1), dest(b10,c6), bin(b11,c3), dest(b11,c11), bin(b12,c6), dest(b12,c1), bin(b13,c13), dest(b13,c2), bin(b14,c7), dest(b14,c11), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c11), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c10), canDrive(c0,c14), xDr1(c0,c11), xDr2(c0,c3), xDr3(c0,c8), canFly(c0,c1), canDrive(c1,c6), canDrive(c1,c7), canDrive(c1,c13), canDrive(c1,c4), xDr1(c1,c6), xDr2(c1,c7), xDr3(c1,c13), canFly(c1,c0), canDrive(c2,c12), canDrive(c2,c9), canDrive(c2,c8), xDr1(c2,c12), xDr2(c2,c9), xDr3(c2,c8), canDrive(c3,c0), canDrive(c3,c11), canDrive(c3,c8), xDr1(c3,c0), xDr2(c3,c11), xDr3(c3,c8), canDrive(c4,c5), canDrive(c4,c13), canDrive(c4,c1), xDr1(c4,c5), xDr2(c4,c13), xDr3(c4,c1), canDrive(c5,c4), canDrive(c5,c13), canDrive(c5,c9), xDr1(c5,c4), xDr2(c5,c13), xDr3(c5,c9), canDrive(c6,c1), canDrive(c6,c7), canDrive(c6,c13), xDr1(c6,c1), xDr2(c6,c7), xDr3(c6,c13), canDrive(c7,c1), canDrive(c7,c6), canDrive(c7,c13), xDr1(c7,c1), xDr2(c7,c6), xDr3(c7,c13), canDrive(c8,c0), canDrive(c8,c2), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c12), xDr1(c8,c0), xDr2(c8,c2), xDr3(c8,c3), canDrive(c9,c2), canDrive(c9,c5), canDrive(c9,c8), canDrive(c9,c13), canDrive(c9,c12), xDr1(c9,c2), xDr2(c9,c5), xDr3(c9,c8), canDrive(c10,c14), canDrive(c10,c11), canDrive(c10,c0), xDr1(c10,c14), xDr2(c10,c11), xDr3(c10,c0), canDrive(c11,c0), canDrive(c11,c3), canDrive(c11,c8), canDrive(c11,c10), canDrive(c11,c14), xDr1(c11,c0), xDr2(c11,c3), xDr3(c11,c8), canDrive(c12,c2), canDrive(c12,c9), canDrive(c12,c8), xDr1(c12,c2), xDr2(c12,c9), xDr3(c12,c8), canDrive(c13,c1), canDrive(c13,c4), canDrive(c13,c5), canDrive(c13,c6), canDrive(c13,c7), canDrive(c13,c9), xDr1(c13,c1), xDr2(c13,c4), xDr3(c13,c5), canDrive(c14,c10), canDrive(c14,c11), canDrive(c14,c0), xDr1(c14,c10), xDr2(c14,c11), xDr3(c14,c0)]).

goal(p11, [bin(b0,c1), bin(b1,c12), bin(b2,c7), bin(b3,c14), bin(b4,c8), bin(b5,c11), bin(b6,c1), bin(b7,c3), bin(b8,c5), bin(b9,c7), bin(b10,c6), bin(b11,c11), bin(b12,c1), bin(b13,c2), bin(b14,c11)], 0.0).

cost(p11, [action(unloadt, 1.0), action(unloadp, 1.0)]).






testDomain(p12, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14])]).

initState(p12, [bin(b0,c8), dest(b0,c1), bin(b1,c4), dest(b1,c12), bin(b2,c6), dest(b2,c7), bin(b3,c1), dest(b3,c14), bin(b4,c12), dest(b4,c8), bin(b5,c12), dest(b5,c11), bin(b6,c9), dest(b6,c1), bin(b7,c7), dest(b7,c3), bin(b8,c9), dest(b8,c5), bin(b9,c4), dest(b9,c7), bin(b10,c1), dest(b10,c6), bin(b11,c3), dest(b11,c11), bin(b12,c6), dest(b12,c1), bin(b13,c13), dest(b13,c2), bin(b14,c7), dest(b14,c11), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c11), canDrive(c0,c3), canDrive(c0,c8), canDrive(c0,c10), canDrive(c0,c14), xDr1(c0,c11), xDr2(c0,c3), xDr3(c0,c8), canFly(c0,c1), canDrive(c1,c6), canDrive(c1,c7), canDrive(c1,c13), canDrive(c1,c4), xDr1(c1,c6), xDr2(c1,c7), xDr3(c1,c13), canFly(c1,c0), canDrive(c2,c12), canDrive(c2,c9), canDrive(c2,c8), xDr1(c2,c12), xDr2(c2,c9), xDr3(c2,c8), canDrive(c3,c0), canDrive(c3,c11), canDrive(c3,c8), xDr1(c3,c0), xDr2(c3,c11), xDr3(c3,c8), canDrive(c4,c5), canDrive(c4,c13), canDrive(c4,c1), xDr1(c4,c5), xDr2(c4,c13), xDr3(c4,c1), canDrive(c5,c4), canDrive(c5,c13), canDrive(c5,c9), xDr1(c5,c4), xDr2(c5,c13), xDr3(c5,c9), canDrive(c6,c1), canDrive(c6,c7), canDrive(c6,c13), xDr1(c6,c1), xDr2(c6,c7), xDr3(c6,c13), canDrive(c7,c1), canDrive(c7,c6), canDrive(c7,c13), xDr1(c7,c1), xDr2(c7,c6), xDr3(c7,c13), canDrive(c8,c0), canDrive(c8,c2), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c12), xDr1(c8,c0), xDr2(c8,c2), xDr3(c8,c3), canDrive(c9,c2), canDrive(c9,c5), canDrive(c9,c8), canDrive(c9,c13), canDrive(c9,c12), xDr1(c9,c2), xDr2(c9,c5), xDr3(c9,c8), canDrive(c10,c14), canDrive(c10,c11), canDrive(c10,c0), xDr1(c10,c14), xDr2(c10,c11), xDr3(c10,c0), canDrive(c11,c0), canDrive(c11,c3), canDrive(c11,c8), canDrive(c11,c10), canDrive(c11,c14), xDr1(c11,c0), xDr2(c11,c3), xDr3(c11,c8), canDrive(c12,c2), canDrive(c12,c9), canDrive(c12,c8), xDr1(c12,c2), xDr2(c12,c9), xDr3(c12,c8), canDrive(c13,c1), canDrive(c13,c4), canDrive(c13,c5), canDrive(c13,c6), canDrive(c13,c7), canDrive(c13,c9), xDr1(c13,c1), xDr2(c13,c4), xDr3(c13,c5), canDrive(c14,c10), canDrive(c14,c11), canDrive(c14,c0), xDr1(c14,c10), xDr2(c14,c11), xDr3(c14,c0)]).

goal(p12, [bin(b0,c1), bin(b1,c12), bin(b2,c7), bin(b3,c14), bin(b4,c8), bin(b5,c11), bin(b6,c1), bin(b7,c3), bin(b8,c5), bin(b9,c7), bin(b10,c6), bin(b11,c11), bin(b12,c1), bin(b13,c2), bin(b14,c11)], 500.0).

cost(p12, [action(drive, -5.0), action(fly, -25.0), action(unloadt, 100.0), action(unloadp, 100.0)]).





testDomain(p13, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19])]).


initState(p13, [bin(b0,c17), dest(b0,c16), bin(b1,c14), dest(b1,c6), bin(b2,c12), dest(b2,c18), bin(b3,c9), dest(b3,c15), bin(b4,c9), dest(b4,c7), bin(b5,c11), dest(b5,c1), bin(b6,c18), dest(b6,c16), bin(b7,c1), dest(b7,c11), bin(b8,c13), dest(b8,c17), bin(b9,c7), dest(b9,c6), bin(b10,c5), dest(b10,c0), bin(b11,c15), dest(b11,c19), bin(b12,c19), dest(b12,c3), bin(b13,c8), dest(b13,c2), bin(b14,c9), dest(b14,c1), bin(b15,c5), dest(b15,c6), bin(b16,c17), dest(b16,c11), bin(b17,c5), dest(b17,c1), bin(b18,c9), dest(b18,c6), bin(b19,c8), dest(b19,c10), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c10), canDrive(c0,c13), canDrive(c0,c1), canDrive(c0,c2), xDr1(c0,c10), xDr2(c0,c13), xDr3(c0,c1), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c9), canDrive(c1,c11), xDr1(c1,c0), xDr2(c1,c9), xDr3(c1,c11), canFly(c1,c0), canDrive(c2,c19), canDrive(c2,c0), canDrive(c2,c10), xDr1(c2,c19), xDr2(c2,c0), xDr3(c2,c10), canDrive(c3,c8), canDrive(c3,c14), canDrive(c3,c9), xDr1(c3,c8), xDr2(c3,c14), xDr3(c3,c9), canDrive(c4,c17), canDrive(c4,c6), canDrive(c4,c18), xDr1(c4,c17), xDr2(c4,c6), xDr3(c4,c18), canDrive(c5,c18), canDrive(c5,c16), canDrive(c5,c17), canDrive(c5,c7), xDr1(c5,c18), xDr2(c5,c16), xDr3(c5,c17), canDrive(c6,c4), canDrive(c6,c16), canDrive(c6,c17), canDrive(c6,c12), xDr1(c6,c4), xDr2(c6,c16), xDr3(c6,c17), canDrive(c7,c19), canDrive(c7,c5), canDrive(c7,c18), xDr1(c7,c19), xDr2(c7,c5), xDr3(c7,c18), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c14), canDrive(c8,c15), xDr1(c8,c3), xDr2(c8,c11), xDr3(c8,c9), canDrive(c9,c1), canDrive(c9,c3), canDrive(c9,c8), canDrive(c9,c14), canDrive(c9,c11), xDr1(c9,c1), xDr2(c9,c3), xDr3(c9,c8), canDrive(c10,c0), canDrive(c10,c2), canDrive(c10,c13), canDrive(c10,c19), xDr1(c10,c0), xDr2(c10,c2), xDr3(c10,c13), canDrive(c11,c1), canDrive(c11,c8), canDrive(c11,c9), canDrive(c11,c15), xDr1(c11,c1), xDr2(c11,c8), xDr3(c11,c9), canDrive(c12,c15), canDrive(c12,c6), canDrive(c12,c16), xDr1(c12,c15), xDr2(c12,c6), xDr3(c12,c16), canDrive(c13,c0), canDrive(c13,c10), canDrive(c13,c16), xDr1(c13,c0), xDr2(c13,c10), xDr3(c13,c16), canDrive(c14,c3), canDrive(c14,c9), canDrive(c14,c8), xDr1(c14,c3), xDr2(c14,c9), xDr3(c14,c8), canDrive(c15,c12), canDrive(c15,c11), canDrive(c15,c8), xDr1(c15,c12), xDr2(c15,c11), xDr3(c15,c8), canDrive(c16,c5), canDrive(c16,c6), canDrive(c16,c12), canDrive(c16,c13), canDrive(c16,c18), xDr1(c16,c5), xDr2(c16,c6), xDr3(c16,c12), canDrive(c17,c4), canDrive(c17,c5), canDrive(c17,c6), canDrive(c17,c18), xDr1(c17,c4), xDr2(c17,c5), xDr3(c17,c6), canDrive(c18,c4), canDrive(c18,c5), canDrive(c18,c7), canDrive(c18,c16), canDrive(c18,c17), xDr1(c18,c4), xDr2(c18,c5), xDr3(c18,c7), canDrive(c19,c2), canDrive(c19,c7), canDrive(c19,c10), xDr1(c19,c2), xDr2(c19,c7), xDr3(c19,c10)]).

goal(p13, [bin(b0,c16), bin(b1,c6), bin(b2,c18), bin(b3,c15), bin(b4,c7), bin(b5,c1), bin(b6,c16), bin(b7,c11), bin(b8,c17), bin(b9,c6), bin(b10,c0), bin(b11,c19), bin(b12,c3), bin(b13,c2), bin(b14,c1), bin(b15,c6), bin(b16,c11), bin(b17,c1), bin(b18,c6), bin(b19,c10)], 1.0).

cost(p13, []).







testDomain(p14, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19])]).

initState(p14, [bin(b0,c17), dest(b0,c16), bin(b1,c14), dest(b1,c6), bin(b2,c12), dest(b2,c18), bin(b3,c9), dest(b3,c15), bin(b4,c9), dest(b4,c7), bin(b5,c11), dest(b5,c1), bin(b6,c18), dest(b6,c16), bin(b7,c1), dest(b7,c11), bin(b8,c13), dest(b8,c17), bin(b9,c7), dest(b9,c6), bin(b10,c5), dest(b10,c0), bin(b11,c15), dest(b11,c19), bin(b12,c19), dest(b12,c3), bin(b13,c8), dest(b13,c2), bin(b14,c9), dest(b14,c1), bin(b15,c5), dest(b15,c6), bin(b16,c17), dest(b16,c11), bin(b17,c5), dest(b17,c1), bin(b18,c9), dest(b18,c6), bin(b19,c8), dest(b19,c10), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c10), canDrive(c0,c13), canDrive(c0,c1), canDrive(c0,c2), xDr1(c0,c10), xDr2(c0,c13), xDr3(c0,c1), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c9), canDrive(c1,c11), xDr1(c1,c0), xDr2(c1,c9), xDr3(c1,c11), canFly(c1,c0), canDrive(c2,c19), canDrive(c2,c0), canDrive(c2,c10), xDr1(c2,c19), xDr2(c2,c0), xDr3(c2,c10), canDrive(c3,c8), canDrive(c3,c14), canDrive(c3,c9), xDr1(c3,c8), xDr2(c3,c14), xDr3(c3,c9), canDrive(c4,c17), canDrive(c4,c6), canDrive(c4,c18), xDr1(c4,c17), xDr2(c4,c6), xDr3(c4,c18), canDrive(c5,c18), canDrive(c5,c16), canDrive(c5,c17), canDrive(c5,c7), xDr1(c5,c18), xDr2(c5,c16), xDr3(c5,c17), canDrive(c6,c4), canDrive(c6,c16), canDrive(c6,c17), canDrive(c6,c12), xDr1(c6,c4), xDr2(c6,c16), xDr3(c6,c17), canDrive(c7,c19), canDrive(c7,c5), canDrive(c7,c18), xDr1(c7,c19), xDr2(c7,c5), xDr3(c7,c18), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c14), canDrive(c8,c15), xDr1(c8,c3), xDr2(c8,c11), xDr3(c8,c9), canDrive(c9,c1), canDrive(c9,c3), canDrive(c9,c8), canDrive(c9,c14), canDrive(c9,c11), xDr1(c9,c1), xDr2(c9,c3), xDr3(c9,c8), canDrive(c10,c0), canDrive(c10,c2), canDrive(c10,c13), canDrive(c10,c19), xDr1(c10,c0), xDr2(c10,c2), xDr3(c10,c13), canDrive(c11,c1), canDrive(c11,c8), canDrive(c11,c9), canDrive(c11,c15), xDr1(c11,c1), xDr2(c11,c8), xDr3(c11,c9), canDrive(c12,c15), canDrive(c12,c6), canDrive(c12,c16), xDr1(c12,c15), xDr2(c12,c6), xDr3(c12,c16), canDrive(c13,c0), canDrive(c13,c10), canDrive(c13,c16), xDr1(c13,c0), xDr2(c13,c10), xDr3(c13,c16), canDrive(c14,c3), canDrive(c14,c9), canDrive(c14,c8), xDr1(c14,c3), xDr2(c14,c9), xDr3(c14,c8), canDrive(c15,c12), canDrive(c15,c11), canDrive(c15,c8), xDr1(c15,c12), xDr2(c15,c11), xDr3(c15,c8), canDrive(c16,c5), canDrive(c16,c6), canDrive(c16,c12), canDrive(c16,c13), canDrive(c16,c18), xDr1(c16,c5), xDr2(c16,c6), xDr3(c16,c12), canDrive(c17,c4), canDrive(c17,c5), canDrive(c17,c6), canDrive(c17,c18), xDr1(c17,c4), xDr2(c17,c5), xDr3(c17,c6), canDrive(c18,c4), canDrive(c18,c5), canDrive(c18,c7), canDrive(c18,c16), canDrive(c18,c17), xDr1(c18,c4), xDr2(c18,c5), xDr3(c18,c7), canDrive(c19,c2), canDrive(c19,c7), canDrive(c19,c10), xDr1(c19,c2), xDr2(c19,c7), xDr3(c19,c10)]).

goal(p14, [bin(b0,c16), bin(b1,c6), bin(b2,c18), bin(b3,c15), bin(b4,c7), bin(b5,c1), bin(b6,c16), bin(b7,c11), bin(b8,c17), bin(b9,c6), bin(b10,c0), bin(b11,c19), bin(b12,c3), bin(b13,c2), bin(b14,c1), bin(b15,c6), bin(b16,c11), bin(b17,c1), bin(b18,c6), bin(b19,c10)], 20.0).

cost(p14, [action(unloadt, 1.0), action(unloadp, 1.0)]).







testDomain(p15, [objects(box, [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19]), objects(truck, [t0, t1, t2, t3]), objects(plane, [p0, p1]), objects(city, [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19])]).

initState(p15, [bin(b0,c17), dest(b0,c16), bin(b1,c14), dest(b1,c6), bin(b2,c12), dest(b2,c18), bin(b3,c9), dest(b3,c15), bin(b4,c9), dest(b4,c7), bin(b5,c11), dest(b5,c1), bin(b6,c18), dest(b6,c16), bin(b7,c1), dest(b7,c11), bin(b8,c13), dest(b8,c17), bin(b9,c7), dest(b9,c6), bin(b10,c5), dest(b10,c0), bin(b11,c15), dest(b11,c19), bin(b12,c19), dest(b12,c3), bin(b13,c8), dest(b13,c2), bin(b14,c9), dest(b14,c1), bin(b15,c5), dest(b15,c6), bin(b16,c17), dest(b16,c11), bin(b17,c5), dest(b17,c1), bin(b18,c9), dest(b18,c6), bin(b19,c8), dest(b19,c10), tin(t0,c0), tin(t1,c0), pin(p0,c0), tin(t2,c1), tin(t3,c1), pin(p1,c1), canDrive(c0,c10), canDrive(c0,c13), canDrive(c0,c1), canDrive(c0,c2), xDr1(c0,c10), xDr2(c0,c13), xDr3(c0,c1), canFly(c0,c1), canDrive(c1,c0), canDrive(c1,c9), canDrive(c1,c11), xDr1(c1,c0), xDr2(c1,c9), xDr3(c1,c11), canFly(c1,c0), canDrive(c2,c19), canDrive(c2,c0), canDrive(c2,c10), xDr1(c2,c19), xDr2(c2,c0), xDr3(c2,c10), canDrive(c3,c8), canDrive(c3,c14), canDrive(c3,c9), xDr1(c3,c8), xDr2(c3,c14), xDr3(c3,c9), canDrive(c4,c17), canDrive(c4,c6), canDrive(c4,c18), xDr1(c4,c17), xDr2(c4,c6), xDr3(c4,c18), canDrive(c5,c18), canDrive(c5,c16), canDrive(c5,c17), canDrive(c5,c7), xDr1(c5,c18), xDr2(c5,c16), xDr3(c5,c17), canDrive(c6,c4), canDrive(c6,c16), canDrive(c6,c17), canDrive(c6,c12), xDr1(c6,c4), xDr2(c6,c16), xDr3(c6,c17), canDrive(c7,c19), canDrive(c7,c5), canDrive(c7,c18), xDr1(c7,c19), xDr2(c7,c5), xDr3(c7,c18), canDrive(c8,c3), canDrive(c8,c11), canDrive(c8,c9), canDrive(c8,c14), canDrive(c8,c15), xDr1(c8,c3), xDr2(c8,c11), xDr3(c8,c9), canDrive(c9,c1), canDrive(c9,c3), canDrive(c9,c8), canDrive(c9,c14), canDrive(c9,c11), xDr1(c9,c1), xDr2(c9,c3), xDr3(c9,c8), canDrive(c10,c0), canDrive(c10,c2), canDrive(c10,c13), canDrive(c10,c19), xDr1(c10,c0), xDr2(c10,c2), xDr3(c10,c13), canDrive(c11,c1), canDrive(c11,c8), canDrive(c11,c9), canDrive(c11,c15), xDr1(c11,c1), xDr2(c11,c8), xDr3(c11,c9), canDrive(c12,c15), canDrive(c12,c6), canDrive(c12,c16), xDr1(c12,c15), xDr2(c12,c6), xDr3(c12,c16), canDrive(c13,c0), canDrive(c13,c10), canDrive(c13,c16), xDr1(c13,c0), xDr2(c13,c10), xDr3(c13,c16), canDrive(c14,c3), canDrive(c14,c9), canDrive(c14,c8), xDr1(c14,c3), xDr2(c14,c9), xDr3(c14,c8), canDrive(c15,c12), canDrive(c15,c11), canDrive(c15,c8), xDr1(c15,c12), xDr2(c15,c11), xDr3(c15,c8), canDrive(c16,c5), canDrive(c16,c6), canDrive(c16,c12), canDrive(c16,c13), canDrive(c16,c18), xDr1(c16,c5), xDr2(c16,c6), xDr3(c16,c12), canDrive(c17,c4), canDrive(c17,c5), canDrive(c17,c6), canDrive(c17,c18), xDr1(c17,c4), xDr2(c17,c5), xDr3(c17,c6), canDrive(c18,c4), canDrive(c18,c5), canDrive(c18,c7), canDrive(c18,c16), canDrive(c18,c17), xDr1(c18,c4), xDr2(c18,c5), xDr3(c18,c7), canDrive(c19,c2), canDrive(c19,c7), canDrive(c19,c10), xDr1(c19,c2), xDr2(c19,c7), xDr3(c19,c10)]).

goal(p15, [bin(b0,c16), bin(b1,c6), bin(b2,c18), bin(b3,c15), bin(b4,c7), bin(b5,c1), bin(b6,c16), bin(b7,c11), bin(b8,c17), bin(b9,c6), bin(b10,c0), bin(b11,c19), bin(b12,c3), bin(b13,c2), bin(b14,c1), bin(b15,c6), bin(b16,c11), bin(b17,c1), bin(b18,c6), bin(b19,c10)], 500.0).

cost(p15, [action(drive, -5.0), action(fly, -25.0), action(unloadt, 100.0), action(unloadp, 100.0)]).



