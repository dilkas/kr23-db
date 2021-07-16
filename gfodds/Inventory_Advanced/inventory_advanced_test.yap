%problems([p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15]).
problems([p1]).

testDomain(p1, [objects(maxobj,[a,a1]),objects(location,[l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,d]),objects(truck,[t1])]).

initState(p1, [depot(d),shop(l1),shop(l2),shop(l3),shop(l4),shop(l6),shop(l5),shop(l7),shop(l8),shop(l9),shop(l15),shop(l18),shop(l20),shop(l19),shop(l17),shop(l16),shop(l14),shop(l10),shop(l11),shop(l12),shop(l13),freq(l1),freq(l6),freq(l7),tfull(t1),level0(l1),level0(l6),level1(l9),level0(l13),level1(l15),level0(d),tin(t1,l1)]).

horizon(p1,40).



testDomain(p2, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p2, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p2,40).



testDomain(p3, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p3, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p3,40).



testDomain(p4, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p4, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p4,40).



testDomain(p5, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p5, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p5,40).



testDomain(p6, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p6, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p6,40).



testDomain(p7, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p7, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p7,40).



testDomain(p8, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p8, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p8,40).



testDomain(p9, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p9, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p9,40).



testDomain(p10, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p10, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p10,40).



testDomain(p11, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p11, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p11,40).



testDomain(p12, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p12, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p12,40).



testDomain(p13, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p13, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p13,40).



testDomain(p14, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p14, [depot(d),shop(l1),freq(l1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p14,40).



testDomain(p15, [objects(maxobj,[a,a1]),objects(location,[l1,d]),objects(truck,[t1])]).

initState(p15, [depot(d),shop(l1),freq(l1),tfull(t1),level0(l1),level0(d),tin(t1,l1)]).

horizon(p15,40).



