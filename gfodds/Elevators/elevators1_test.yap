%problems([p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15]).
problems([p1]).


testDomain(p1, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p1, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pup(bottom),pup(f2),pup(f3),pinup,pdown(f2),pdown(f3)]).

horizon(p1,20).



testDomain(p2, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p2, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(bottom),pdown(f2)]).

horizon(p2,20).



testDomain(p3, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p3, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pup(f3),pindown,pinup,pdown(f1),pdown(f3),pdown(top)]).

horizon(p3,20).



testDomain(p4, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p4, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(bottom),pup(f1),pup(f2),pup(f3),pinup,pdown(f3)]).

horizon(p4,20).



testDomain(p5, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p5, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(f2),pindown,pdown(f1),pdown(f3),pdown(top)]).

horizon(p5,20).



testDomain(p6, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p6, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pindown,pdown(f3)]).

horizon(p6,20).



testDomain(p7, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p7, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pup(f1),pup(f2),pindown,pdown(f1),pdown(f2),pdown(top)]).

horizon(p7,20).



testDomain(p8, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p8, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(bottom),pup(f2),pup(f3),pindown,pdown(f1),pdown(f2)]).

horizon(p8,20).



testDomain(p9, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p9, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pup(bottom),pup(f1),pup(f2),pdown(f1)]).

horizon(p9,20).



testDomain(p10, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p10, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(bottom),pup(f1),pup(f2),pup(f3),pdown(f2),pdown(f3),pdown(top)]).

horizon(p10,20).



testDomain(p11, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p11, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(bottom),pup(f1),pup(f2),pindown,pdown(f1),pdown(f2),pdown(top)]).

horizon(p11,20).



testDomain(p12, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p12, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pup(bottom),pup(f3),pindown,pdown(f3)]).

horizon(p12,20).



testDomain(p13, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p13, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),up,at(f1),pindown,pinup,pdown(f1),pdown(f2),pdown(f3),pdown(top)]).

horizon(p13,20).



testDomain(p14, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p14, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(f1),pup(f3),pdown(f1),pdown(f2)]).

horizon(p14,20).



testDomain(p15, [objects(maxobj,[a,a1]),objects(floor,[bottom,f1,f2,f3,top])]).

initState(p15, [above(top,bottom),above(top,f1),above(f1,bottom),above(top,f2),above(f2,f1),above(f2,bottom),above(top,f3),above(f3,f2),above(f3,f1),above(f3,bottom),at(f1),pup(f1),pup(f3),pindown,pinup,pdown(f1),pdown(f2),pdown(top)]).

horizon(p15,20).



