problems([p1]).


testDomain(p1, [objects(maxobj,[a,a1]),objects(floor,[bottom,top]),objects(person,[])]).

initState(p1, [above(top,bottom),up,at(top)]).

horizon(p1,20).
