problems([p1]).


testDomain(p1, [objects(person,[]),objects(maxobj,[a,a1]),objects(location,[l1,l2,d]),objects(truck,[t1])]).

initState(p1, [depot(d),shop(l1),shop(l2),tfull(t1),empty(l1),tin(t1,l1)]).

horizon(p1,40).
