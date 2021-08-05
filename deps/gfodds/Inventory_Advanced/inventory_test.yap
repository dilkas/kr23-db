problems([p01]).

testDomain(p01, [objects(location, [s1,s2, s3, s4, d]), objects(truck, [t1]), objects(maxobj, [a,a1])]).

initState(p01, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), freq(s1), freq(s2), tin(t1,d), tfull(t1), level0(s1), level0(s2), level1(s3), level0(s4), empty(d)]).
horizon(p01,10).



testDomain(p02, [objects(location, [s1,s2, s3, s4, s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,d]), objects(truck, [t1]), objects(maxobj, [a,a1])]).

initState(p02, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), shop(s10), shop(s11), shop(s12), shop(s13), shop(s14), shop(s15), tin(t1,s1), empty(s1), empty(s2), empty(s3), empty(s4), empty(s5), empty(s6), empty(s7), empty(s8), empty(s9), empty(s10), empty(s11), empty(s12), empty(s13), empty(s14), empty(s15), empty(d)]).

horizon(p02,10).



