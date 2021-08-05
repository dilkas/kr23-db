problems([p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,p131,p132,p133,p134,p135,p136,p137,p138,p139,p140,p141,p142,p143,p144,p145,p146,p147,p148,p149,p150]).




testDomain(p121, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p121, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), freq(s5), level1(s2), level1(s1), tin(t1,s2), level1(s4), level1(s5), freq(s1), freq(s3), freq(s2)]).

horizon(p121,40).



testDomain(p122, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p122, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), freq(s4), freq(s5), level1(s3), level1(s5), freq(d), tin(t1,s4), level0(s1), level0(s2), freq(s2)]).

horizon(p122,40).



testDomain(p123, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p123, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), level0(s5), freq(s4), level1(s3), tin(t1,d), freq(d), level0(s2)]).

horizon(p123,40).



testDomain(p124, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p124, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), freq(s5), tin(t1,s1), freq(d), level0(s1), freq(s1), freq(s2)]).

horizon(p124,40).



testDomain(p125, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p125, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), level1(s2), level1(s1), level1(s4), tin(t1,s4), level0(s3), freq(s3)]).

horizon(p125,40).



testDomain(p126, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p126, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), tfull(t1), tin(t1,s4), level0(s1), freq(s1), freq(s3), freq(s2)]).

horizon(p126,40).



testDomain(p127, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p127, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s5), freq(s4), freq(s5), level1(s1), tin(t1,s2), freq(s1), freq(s2)]).

horizon(p127,40).



testDomain(p128, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p128, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), level1(s2), level1(s4), tin(t1,s1), freq(d), level0(s3), freq(s3)]).

horizon(p128,40).



testDomain(p129, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p129, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s5), tfull(t1), level1(s4), tin(t1,s1), freq(d), level0(s3), freq(s2)]).

horizon(p129,40).



testDomain(p130, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p130, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), level1(s1), tfull(t1), level1(s4), tin(t1,s1), freq(d), level0(s3), level0(s2), freq(s1), freq(s3)]).

horizon(p130,40).



testDomain(p131, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p131, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), level0(s5), freq(s5), level1(s1), freq(d), tin(t1,s5), level0(s3)]).

horizon(p131,40).



testDomain(p132, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p132, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s5), level1(s2), level1(s1), level1(s4), freq(d), tin(t1,s4), level0(s3), freq(s1), freq(s3), freq(s2)]).

horizon(p132,40).



testDomain(p133, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p133, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level1(s2), tfull(t1), tin(t1,s1), level1(s5), freq(d), level0(s3), freq(s1)]).

horizon(p133,40).



testDomain(p134, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p134, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), freq(s4), level1(s1), tin(t1,s2), level1(s5), level0(s3), freq(s2)]).

horizon(p134,40).



testDomain(p135, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p135, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), freq(s4), level1(s1), freq(d), tin(t1,s3), level0(s2), freq(s3), freq(s2)]).

horizon(p135,40).



testDomain(p136, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p136, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), freq(s4), tfull(t1), level1(s5), tin(t1,s4), level0(s1), level0(s3)]).

horizon(p136,40).



testDomain(p137, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p137, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), freq(s5), level1(s2), level1(s1), tfull(t1), tin(t1,s1), level0(s3), freq(s1), freq(s2)]).

horizon(p137,40).



testDomain(p138, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p138, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), level0(s5), level1(s2), tin(t1,s2), level0(s3), freq(s1)]).

horizon(p138,40).



testDomain(p139, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p139, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), level0(s5), freq(s5), level1(s3), level1(s2), tin(t1,s2), freq(d), freq(s3)]).

horizon(p139,40).



testDomain(p140, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p140, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s5), level1(s2), tfull(t1), tin(t1,s2), level1(s5), freq(d)]).

horizon(p140,40).



testDomain(p141, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p141, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), freq(s5), level1(s4), level1(s5), freq(d), tin(t1,s5), level0(s3), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p141,40).



testDomain(p142, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p142, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s5), tin(t1,s2), level1(s4), level1(s5), freq(d), level0(s3), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p142,40).



testDomain(p143, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p143, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), freq(s5), level1(s2), level1(s1), tin(t1,s2), level1(s5), level0(s3), freq(s1), freq(s3), freq(s2)]).

horizon(p143,40).



testDomain(p144, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p144, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), freq(s4), level1(s2), tfull(t1), tin(t1,s2), level1(s4), freq(d), level0(s1), freq(s3), freq(s2)]).

horizon(p144,40).



testDomain(p145, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p145, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s5), level1(s1), freq(d), tin(t1,s3), level0(s2), freq(s1), freq(s3)]).

horizon(p145,40).



testDomain(p146, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p146, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), tin(t1,d), level1(s1), tfull(t1), level0(s2)]).

horizon(p146,40).



testDomain(p147, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p147, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), freq(s5), tin(t1,s3), freq(s1), freq(s3), freq(s2)]).

horizon(p147,40).



testDomain(p148, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p148, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s4), tfull(t1), tin(t1,s2), level1(s5), freq(d), level0(s1), level0(s2), freq(s1), freq(s2)]).

horizon(p148,40).



testDomain(p149, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p149, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s5), freq(s4), freq(s5), level1(s2), tin(t1,d), level1(s1), freq(s1)]).

horizon(p149,40).



testDomain(p150, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p150, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), level0(d), level0(s5), level1(s3), level1(s2), level1(s4), tin(t1,s5)]).

horizon(p150,40).





