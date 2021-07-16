problems([p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30]). 
%problems([p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60]).
%problems([p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p90]).
%problems([p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,p101,p102,p103,p104,p105,p106,p107,p108,p109,p110,p111,p112,p113,p114,p115,p116,p117,p118,p119,p120]).
%problems([p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,p131,p132,p133,p134,p135,p136,p137,p138,p139,p140,p141,p142,p143,p144,p145,p146,p147,p148,p149,p150]).

%problems([p151]).


testDomain(p1, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p1, [depot(d), shop(s1), tin(t1,s1), level0(d), freq(d), freq(s1), tfull(t1)]).

horizon(p1,40).



testDomain(p2, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p2, [depot(d), shop(s1), level0(d), tin(t1,d), level1(s1)]).

horizon(p2,40).



testDomain(p3, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p3, [depot(d), shop(s1), level0(d), tin(t1,d), tfull(t1)]).

horizon(p3,40).



testDomain(p4, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p4, [depot(d), shop(s1), tin(t1,s1), level0(d), freq(d), tfull(t1)]).

horizon(p4,40).



testDomain(p5, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p5, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), tfull(t1)]).

horizon(p5,40).



testDomain(p6, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p6, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), tfull(t1)]).

horizon(p6,40).



testDomain(p7, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p7, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1)]).

horizon(p7,40).



testDomain(p8, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p8, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), level1(s1), freq(s1)]).

horizon(p8,40).



testDomain(p9, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p9, [depot(d), shop(s1), tin(t1,s1), level0(d), freq(d), level1(s1), freq(s1)]).

horizon(p9,40).



testDomain(p10, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p10, [depot(d), shop(s1), level0(d), tin(t1,d), freq(s1)]).

horizon(p10,40).



testDomain(p11, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p11, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1), freq(s1), tfull(t1)]).

horizon(p11,40).



testDomain(p12, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p12, [depot(d), shop(s1), tin(t1,s1), level0(d), freq(d), level0(s1), tfull(t1)]).

horizon(p12,40).



testDomain(p13, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p13, [depot(d), shop(s1), level0(d), freq(d), level0(s1), tin(t1,d), tfull(t1)]).

horizon(p13,40).



testDomain(p14, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p14, [depot(d), shop(s1), level0(d), freq(d), level0(s1), tin(t1,d)]).

horizon(p14,40).



testDomain(p15, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p15, [depot(d), shop(s1), tin(t1,s1), level0(d), freq(s1)]).

horizon(p15,40).



testDomain(p16, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p16, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), freq(s1), tfull(t1)]).

horizon(p16,40).



testDomain(p17, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p17, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), freq(s1)]).

horizon(p17,40).



testDomain(p18, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p18, [depot(d), shop(s1), level0(d), level0(s1), tin(t1,d)]).

horizon(p18,40).



testDomain(p19, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p19, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d)]).

horizon(p19,40).



testDomain(p20, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p20, [depot(d), shop(s1), level0(d), tin(t1,d), level1(s1)]).

horizon(p20,40).



testDomain(p21, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p21, [depot(d), shop(s1), level0(d), freq(d), level0(s1), tin(t1,d), tfull(t1)]).

horizon(p21,40).



testDomain(p22, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p22, [depot(d), shop(s1), level0(d), tin(t1,d)]).

horizon(p22,40).



testDomain(p23, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p23, [depot(d), shop(s1), level0(d), freq(d), tin(t1,d), level1(s1), freq(s1)]).

horizon(p23,40).



testDomain(p24, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p24, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1), freq(s1), tfull(t1)]).

horizon(p24,40).



testDomain(p25, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p25, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1), tfull(t1)]).

horizon(p25,40).



testDomain(p26, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p26, [depot(d), shop(s1), level0(d), freq(d), level0(s1), tin(t1,d), freq(s1)]).

horizon(p26,40).



testDomain(p27, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p27, [depot(d), shop(s1), level0(d), level0(s1), tin(t1,d)]).

horizon(p27,40).



testDomain(p28, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p28, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1), freq(s1), tfull(t1)]).

horizon(p28,40).



testDomain(p29, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p29, [depot(d), shop(s1), tin(t1,s1), level0(d), level1(s1), tfull(t1)]).

horizon(p29,40).



testDomain(p30, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p30, [depot(d), shop(s1), tin(t1,s1), level0(d), level0(s1), freq(s1), tfull(t1)]).

horizon(p30,40).



testDomain(p31, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p31, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level0(s1)]).

horizon(p31,40).



testDomain(p32, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p32, [depot(d), shop(s1), shop(s2), level0(d), level0(s1), tin(t1,d), freq(s1)]).

horizon(p32,40).



testDomain(p33, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p33, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), level0(s1), level1(s2), freq(s1), tfull(t1), freq(s2)]).

horizon(p33,40).



testDomain(p34, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p34, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level1(s2), level1(s1), freq(s2)]).

horizon(p34,40).



testDomain(p35, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p35, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), level0(s2), freq(s1), tfull(t1), freq(s2)]).

horizon(p35,40).



testDomain(p36, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p36, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), level1(s1), level0(s2), tfull(t1)]).

horizon(p36,40).



testDomain(p37, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p37, [depot(d), shop(s1), shop(s2), level0(d), tin(t1,d), level0(s2), tfull(t1)]).

horizon(p37,40).



testDomain(p38, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p38, [depot(d), shop(s1), shop(s2), level0(d), freq(d), tin(t1,d), level1(s1), level0(s2), freq(s1), tfull(t1), freq(s2)]).

horizon(p38,40).



testDomain(p39, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p39, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level1(s1), level0(s2), freq(s1)]).

horizon(p39,40).



testDomain(p40, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p40, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(s1), tfull(t1)]).

horizon(p40,40).



testDomain(p41, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p41, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level0(s1), level0(s2), freq(s1), freq(s2)]).

horizon(p41,40).



testDomain(p42, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p42, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(d), level1(s2), freq(s1)]).

horizon(p42,40).



testDomain(p43, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p43, [depot(d), shop(s1), shop(s2), level0(d), level0(s1), tin(t1,d), freq(s1), freq(s2)]).

horizon(p43,40).



testDomain(p44, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p44, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(d), level0(s1), level1(s2), freq(s1), tfull(t1)]).

horizon(p44,40).



testDomain(p45, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p45, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(d), level0(s2), freq(s1), tfull(t1)]).

horizon(p45,40).



testDomain(p46, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p46, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), level1(s2), tfull(t1)]).

horizon(p46,40).



testDomain(p47, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p47, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level0(s2), freq(s1)]).

horizon(p47,40).



testDomain(p48, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p48, [depot(d), shop(s1), shop(s2), level0(d), freq(d), tin(t1,d), freq(s1), freq(s2)]).

horizon(p48,40).



testDomain(p49, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p49, [depot(d), shop(s1), shop(s2), level0(d), tin(t1,d), freq(s1), tfull(t1)]).

horizon(p49,40).



testDomain(p50, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p50, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), freq(d), level0(s2), freq(s1), tfull(t1), freq(s2)]).

horizon(p50,40).



testDomain(p51, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p51, [depot(d), shop(s1), shop(s2), level0(d), level1(s2), tin(t1,d), level1(s1), tfull(t1), freq(s2)]).

horizon(p51,40).



testDomain(p52, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p52, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(d)]).

horizon(p52,40).



testDomain(p53, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p53, [depot(d), shop(s1), shop(s2), level0(d), tin(t1,d), level0(s2), tfull(t1)]).

horizon(p53,40).



testDomain(p54, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p54, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), level0(s1), level0(s2), tfull(t1), freq(s2)]).

horizon(p54,40).



testDomain(p55, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p55, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), level1(s1), freq(s1), tfull(t1), freq(s2)]).

horizon(p55,40).



testDomain(p56, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p56, [depot(d), shop(s1), shop(s2), tin(t1,s2), level0(d), level0(s2), freq(s2)]).

horizon(p56,40).



testDomain(p57, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p57, [depot(d), shop(s1), shop(s2), level0(d), tin(t1,d), level1(s1), level0(s2), freq(s1), freq(s2)]).

horizon(p57,40).



testDomain(p58, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p58, [depot(d), shop(s1), shop(s2), level0(d), freq(d), level0(s1), level1(s2), tin(t1,d), freq(s1), tfull(t1)]).

horizon(p58,40).



testDomain(p59, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p59, [depot(d), shop(s1), shop(s2), level0(d), freq(d), tin(t1,d), level1(s1), level0(s2), tfull(t1)]).

horizon(p59,40).



testDomain(p60, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p60, [depot(d), shop(s1), shop(s2), tin(t1,s1), level0(d), freq(d), level0(s2), freq(s1)]).

horizon(p60,40).



testDomain(p61, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p61, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), tin(t1,d), level1(s1), level0(s2), freq(s3)]).

horizon(p61,40).



testDomain(p62, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p62, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s1), level0(s1), freq(s2)]).

horizon(p62,40).



testDomain(p63, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p63, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), level1(s2), tin(t1,d), tfull(t1), freq(s3)]).

horizon(p63,40).



testDomain(p64, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p64, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), level1(s1), tin(t1,s3), freq(s2)]).

horizon(p64,40).



testDomain(p65, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p65, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), level1(s2), level1(s1), tfull(t1), tin(t1,s1), freq(s2)]).

horizon(p65,40).



testDomain(p66, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p66, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), tin(t1,d), tfull(t1), level0(s1), freq(s3)]).

horizon(p66,40).



testDomain(p67, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p67, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tfull(t1), tin(t1,s2), freq(d), level0(s3), freq(s2)]).

horizon(p67,40).



testDomain(p68, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p68, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s3), level0(s1), level0(s3), level0(s2), freq(s2)]).

horizon(p68,40).



testDomain(p69, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p69, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s1), freq(d), level0(s3), freq(s3), freq(s2)]).

horizon(p69,40).



testDomain(p70, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p70, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), tfull(t1), tin(t1,s3), level0(s1), freq(s3)]).

horizon(p70,40).



testDomain(p71, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p71, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,d), level0(s1), level0(s2), freq(s1), freq(s2)]).

horizon(p71,40).



testDomain(p72, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p72, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s1), level0(s3), level0(s2), freq(s3), freq(s2)]).

horizon(p72,40).



testDomain(p73, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p73, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), tfull(t1), tin(t1,s1), freq(d), level0(s3), freq(s1)]).

horizon(p73,40).



testDomain(p74, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p74, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), level1(s1), tin(t1,s3), freq(s1), freq(s2)]).

horizon(p74,40).



testDomain(p75, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p75, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), tin(t1,d), level1(s1), level0(s3), freq(s1), freq(s3)]).

horizon(p75,40).



testDomain(p76, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p76, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,d), tfull(t1), freq(d), level0(s1)]).

horizon(p76,40).



testDomain(p77, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p77, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), level1(s1), tin(t1,s2), level0(s2), freq(s3), freq(s2)]).

horizon(p77,40).



testDomain(p78, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p78, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s1), tin(t1,s2), freq(d), level0(s3), level0(s2), freq(s3)]).

horizon(p78,40).



testDomain(p79, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p79, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), freq(d), tin(t1,s3), level0(s1), level0(s3), level0(s2), freq(s2)]).

horizon(p79,40).



testDomain(p80, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p80, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,d), tfull(t1), level0(s1), level0(s3), level0(s2), freq(s1), freq(s3)]).

horizon(p80,40).



testDomain(p81, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p81, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), tin(t1,s2), freq(d), level0(s1), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p81,40).



testDomain(p82, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p82, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), tfull(t1), tin(t1,s2), freq(s3), freq(s2)]).

horizon(p82,40).



testDomain(p83, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p83, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s3), tfull(t1), tin(t1,s3), level0(s1), freq(s3)]).

horizon(p83,40).



testDomain(p84, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p84, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s1), tin(t1,s1), level0(s2), freq(s1)]).

horizon(p84,40).



testDomain(p85, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p85, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), tin(t1,d), tfull(t1), freq(d), level0(s3), freq(s2)]).

horizon(p85,40).



testDomain(p86, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p86, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), tin(t1,s3), level0(s3)]).

horizon(p86,40).



testDomain(p87, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p87, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s2), freq(d), level0(s1), level0(s2), freq(s3)]).

horizon(p87,40).



testDomain(p88, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p88, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tin(t1,s2), level0(s1), level0(s3), level0(s2), freq(s1), freq(s2)]).

horizon(p88,40).



testDomain(p89, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p89, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), level1(s2), level1(s1), tin(t1,s3), freq(s1), freq(s3), freq(s2)]).

horizon(p89,40).



testDomain(p90, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p90, [depot(d), shop(s1), shop(s2), shop(s3), level0(d), tfull(t1), tin(t1,s1), level0(s1), level0(s3), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p90,40).



testDomain(p91, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p91, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), tfull(t1), level1(s4), tin(t1,s1), freq(d), freq(s1), freq(s2)]).

horizon(p91,40).



testDomain(p92, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p92, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), tin(t1,d), level1(s1), level1(s4), freq(d)]).

horizon(p92,40).



testDomain(p93, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p93, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s2), tin(t1,d), level1(s1), tfull(t1), level1(s4), freq(d), level0(s3)]).

horizon(p93,40).



testDomain(p94, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p94, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s2), tin(t1,d), freq(d), level0(s1), freq(s1), freq(s3), freq(s2)]).

horizon(p94,40).



testDomain(p95, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p95, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s3), level1(s2), level1(s4), tin(t1,s4)]).

horizon(p95,40).



testDomain(p96, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p96, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), tfull(t1), tin(t1,s3), level0(s1), level0(s3), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p96,40).



testDomain(p97, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p97, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s3), tin(t1,s2), level1(s4), level0(s2), freq(s1), freq(s3)]).

horizon(p97,40).



testDomain(p98, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p98, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), level1(s2), level1(s1), tfull(t1), tin(t1,s3), freq(s1), freq(s2)]).

horizon(p98,40).



testDomain(p99, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p99, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s2), tin(t1,d), level1(s4), freq(d), level0(s1), freq(s1), freq(s3)]).

horizon(p99,40).



testDomain(p100, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p100, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s1), tin(t1,s2), level1(s4), freq(d), level0(s3), freq(s1), freq(s2)]).

horizon(p100,40).



testDomain(p101, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p101, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), tfull(t1), level1(s4), tin(t1,s3), level0(s3), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p101,40).



testDomain(p102, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p102, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s3), level1(s2), level1(s1), freq(d), tin(t1,s4), freq(s1)]).

horizon(p102,40).



testDomain(p103, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p103, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s3), tfull(t1), level1(s4), tin(t1,s3), level0(s2), freq(s1), freq(s3)]).

horizon(p103,40).



testDomain(p104, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p104, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), level1(s3), level1(s2), tfull(t1), freq(d), tin(t1,s3), level0(s1), freq(s3), freq(s2)]).

horizon(p104,40).



testDomain(p105, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p105, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), tfull(t1), tin(t1,s4), level0(s1), freq(s3), freq(s2)]).

horizon(p105,40).



testDomain(p106, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p106, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), tin(t1,d), level1(s1), tfull(t1), level0(s2)]).

horizon(p106,40).



testDomain(p107, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p107, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s2), level1(s1), tfull(t1), level1(s4), freq(d), tin(t1,s4), level0(s3), freq(s1), freq(s3), freq(s2)]).

horizon(p107,40).



testDomain(p108, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p108, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), level1(s2), freq(d), tin(t1,s4), level0(s1), freq(s1)]).

horizon(p108,40).



testDomain(p109, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p109, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), level1(s3), freq(d), tin(t1,s4), level0(s2), freq(s3), freq(s2)]).

horizon(p109,40).



testDomain(p110, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p110, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), tin(t1,s2), level0(s1), freq(s3)]).

horizon(p110,40).



testDomain(p111, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p111, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), level1(s3), tfull(t1), tin(t1,s1), freq(d), level0(s1), freq(s2)]).

horizon(p111,40).



testDomain(p112, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p112, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s3), level1(s1), level1(s4), tin(t1,s1), freq(d), level0(s2), freq(s1), freq(s3), freq(s2)]).

horizon(p112,40).



testDomain(p113, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p113, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s3), level1(s2), tin(t1,d), level1(s1), tfull(t1), freq(s1), freq(s3), freq(s2)]).

horizon(p113,40).



testDomain(p114, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p114, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), level1(s3), level1(s2), tin(t1,s4), level0(s1), freq(s1), freq(s3), freq(s2)]).

horizon(p114,40).



testDomain(p115, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p115, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), level1(s3), level1(s1), tin(t1,s1), level0(s2), freq(s1), freq(s3)]).

horizon(p115,40).



testDomain(p116, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p116, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), tfull(t1), level1(s4), tin(t1,s3), level0(s3), freq(s1), freq(s3)]).

horizon(p116,40).



testDomain(p117, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p117, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s3), level1(s2), tin(t1,s4)]).

horizon(p117,40).



testDomain(p118, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p118, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level1(s1), tfull(t1), tin(t1,s3), freq(s1), freq(s2)]).

horizon(p118,40).



testDomain(p119, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p119, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), freq(s4), level1(s2), tin(t1,d), freq(d), level0(s3), freq(s1)]).

horizon(p119,40).



testDomain(p120, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p120, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), level0(d), level0(s4), freq(s4), tfull(t1), tin(t1,s2), freq(d), freq(s3)]).

horizon(p120,40).




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







testDomain(p151, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p151, [depot(d), shop(s1), level0(d), level0(s1), tin(t1,s1)]).

horizon(p151,40).






