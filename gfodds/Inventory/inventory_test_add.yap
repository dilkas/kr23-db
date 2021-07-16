
%problems([p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30]).

%problems([p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60]).

%problems([p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p90]).

%problems([p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,p101,p102,p103,p104,p105,p106,p107,p108,p109,p110,p111,p112,p113,p114,p115,p116,p117,p118,p119,p120]).

%problems([p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,p131,p132,p133,p134,p135,p136,p137,p138,p139,p140,p141,p142,p143,p144,p145,p146,p147,p148,p149,p150]).

%problems([p151,p152,p153,p154,p155,p156,p157,p158,p159,p160,p161,p162,p163,p164,p165,p166,p167,p168,p169,p170,p171,p172,p173,p174,p175,p176,p177,p178,p179,p180]).

%problems([p181,p182,p183,p184,p185,p186,p187,p188,p189,p190,p191,p192,p193,p194,p195,p196,p197,p198,p199,p200,p201,p202,p203,p204,p205,p206,p207,p208,p209,p210]).

%problems([p211,p212,p213,p214,p215,p216,p217,p218,p219,p220,p221,p222,p223,p224,p225,p226,p227,p228,p229,p230,p231,p232,p233,p234,p235,p236,p237,p238,p239,p240]).

problems([p241,p242,p243,p244,p245,p246,p247,p248,p249,p250,p251,p252,p253,p254,p255,p256,p257,p258,p259,p260,p261,p262,p263,p264,p265,p266,p267,p268,p269,p270]).



testDomain(p1, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p1, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p1,40).



testDomain(p2, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p2, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p2,40).



testDomain(p3, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p3, [depot(d), shop(s1), tin(t1,s1), empty(d)]).

horizon(p3,40).



testDomain(p4, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p4, [depot(d), shop(s1), empty(d), tin(t1,d), tfull(t1)]).

horizon(p4,40).



testDomain(p5, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p5, [depot(d), shop(s1), tin(t1,s1), empty(d), tfull(t1)]).

horizon(p5,40).



testDomain(p6, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p6, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d)]).

horizon(p6,40).



testDomain(p7, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p7, [depot(d), shop(s1), empty(d), tin(t1,d)]).

horizon(p7,40).



testDomain(p8, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p8, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1)]).

horizon(p8,40).



testDomain(p9, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p9, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1)]).

horizon(p9,40).



testDomain(p10, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p10, [depot(d), shop(s1), empty(d), tin(t1,d), tfull(t1)]).

horizon(p10,40).



testDomain(p11, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p11, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d)]).

horizon(p11,40).



testDomain(p12, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p12, [depot(d), shop(s1), empty(d), tin(t1,d), tfull(t1)]).

horizon(p12,40).



testDomain(p13, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p13, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p13,40).



testDomain(p14, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p14, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p14,40).



testDomain(p15, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p15, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p15,40).



testDomain(p16, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p16, [depot(d), shop(s1), empty(d), tin(t1,d)]).

horizon(p16,40).



testDomain(p17, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p17, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d)]).

horizon(p17,40).



testDomain(p18, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p18, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p18,40).



testDomain(p19, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p19, [depot(d), shop(s1), tin(t1,s1), empty(d)]).

horizon(p19,40).



testDomain(p20, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p20, [depot(d), shop(s1), empty(d), tin(t1,d), tfull(t1)]).

horizon(p20,40).



testDomain(p21, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p21, [depot(d), shop(s1), empty(d), tin(t1,d)]).

horizon(p21,40).



testDomain(p22, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p22, [depot(d), shop(s1), empty(d), tin(t1,d)]).

horizon(p22,40).



testDomain(p23, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p23, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p23,40).



testDomain(p24, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p24, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p24,40).



testDomain(p25, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p25, [depot(d), shop(s1), empty(d), empty(s1), tin(t1,d)]).

horizon(p25,40).



testDomain(p26, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p26, [depot(d), shop(s1), tin(t1,s1), empty(d)]).

horizon(p26,40).



testDomain(p27, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p27, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p27,40).



testDomain(p28, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p28, [depot(d), shop(s1), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p28,40).



testDomain(p29, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p29, [depot(d), shop(s1), tin(t1,s1), empty(d), tfull(t1)]).

horizon(p29,40).



testDomain(p30, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1])]).

initState(p30, [depot(d), shop(s1), tin(t1,s1), empty(d)]).

horizon(p30,40).



testDomain(p31, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p31, [depot(d), shop(s1), shop(s2), empty(d), empty(s1), tin(t1,d)]).

horizon(p31,40).



testDomain(p32, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p32, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d)]).

horizon(p32,40).



testDomain(p33, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p33, [depot(d), shop(s1), shop(s2), empty(d), empty(s2), tin(t1,d)]).

horizon(p33,40).



testDomain(p34, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p34, [depot(d), shop(s1), shop(s2), empty(d), tin(t1,d)]).

horizon(p34,40).



testDomain(p35, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p35, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s2), empty(s1)]).

horizon(p35,40).



testDomain(p36, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p36, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2), empty(s1), tfull(t1)]).

horizon(p36,40).



testDomain(p37, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p37, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s2)]).

horizon(p37,40).



testDomain(p38, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p38, [depot(d), shop(s1), shop(s2), empty(d), empty(s2), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p38,40).



testDomain(p39, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p39, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p39,40).



testDomain(p40, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p40, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s1)]).

horizon(p40,40).



testDomain(p41, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p41, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s2), empty(s1), tfull(t1)]).

horizon(p41,40).



testDomain(p42, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p42, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2), tfull(t1)]).

horizon(p42,40).



testDomain(p43, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p43, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), tfull(t1)]).

horizon(p43,40).



testDomain(p44, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p44, [depot(d), shop(s1), shop(s2), empty(d), empty(s1), tin(t1,d)]).

horizon(p44,40).



testDomain(p45, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p45, [depot(d), shop(s1), shop(s2), empty(d), tin(t1,d), tfull(t1)]).

horizon(p45,40).



testDomain(p46, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p46, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s1), tfull(t1)]).

horizon(p46,40).



testDomain(p47, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p47, [depot(d), shop(s1), shop(s2), empty(d), empty(s2), tin(t1,d), tfull(t1)]).

horizon(p47,40).



testDomain(p48, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p48, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d)]).

horizon(p48,40).



testDomain(p49, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p49, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2)]).

horizon(p49,40).



testDomain(p50, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p50, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2), tfull(t1)]).

horizon(p50,40).



testDomain(p51, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p51, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s2), tfull(t1)]).

horizon(p51,40).



testDomain(p52, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p52, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), tfull(t1)]).

horizon(p52,40).



testDomain(p53, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p53, [depot(d), shop(s1), shop(s2), empty(d), tin(t1,d), tfull(t1)]).

horizon(p53,40).



testDomain(p54, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p54, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d)]).

horizon(p54,40).



testDomain(p55, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p55, [depot(d), shop(s1), shop(s2), empty(d), tin(t1,d)]).

horizon(p55,40).



testDomain(p56, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p56, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2)]).

horizon(p56,40).



testDomain(p57, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p57, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2), empty(s1), tfull(t1)]).

horizon(p57,40).



testDomain(p58, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p58, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d)]).

horizon(p58,40).



testDomain(p59, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p59, [depot(d), shop(s1), shop(s2), tin(t1,s2), empty(d), empty(s2), empty(s1)]).

horizon(p59,40).



testDomain(p60, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2])]).

initState(p60, [depot(d), shop(s1), shop(s2), tin(t1,s1), empty(d), empty(s1)]).

horizon(p60,40).



testDomain(p61, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p61, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s3), empty(s1), tfull(t1)]).

horizon(p61,40).



testDomain(p62, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p62, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s2)]).

horizon(p62,40).



testDomain(p63, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p63, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s2), empty(s1), tfull(t1)]).

horizon(p63,40).



testDomain(p64, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p64, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), empty(s2), tin(t1,d), tfull(t1)]).

horizon(p64,40).



testDomain(p65, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p65, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,d)]).

horizon(p65,40).



testDomain(p66, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p66, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,s3), empty(s3), empty(s1), tfull(t1)]).

horizon(p66,40).



testDomain(p67, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p67, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), empty(s3), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p67,40).



testDomain(p68, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p68, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s1), tfull(t1)]).

horizon(p68,40).



testDomain(p69, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p69, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,s3), empty(s2), empty(s1)]).

horizon(p69,40).



testDomain(p70, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p70, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s2), tfull(t1)]).

horizon(p70,40).



testDomain(p71, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p71, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), empty(s2), tin(t1,d), tfull(t1)]).

horizon(p71,40).



testDomain(p72, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p72, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s1)]).

horizon(p72,40).



testDomain(p73, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p73, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p73,40).



testDomain(p74, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p74, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s2), empty(s1)]).

horizon(p74,40).



testDomain(p75, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p75, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p75,40).



testDomain(p76, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p76, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), empty(s2), empty(s1), tin(t1,d)]).

horizon(p76,40).



testDomain(p77, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p77, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p77,40).



testDomain(p78, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p78, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p78,40).



testDomain(p79, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p79, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s3), empty(s2), empty(s1)]).

horizon(p79,40).



testDomain(p80, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p80, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,s3), empty(s3), tfull(t1)]).

horizon(p80,40).



testDomain(p81, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p81, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p81,40).



testDomain(p82, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p82, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s3), empty(s2), empty(s1)]).

horizon(p82,40).



testDomain(p83, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p83, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s2), empty(s1), tfull(t1)]).

horizon(p83,40).



testDomain(p84, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p84, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s2), tfull(t1)]).

horizon(p84,40).



testDomain(p85, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p85, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p85,40).



testDomain(p86, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p86, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s1), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p86,40).



testDomain(p87, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p87, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s3)]).

horizon(p87,40).



testDomain(p88, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p88, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), tfull(t1)]).

horizon(p88,40).



testDomain(p89, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p89, [depot(d), shop(s1), shop(s2), shop(s3), tin(t1,s2), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p89,40).



testDomain(p90, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3])]).

initState(p90, [depot(d), shop(s1), shop(s2), shop(s3), empty(d), tin(t1,s3), empty(s1), tfull(t1)]).

horizon(p90,40).



testDomain(p91, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p91, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p91,40).



testDomain(p92, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p92, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s1), empty(d), tfull(t1)]).

horizon(p92,40).



testDomain(p93, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p93, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), empty(s3), empty(s2), empty(s1)]).

horizon(p93,40).



testDomain(p94, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p94, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), tfull(t1)]).

horizon(p94,40).



testDomain(p95, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p95, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s2), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p95,40).



testDomain(p96, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p96, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), empty(s3), empty(s1), tin(t1,d)]).

horizon(p96,40).



testDomain(p97, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p97, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s4), empty(s2), tfull(t1)]).

horizon(p97,40).



testDomain(p98, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p98, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), tin(t1,s2), empty(d), empty(s3), empty(s2), empty(s1)]).

horizon(p98,40).



testDomain(p99, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p99, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), empty(s3), empty(s2)]).

horizon(p99,40).



testDomain(p100, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p100, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s4), empty(s3), empty(s2), tfull(t1)]).

horizon(p100,40).



testDomain(p101, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p101, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s1), empty(d), empty(s3), empty(s1), tfull(t1)]).

horizon(p101,40).



testDomain(p102, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p102, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s4), empty(s2), empty(s1), tfull(t1)]).

horizon(p102,40).



testDomain(p103, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p103, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s4), empty(s2)]).

horizon(p103,40).



testDomain(p104, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p104, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), tfull(t1)]).

horizon(p104,40).



testDomain(p105, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p105, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), empty(s1), tin(t1,d)]).

horizon(p105,40).



testDomain(p106, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p106, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), empty(s3), tin(t1,d), tfull(t1)]).

horizon(p106,40).



testDomain(p107, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p107, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s2), empty(d), empty(s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p107,40).



testDomain(p108, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p108, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s2), empty(d), empty(s3)]).

horizon(p108,40).



testDomain(p109, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p109, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s2), empty(d), empty(s2), empty(s1)]).

horizon(p109,40).



testDomain(p110, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p110, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), empty(s2), empty(s1), tfull(t1)]).

horizon(p110,40).



testDomain(p111, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p111, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p111,40).



testDomain(p112, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p112, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s3), empty(s3), tfull(t1)]).

horizon(p112,40).



testDomain(p113, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p113, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), tin(t1,s1), empty(d), empty(s3), empty(s2)]).

horizon(p113,40).



testDomain(p114, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p114, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s3), tfull(t1)]).

horizon(p114,40).



testDomain(p115, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p115, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s4), empty(s1)]).

horizon(p115,40).



testDomain(p116, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p116, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), tin(t1,s4), empty(s2), empty(s1)]).

horizon(p116,40).



testDomain(p117, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p117, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), empty(s3), empty(s1), tin(t1,d), tfull(t1)]).

horizon(p117,40).



testDomain(p118, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p118, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(s4), empty(d), empty(s3), empty(s2), tin(t1,d)]).

horizon(p118,40).



testDomain(p119, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p119, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), tin(t1,s1), empty(d), empty(s3), tfull(t1)]).

horizon(p119,40).



testDomain(p120, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4])]).

initState(p120, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), empty(d), tin(t1,s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p120,40).



testDomain(p121, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p121, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), tin(t1,s5), empty(s3), tfull(t1)]).

horizon(p121,40).



testDomain(p122, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p122, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s5), empty(d), empty(s3), empty(s2), empty(s1), tin(t1,d)]).

horizon(p122,40).



testDomain(p123, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p123, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), tin(t1,s5), empty(s3), tfull(t1)]).

horizon(p123,40).



testDomain(p124, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p124, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), tin(t1,s1), empty(d), empty(s3), empty(s2), empty(s1)]).

horizon(p124,40).



testDomain(p125, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p125, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), empty(s3), tin(t1,d)]).

horizon(p125,40).



testDomain(p126, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p126, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), tin(t1,s5), empty(s3)]).

horizon(p126,40).



testDomain(p127, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p127, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), tin(t1,s5), empty(s2), tfull(t1)]).

horizon(p127,40).



testDomain(p128, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p128, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,s3), empty(s3), empty(s2), empty(s1)]).

horizon(p128,40).



testDomain(p129, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p129, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), tin(t1,s3), empty(s3), tfull(t1)]).

horizon(p129,40).



testDomain(p130, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p130, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), tin(t1,s2), empty(d), empty(s3), empty(s1)]).

horizon(p130,40).



testDomain(p131, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p131, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,s5), empty(s3), empty(s2), empty(s1)]).

horizon(p131,40).



testDomain(p132, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p132, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), empty(s3), tin(t1,d), tfull(t1)]).

horizon(p132,40).



testDomain(p133, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p133, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), tin(t1,s3), empty(s3), empty(s2), tfull(t1)]).

horizon(p133,40).



testDomain(p134, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p134, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,s3), empty(s3), empty(s2), empty(s1)]).

horizon(p134,40).



testDomain(p135, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p135, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s5), empty(d), tin(t1,s5), empty(s2)]).

horizon(p135,40).



testDomain(p136, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p136, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), tin(t1,s1), empty(d)]).

horizon(p136,40).



testDomain(p137, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p137, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), tin(t1,s2), empty(s5), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p137,40).



testDomain(p138, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p138, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,d)]).

horizon(p138,40).



testDomain(p139, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p139, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p139,40).



testDomain(p140, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p140, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s5), tin(t1,s1), empty(d), empty(s3), empty(s2), empty(s1), tfull(t1)]).

horizon(p140,40).



testDomain(p141, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p141, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), tin(t1,s3), tfull(t1)]).

horizon(p141,40).



testDomain(p142, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p142, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s5), empty(d), tin(t1,s4), empty(s3), empty(s1), tfull(t1)]).

horizon(p142,40).



testDomain(p143, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p143, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s5), empty(d), tin(t1,s5), empty(s1), tfull(t1)]).

horizon(p143,40).



testDomain(p144, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p144, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), tin(t1,s3), empty(s3), empty(s2)]).

horizon(p144,40).



testDomain(p145, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p145, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(d), tin(t1,s4), empty(s3), empty(s1)]).

horizon(p145,40).



testDomain(p146, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p146, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(d), tin(t1,s4), empty(s3), empty(s2)]).

horizon(p146,40).



testDomain(p147, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p147, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), tin(t1,s2), empty(d), empty(s3), empty(s2), tfull(t1)]).

horizon(p147,40).



testDomain(p148, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p148, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), tin(t1,s5), empty(s3), empty(s2), empty(s1)]).

horizon(p148,40).



testDomain(p149, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p149, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), tin(t1,s1), empty(d), empty(s1), tfull(t1)]).

horizon(p149,40).



testDomain(p150, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5])]).

initState(p150, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), empty(s4), empty(s5), empty(d), empty(s1), tin(t1,d)]).

horizon(p150,40).



testDomain(p151, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p151, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s1), tfull(t1), empty(s4), empty(s5), empty(d), tin(t1,s4)]).

horizon(p151,40).



testDomain(p152, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p152, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), empty(s1), empty(s4), empty(s6), empty(d), tin(t1,s5)]).

horizon(p152,40).



testDomain(p153, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p153, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), empty(s4), empty(d), tin(t1,s4)]).

horizon(p153,40).



testDomain(p154, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p154, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s4), tin(t1,s1), empty(d)]).

horizon(p154,40).



testDomain(p155, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p155, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), tfull(t1), empty(s4), tin(t1,s2), empty(d)]).

horizon(p155,40).



testDomain(p156, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p156, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), tfull(t1), empty(s5), empty(s6), empty(d), tin(t1,s3)]).

horizon(p156,40).



testDomain(p157, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p157, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), tin(t1,d), tfull(t1), empty(s4), empty(s5), empty(d)]).

horizon(p157,40).



testDomain(p158, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p158, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), tfull(t1), empty(s4), empty(d), tin(t1,s6)]).

horizon(p158,40).



testDomain(p159, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p159, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), tfull(t1), tin(t1,s2), empty(s5), empty(s6), empty(d)]).

horizon(p159,40).



testDomain(p160, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p160, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), empty(s5), empty(s6), empty(d), tin(t1,s3)]).

horizon(p160,40).



testDomain(p161, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p161, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), empty(s5), empty(d), tin(t1,s4)]).

horizon(p161,40).



testDomain(p162, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p162, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), tin(t1,s2), empty(s6), empty(d)]).

horizon(p162,40).



testDomain(p163, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p163, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), empty(s6), empty(d), tin(t1,s5)]).

horizon(p163,40).



testDomain(p164, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p164, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), empty(s4), tin(t1,s2), empty(s5), empty(s6), empty(d)]).

horizon(p164,40).



testDomain(p165, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p165, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), tin(t1,d), tfull(t1), empty(s5), empty(s6), empty(d)]).

horizon(p165,40).



testDomain(p166, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p166, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), empty(s1), tin(t1,d), tfull(t1), empty(s4), empty(d)]).

horizon(p166,40).



testDomain(p167, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p167, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), tin(t1,d), tfull(t1), empty(s4), empty(s6), empty(d)]).

horizon(p167,40).



testDomain(p168, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p168, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), tfull(t1), empty(s4), empty(s6), empty(d), tin(t1,s6)]).

horizon(p168,40).



testDomain(p169, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p169, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), tfull(t1), empty(s5), empty(s6), empty(d), tin(t1,s4)]).

horizon(p169,40).



testDomain(p170, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p170, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), empty(s4), empty(s6), empty(d), tin(t1,s4)]).

horizon(p170,40).



testDomain(p171, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p171, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), empty(s5), empty(s6), empty(d), tin(t1,s3)]).

horizon(p171,40).



testDomain(p172, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p172, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s1), tfull(t1), empty(d), tin(t1,s4)]).

horizon(p172,40).



testDomain(p173, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p173, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s1), tin(t1,d), empty(s4), empty(s6), empty(d)]).

horizon(p173,40).



testDomain(p174, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p174, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s2), tfull(t1), empty(d), tin(t1,s6)]).

horizon(p174,40).



testDomain(p175, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p175, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), tfull(t1), empty(s4), empty(s5), empty(s6), empty(d), tin(t1,s4)]).

horizon(p175,40).



testDomain(p176, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p176, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), tfull(t1), empty(s6), empty(d), tin(t1,s5)]).

horizon(p176,40).



testDomain(p177, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p177, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), empty(s4), empty(s6), empty(d), tin(t1,s6)]).

horizon(p177,40).



testDomain(p178, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p178, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s2), empty(s1), tfull(t1), tin(t1,s2), empty(d)]).

horizon(p178,40).



testDomain(p179, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p179, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s1), empty(s4), tin(t1,s2), empty(s5), empty(d)]).

horizon(p179,40).



testDomain(p180, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6])]).

initState(p180, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), empty(s3), empty(s5), empty(d), tin(t1,s6)]).

horizon(p180,40).



testDomain(p181, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p181, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s2), empty(s1), empty(s5), empty(s6), empty(d), tin(t1,s5)]).

horizon(p181,40).



testDomain(p182, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p182, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), tfull(t1), empty(s4), tin(t1,s1), empty(d), empty(s7)]).

horizon(p182,40).



testDomain(p183, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p183, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s2), empty(s5), tin(t1,s1), empty(s6), empty(d), empty(s7)]).

horizon(p183,40).



testDomain(p184, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p184, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), tfull(t1), empty(s4), empty(s5), empty(d), empty(s7), tin(t1,s5)]).

horizon(p184,40).



testDomain(p185, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p185, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), tin(t1,s2), empty(s5), empty(d)]).

horizon(p185,40).



testDomain(p186, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p186, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s1), empty(s4), empty(s6), empty(d), empty(s7), tin(t1,s4)]).

horizon(p186,40).



testDomain(p187, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p187, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), tin(t1,d), empty(s4), empty(s6), empty(d), empty(s7)]).

horizon(p187,40).



testDomain(p188, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p188, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), tfull(t1), empty(s6), empty(d), tin(t1,s3)]).

horizon(p188,40).



testDomain(p189, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p189, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), tfull(t1), empty(d), tin(t1,s4)]).

horizon(p189,40).



testDomain(p190, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p190, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), tfull(t1), empty(d), tin(t1,s5)]).

horizon(p190,40).



testDomain(p191, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p191, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), tin(t1,s2), empty(d)]).

horizon(p191,40).



testDomain(p192, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p192, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s4), tin(t1,s2), empty(d), empty(s7)]).

horizon(p192,40).



testDomain(p193, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p193, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s2), tfull(t1), empty(s6), empty(d), tin(t1,s4)]).

horizon(p193,40).



testDomain(p194, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p194, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s2), empty(s1), empty(s4), empty(s5), tin(t1,s1), empty(d), empty(s7)]).

horizon(p194,40).



testDomain(p195, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p195, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), empty(s4), empty(s5), empty(d), tin(t1,s6)]).

horizon(p195,40).



testDomain(p196, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p196, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s2), empty(s1), empty(s4), empty(s5), tin(t1,s1), empty(d), empty(s7)]).

horizon(p196,40).



testDomain(p197, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p197, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), tin(t1,d), tfull(t1), empty(s4), empty(s5), empty(s6), empty(d)]).

horizon(p197,40).



testDomain(p198, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p198, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), tfull(t1), empty(s5), tin(t1,s1), empty(s6), empty(d)]).

horizon(p198,40).



testDomain(p199, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p199, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), empty(s6), empty(d), empty(s7), tin(t1,s4)]).

horizon(p199,40).



testDomain(p200, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p200, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), tin(t1,d), empty(s5), empty(s6), empty(d), empty(s7)]).

horizon(p200,40).



testDomain(p201, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p201, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s1), empty(d), tin(t1,s6)]).

horizon(p201,40).



testDomain(p202, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p202, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s5), empty(s6), empty(d), tin(t1,s6)]).

horizon(p202,40).



testDomain(p203, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p203, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), tin(t1,d), empty(d), empty(s7)]).

horizon(p203,40).



testDomain(p204, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p204, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), tin(t1,d), empty(s4), empty(s6), empty(d)]).

horizon(p204,40).



testDomain(p205, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p205, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), tin(t1,d), tfull(t1), empty(s5), empty(s6), empty(d)]).

horizon(p205,40).



testDomain(p206, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p206, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s1), tfull(t1), empty(s5), empty(s6), empty(d), tin(t1,s4)]).

horizon(p206,40).



testDomain(p207, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p207, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), tin(t1,s1), empty(d), empty(s7)]).

horizon(p207,40).



testDomain(p208, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p208, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s1), tin(t1,d), tfull(t1), empty(s4), empty(s6), empty(d), empty(s7)]).

horizon(p208,40).



testDomain(p209, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p209, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s1), tfull(t1), empty(d), tin(t1,s3)]).

horizon(p209,40).



testDomain(p210, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7])]).

initState(p210, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s5), empty(s6), empty(d), tin(t1,s5)]).

horizon(p210,40).



testDomain(p211, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p211, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), tfull(t1), empty(s4), empty(s5), tin(t1,s1), empty(d), empty(s8)]).

horizon(p211,40).



testDomain(p212, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p212, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s1), empty(s5), empty(s6), empty(d), tin(t1,s8)]).

horizon(p212,40).



testDomain(p213, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p213, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s4), empty(d), empty(s7), empty(s8), tin(t1,s8)]).

horizon(p213,40).



testDomain(p214, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p214, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s2), tfull(t1), empty(s6), empty(d), empty(s8), tin(t1,s4)]).

horizon(p214,40).



testDomain(p215, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p215, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s1), tfull(t1), empty(s4), empty(d), tin(t1,s6)]).

horizon(p215,40).



testDomain(p216, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p216, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s4), tin(t1,s2), empty(s6), empty(d), empty(s8)]).

horizon(p216,40).



testDomain(p217, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p217, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), tin(t1,s2), empty(s5), empty(s6), empty(d), empty(s7)]).

horizon(p217,40).



testDomain(p218, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p218, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), tin(t1,s2), empty(s6), empty(d), empty(s8)]).

horizon(p218,40).



testDomain(p219, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p219, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), tfull(t1), empty(d), tin(t1,s5)]).

horizon(p219,40).



testDomain(p220, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p220, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), tfull(t1), tin(t1,s1), empty(d), empty(s8)]).

horizon(p220,40).



testDomain(p221, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p221, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), tfull(t1), empty(s5), empty(s6), empty(d), empty(s7), tin(t1,s3)]).

horizon(p221,40).



testDomain(p222, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p222, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s1), tfull(t1), empty(s5), tin(t1,s1), empty(s6), empty(d), empty(s8)]).

horizon(p222,40).



testDomain(p223, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p223, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), tfull(t1), empty(s6), empty(d), empty(s7), tin(t1,s8)]).

horizon(p223,40).



testDomain(p224, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p224, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), tfull(t1), empty(s6), empty(d), tin(t1,s3)]).

horizon(p224,40).



testDomain(p225, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p225, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), empty(s4), empty(s6), empty(d), empty(s8), tin(t1,s8)]).

horizon(p225,40).



testDomain(p226, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p226, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), tfull(t1), empty(d), empty(s8), tin(t1,s3)]).

horizon(p226,40).



testDomain(p227, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p227, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), empty(s4), tin(t1,s2), empty(s6), empty(d), empty(s7), empty(s8)]).

horizon(p227,40).



testDomain(p228, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p228, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), empty(s5), empty(d), tin(t1,s7)]).

horizon(p228,40).



testDomain(p229, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p229, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s4), empty(s5), empty(d), tin(t1,s5)]).

horizon(p229,40).



testDomain(p230, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p230, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), empty(d), empty(s7), tin(t1,s3)]).

horizon(p230,40).



testDomain(p231, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p231, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s1), empty(s4), empty(s5), empty(s6), empty(d), empty(s7), tin(t1,s6)]).

horizon(p231,40).



testDomain(p232, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p232, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s1), tin(t1,d), tfull(t1), empty(s6), empty(d)]).

horizon(p232,40).



testDomain(p233, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p233, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), tfull(t1), empty(s5), empty(d), empty(s7), empty(s8), tin(t1,s5)]).

horizon(p233,40).



testDomain(p234, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p234, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s1), tfull(t1), empty(s4), empty(d), empty(s8), tin(t1,s5)]).

horizon(p234,40).



testDomain(p235, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p235, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), tin(t1,d), tfull(t1), empty(s6), empty(d)]).

horizon(p235,40).



testDomain(p236, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p236, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), empty(s6), empty(d), empty(s7), tin(t1,s8)]).

horizon(p236,40).



testDomain(p237, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p237, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s2), tin(t1,s2), empty(s6), empty(d), empty(s7)]).

horizon(p237,40).



testDomain(p238, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p238, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s2), empty(s1), empty(s4), tin(t1,s2), empty(d), empty(s7), empty(s8)]).

horizon(p238,40).



testDomain(p239, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p239, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s3), empty(s2), tfull(t1), empty(s6), empty(d), empty(s7), empty(s8), tin(t1,s6)]).

horizon(p239,40).



testDomain(p240, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8])]).

initState(p240, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), empty(s6), empty(d), empty(s7), tin(t1,s8)]).

horizon(p240,40).




testDomain(p241, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p241, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tfull(t1), empty(s5), tin(t1,s1), empty(d), empty(s7)]).

horizon(p241,40).



testDomain(p242, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p242, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s1), tfull(t1), empty(s4), empty(s5), empty(d), empty(s8), tin(t1,s6)]).

horizon(p242,40).



testDomain(p243, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p243, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), tin(t1,d), tfull(t1), empty(s4), empty(d), empty(s8)]).

horizon(p243,40).



testDomain(p244, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p244, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), empty(s4), empty(s6), empty(d), empty(s7), empty(s8), tin(t1,s9)]).

horizon(p244,40).



testDomain(p245, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p245, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), tfull(t1), empty(d), empty(s7), empty(s8), tin(t1,s9)]).

horizon(p245,40).



testDomain(p246, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p246, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), tin(t1,d), empty(s4), empty(s5), empty(d), empty(s9)]).

horizon(p246,40).



testDomain(p247, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p247, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(d), empty(s7), empty(s8), empty(s9), tin(t1,s3)]).

horizon(p247,40).



testDomain(p248, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p248, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tfull(t1), empty(s5), empty(d), empty(s8), empty(s9), tin(t1,s9)]).

horizon(p248,40).



testDomain(p249, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p249, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), empty(s1), empty(s4), empty(s6), empty(d), empty(s8), empty(s9), tin(t1,s8)]).

horizon(p249,40).



testDomain(p250, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p250, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), tfull(t1), empty(s4), empty(d), empty(s7), empty(s8), tin(t1,s9)]).

horizon(p250,40).



testDomain(p251, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p251, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s1), tfull(t1), empty(d), empty(s9), tin(t1,s7)]).

horizon(p251,40).



testDomain(p252, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p252, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), tfull(t1), empty(s4), empty(s5), empty(d), empty(s9), tin(t1,s3)]).

horizon(p252,40).



testDomain(p253, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p253, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s4), empty(s5), empty(d), empty(s7), empty(s8), empty(s9), tin(t1,s8)]).

horizon(p253,40).



testDomain(p254, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p254, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), tfull(t1), empty(s4), empty(d), empty(s7), tin(t1,s8)]).

horizon(p254,40).



testDomain(p255, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p255, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), empty(s1), empty(s4), empty(s5), empty(s6), empty(d), empty(s7), empty(s8), tin(t1,s7)]).

horizon(p255,40).



testDomain(p256, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p256, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s1), tin(t1,d), tfull(t1), empty(s4), empty(d), empty(s7), empty(s9)]).

horizon(p256,40).



testDomain(p257, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p257, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), empty(s1), tfull(t1), empty(d), empty(s7), tin(t1,s6)]).

horizon(p257,40).



testDomain(p258, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p258, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s2), empty(s1), tfull(t1), empty(s4), tin(t1,s2), empty(s5), empty(d)]).

horizon(p258,40).



testDomain(p259, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p259, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s2), tfull(t1), empty(s5), empty(d), empty(s7), empty(s8), empty(s9), tin(t1,s3)]).

horizon(p259,40).



testDomain(p260, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p260, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), empty(s4), empty(s6), empty(d), empty(s7), tin(t1,s7)]).

horizon(p260,40).



testDomain(p261, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p261, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s2), empty(s1), empty(d), empty(s8), empty(s9), tin(t1,s7)]).

horizon(p261,40).



testDomain(p262, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p262, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s2), empty(s1), tfull(t1), empty(s4), empty(d), empty(s7), empty(s8), empty(s9), tin(t1,s3)]).

horizon(p262,40).



testDomain(p263, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p263, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tfull(t1), empty(s6), empty(d), empty(s8), tin(t1,s6), empty(s9)]).

horizon(p263,40).



testDomain(p264, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p264, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s2), empty(s4), empty(s5), empty(d), tin(t1,s7)]).

horizon(p264,40).



testDomain(p265, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p265, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), tfull(t1), empty(s4), empty(d), tin(t1,s3)]).

horizon(p265,40).



testDomain(p266, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p266, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tfull(t1), empty(s5), empty(d), empty(s8), tin(t1,s6), empty(s9)]).

horizon(p266,40).



testDomain(p267, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p267, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s3), empty(s2), tfull(t1), empty(s5), empty(d), empty(s8), tin(t1,s6), empty(s9)]).

horizon(p267,40).



testDomain(p268, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p268, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tin(t1,d), tfull(t1), empty(s5), empty(d)]).

horizon(p268,40).



testDomain(p269, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p269, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), tfull(t1), empty(s4), empty(s5), empty(d), empty(s9), tin(t1,s9)]).

horizon(p269,40).



testDomain(p270, [objects(maxobj,[a,a1]),objects(truck,[t1]),objects(location,[d,s1,s2,s3,s4,s5,s6,s7,s8,s9])]).

initState(p270, [depot(d), shop(s1), shop(s2), shop(s3), shop(s4), shop(s5), shop(s6), shop(s7), shop(s8), shop(s9), empty(s1), empty(d), empty(s7), empty(s8), tin(t1,s8)]).

horizon(p270,40).



