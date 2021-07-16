%% Examples for model checking reduciton by forward random walk


interpretationObjs3579(empty,[objects(location,[s1,d]),objects(truck,[t1]),objects(maxobj,[a,a1])]).

interpretations3579(empty,[[[empty],[not_empty],[tfull],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull],[not_tfull],[tin,tin(t1,d)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]]).


%[[depot(d),shop(s1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,d),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tin(t1,d),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]).




%[[depot(d),shop(s1),tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1), tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,d)],[depot(d),shop(s1),empty(s1),tin(t1,d)],[depot(d),shop(s1),empty(s1),tin(t1,s1)]]).











