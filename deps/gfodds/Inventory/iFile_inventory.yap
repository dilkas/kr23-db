%% Examples for model checking reduciton by forward random walk


%interpretationObjs3579(empty,[objects(location,[s1,s2,d]),objects(truck,[t1]),objects(maxobj,[a,a1])]).

interpretationObjs3579(empty, [objects(location,[s1,d]),objects(truck,[t1]), objects(maxobj,[a,a1])]).


interpretations3579(empty,[
%[[empty],[not_empty,not_empty(s1),not_empty(s2),not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,s1)],[not_tin,not_tin(t1,s2),not_tin(t1,d)],[shop,shop(s1),shop(s2)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]],
%[[empty,empty(s1)],[not_empty,not_empty(s2),not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,s1)],[not_tin,not_tin(t1,s2),not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(s2),not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]],
%[[empty,empty(s1)],[not_empty,not_empty(s2),not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,s2)],[not_tin,not_tin(t1,s1),not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(s2),not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]],
%[[empty,empty(s1)],[not_empty,not_empty(s2),not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1),not_tin(t1,s2)],[shop,shop(s1)],[not_shop,not_shop(s2),not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]],
%[[empty,empty(s1),empty(s2)],[not_empty,not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1),not_tin(t1,s2)],[shop,shop(s1)],[not_shop,not_shop(s2),not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]],
%[[empty,empty(s2)],[not_empty,not_empty(s1),not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,s1)],[not_tin,not_tin(t1,s2),not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(s2),not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2),not_eq(a,a1),not_eq(a1,a)]]
[[empty,empty(s1),empty(s2)],[not_empty,not_empty(s3),not_empty(s4),not_empty(d)],[tfull,tfull(t1)],[not_tfull,not_tfull(t2)],[tin,tin(t1,d),tin(t2,d)],[not_tin,not_tin(t1,s1),not_tin(t1,s2),not_tin(t1,s3),not_tin(t1,s4),not_tin(t2,s1),not_tin(t2,s2),not_tin(t2,s3),not_tin(t2,s4)],[shop,shop(s1),shop(s2),shop(s3),shop(s4)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2),not_depot(s3),not_depot(s4)],[eq,eq(s1,s1),eq(s2,s2),eq(s3,s3),eq(s4,s4),eq(d,d),eq(t1,t1),eq(t2,t2),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,s2),not_eq(s1,s3),not_eq(s1,s4),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,s3),not_eq(s2,s4),not_eq(s2,d),not_eq(s3,s1),not_eq(s3,s2),not_eq(s3,s4),not_eq(s3,d),not_eq(s4,s1),not_eq(s4,s2),not_eq(s4,s3),not_eq(s4,d),not_eq(d,s1),not_eq(d,s2),not_eq(d,s3),not_eq(d,s4),not_eq(t1,t2),not_eq(t2,t1),not_eq(a,a1),not_eq(a1,a)]]
%[[empty,empty(s1),empty(s2)],[not_empty,not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1),not_tin(t1,s2)],[shop,shop(s1),shop(s2)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1),not_depot(s2)],[eq,eq(s1,s1),eq(s2,s2),eq(d,d),eq(t1,t1)],[not_eq,not_eq(s1,s2),not_eq(s1,d),not_eq(s2,s1),not_eq(s2,d),not_eq(d,s1),not_eq(d,s2)]]
%[[empty,empty(s1)],[not_empty,not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]
]).


%interpretations3579(empty,[[[empty],[not_empty,not_empty(s1),not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,s1)],[not_tin,not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty,not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,s1)],[not_tin,not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty,not_empty(d)],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty,not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,d)],[not_tin,not_tin(t1,s1)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty,not_empty(d)],[tfull],[not_tfull,not_tfull(t1)],[tin,tin(t1,s1)],[not_tin,not_tin(t1,d)],[shop,shop(s1)],[not_shop,not_shop(d)],[depot,depot(d)],[not_depot,not_depot(s1)],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]]).

%interpretations3579(empty,[[[empty],[not_empty],[tfull],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull,tfull(t1)],[not_tfull],[tin,tin(t1,d)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull],[not_tfull],[tin,tin(t1,d)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]],[[empty,empty(s1)],[not_empty],[tfull],[not_tfull],[tin,tin(t1,s1)],[not_tin],[shop,shop(s1)],[not_shop],[depot,depot(d)],[not_depot],[eq,eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1)],[not_eq,not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]]).


%[[depot(d),shop(s1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,d),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tin(t1,d),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)],[depot(d),shop(s1),empty(s1),tin(t1,s1),eq(s1,s1),eq(d,d),eq(t1,t1),eq(a,a),eq(a1,a1),not_eq(s1,d),not_eq(d,s1),not_eq(a,a1),not_eq(a1,a)]]).




%[[depot(d),shop(s1),tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1), tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,d)],[depot(d),shop(s1),empty(s1),tin(t1,d)],[depot(d),shop(s1),empty(s1),tin(t1,s1)]]).



hmm([[depot(d),shop(s1),shop(s2),tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,s1)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,s2)],[depot(d),shop(s1),empty(s1),tfull(t1),tin(t1,d)],[depot(d),shop(s1),empty(s1),empty(s2),tin(t1,d)],[depot(d),shop(s1),empty(s2),tin(t1,s1)]]).







