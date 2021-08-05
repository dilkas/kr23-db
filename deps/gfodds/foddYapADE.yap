

% This file can be used for off line planning and/or for testing a policy by
%  loading appropriate domain definitions and setting parameters

% ----------------------------------------------------------------

% Part I
% Load system code 

%:- yap_flag(language, sicstus).
%:- yap_flag(update_semantics,logical).
:- ensure_loaded('utils.yap').
:- ensure_loaded('foddUtils.yap').
:- ensure_loaded('thmProver6.yap').
:- ensure_loaded('django.yap').
:- ensure_loaded('dc_subsumption.yap').
:- ensure_loaded('reductionUtils.yap').
%:- ensure_loaded('reduce5.yap').
:- ensure_loaded('examplegeneration.yap').
:- ensure_loaded('modelcheckingreductions.yap').
:- ensure_loaded('fodd_inference_3.yap').
:- ensure_loaded('goal_ordering.yap').
:- ensure_loaded('node_precedence.yap').
:- ensure_loaded('apply2.yap').
:- ensure_loaded('printfodd.yap').
:- ensure_loaded('map6.yap').
:- ensure_loaded('valueIteration3.yap').
:- ensure_loaded('policyspeedup.yap').
:- ensure_loaded('policyeval.yap').
%:- ensure_loaded('mdd2.yap').


% Uncomment debug with the right argument for debuggin information. 
% (grep for debug in code)
%debug(main).
%debug(main1).
%debug(main2).

% ----------------------------------------------------------------

% Part II
% load domain definitions 
% set options (and load other domain files if needed for option)

% default if this is not given is 0.999
%discountfactor(0.45).
%discountfactor(0.9).

epsilon(0.2).

% ----------------------------------------------------------------
%This is automatically loaded when used by FODDServerImpl
% ----------------------------------------------------------------

% uncomment if using model checking reductions
% this also requires an example file

%modelchecking.
%modelcheckingnode.

%variableElimination.
%ve_by_mdd.
%empirical_max_reduction.
%unify_avg_var_at_iteration_boundary.
%bidirectional_reduction.
%bidirectional_reduction_high_first.


%% Domain Files
%%% Inventory Control
%:- ensure_loaded('Inventory/inventoryDomain1.yap'). 
%:- ensure_loaded('Inventory/inventoryDomain4_1.yap'). 
%:- ensure_loaded('Inventory/inventoryDomain5_1.yap'). 
%:- ensure_loaded('Inventory/iFile_inventory.yap').
%:- ensure_loaded('Inventory/iFile_inventory_frw_10_10.yap').
%:- ensure_loaded('Inventory/iFile_inventory_2shops_1truck_complete.yap').
%:- ensure_loaded('Inventory/iFile_inventory5_1shop_complete.yap').
%:- ensure_loaded('Inventory/iFile_inventory5_2shops_complete.yap').
%:- ensure_loaded('Inventory/iFile_inventory5_1_2shops_complete.yap').


%%% Advanced Inventory Control
:- ensure_loaded('Inventory_Advanced/inventoryDomain2_1.yap'). 
:- ensure_loaded('Inventory_Advanced/iFile_inventory_advanced_2shops_complete.yap').


%%% Elevator Control
%:- ensure_loaded('Elevators/elevatorsDomain1.yap'). 
%:- ensure_loaded('Elevators/elevatorsDomain1_1.yap').
%:- ensure_loaded('Elevators/elevatorsDomain2.yap'). 
%:- ensure_loaded('Elevators/elevatorsDomain3.yap'). 

%:- ensure_loaded('Elevators/iFile_elevators_3floor_complete.yap').
%:- ensure_loaded('Elevators/iFile_elevators_3floor_30ex.yap').
%:- ensure_loaded('Elevators/iFile_elevators2_3floor_25ex.yap').
%:- ensure_loaded('Elevators/iFile_elevators2_2floors_complete.yap').
%:- ensure_loaded('Elevators/iFile_elevators1_3floors_30ex.yap').
%:- ensure_loaded('Elevators/iFile_elevators1_3floors_complete.yap').
%:- ensure_loaded('Elevators/iFile_elevators1_2floors_complete.yap').
%:- ensure_loaded('Elevators/iFile_elevators3_3floor_50ex.yap').
%:- ensure_loaded('Elevators/iFile_elevators3_2floor_complete.yap').


% when testing the following are needed (test problems and policy)
% OK to leave uncommented when off line planning
%:- ensure_loaded('Inventory/inventory_test.yap').
%:- ensure_loaded('Inventory/inventory_test_add.yap').
%:- ensure_loaded('Inventory/inventory5_test.yap').
:- ensure_loaded('Inventory_Advanced/inventory_advanced_test_add.yap').
%:- ensure_loaded('Elevators/elevators1_test.yap').
%:- ensure_loaded('Elevators/elevators2_test.yap').
%:- ensure_loaded('Elevators/elevators3_test.yap').



% Part III
% Set options (and load other domain files if needed for option)
% Set system parameters 



% The following knobs control serial or parallel execution of code. 
% Each is explicitly declared so they can be asserted/retracted by test scripts
:- dynamic(parallelize_edge_removal/0).
:- dynamic(parallelize_node_removal/0).
:- dynamic(parallelize_allnode_removal/0).
:- dynamic(parallelize_regression_over_actions/0).
:- dynamic(parallelize_planning_over_goals/0).


%:- asserta(parallelize_edge_removal).
%:- asserta(parallelize_node_removal).
%:- asserta(parallelize_allnode_removal).
%:- asserta(parallelize_regression_over_actions).
% :- asserta(parallelize_planning_over_goals).




% the following knobs are for controlling example generation by instance 
% regression and backward random walk. They are used to turn on the use 
% of special literals (action preconditions) and marked literals. Domain 
% file must implement get_readable_state.

%brw_ir_sp_lits.
%brw_ir_marked.



% controling theorem proving reductions. The r7on, r11on and r9on are switches
% that need to be uncommented to turn these reductions on. Uncommenting r7on
% will turn off r10 and commenting r7on will turn on r10.

%r7on.
%r11on.
%r9on.
r13on.

% ===> THIS IS THE MAIN CONTROL
% if set - off line planning occurs via vi with number of iterations
% if commented out testing will occur
%plan(9).
% the number of iterations to run with standartizing apart
% all -> set same as in plan() or larger; none --> set to 0
switch(0).

% In the following line, X indicates the precision value on the
% leaves. Replace X by that number. E.g. X = 10 will map leaves (except 0.0 and
% 1.0) to their closest multiple of 10. If you do not want the approximation
% of merging leaves, set X to 0.

leafprecision(0).



% In the following line X indicates the parameter 'w' of the weighted goal
% ordering heuristic. 0.0 < w <= 1.0.

wgoparam(1.0).



% When policyrules3579 is on, the policy is written in the form of rules 
% to file and the final iteration is not run to completion
policyrules3579.


% When this is turned the random policy is executed. 
% It still needs to read in a policy file but will ignore the policy in there.
%random_policy.


% When postprocesspolicy3579 is on, the policy is processed so that rules 
% containing the reward condition are removed and replaced by a single rule. 
%postprocesspolicy3579.
%postprocesspolicy3579atoms.




testRounds(2).
maxplanningsteps3579(50).



% Part IV

% These are the lines that kick off the planning/execution routines.
% If you are running execution, replace the "exec_out_file" by the 
% file you want to write the output to. 

oldcontrol :-  statistics(cputime, [T1,_]), 
    (plan(X),
     !,
     vi(X)
     ;
     tell('exec_out_file'),
     solve, 
     told),
     statistics(cputime, [T2,_]),
     T3 is (T2 - T1)/1000.0,
     write('Total Time Taken: '), write(T3), write(' seconds'), nl, nl.


planit(X,PF) :-  
%    asserta(plan(X)),
    tell(PF),
    write('%%% POLICY:'), nl, nl, nl, 
    told,
    asserta(outpolicyfile3579(PF)),
    statistics(cputime, [T1,_]), 
     vi(X),
     statistics(cputime, [T2,_]),
     T3 is (T2 - T1)/1000.0,
     write('Total Time Taken: '), write(T3), write(' seconds'), nl, nl.

testit(PF) :-  
    ensure_loaded(PF),
    statistics(cputime, [T1,_]), 
     solve, 
     statistics(cputime, [T2,_]),
     problems(P),
     length(P,Len),
     testRounds(R),
     Deno is Len*R*1000.0,
     T3 is (T2 - T1)/Deno,	
     write('Total Time Taken: '), write(T3), write(' seconds'), nl, nl.

getA(Policy,State,Goal,Action) :-
     ensure_loaded(Policy),
     mygetA(State,Goal,Action),
     write('Chosen Action: '), write(Action),nl.

getApo(Policy,State,Goal,Action) :-
     ensure_loaded(Policy),
     mygetA2(State,Goal,Action),
     write('Chosen Action: '), write(Action),nl.

getApoSet(Policy,State,Goal,Action) :-
     ensure_loaded(Policy),
     mygetA3(State,Goal,Action),
     write('Chosen Action: '), write(Action),nl.

getA2(Policy,State,Goal,Action) :-
     ensure_loaded(Policy),
     mygetA4(State,Goal,Action),
     write('Chosen Action: '), write(Action),nl.


doit :-
    recorda(rewdPred, empty, _),
    recorda(rewdLit, empty(b1), _),
    D = [node(5,tfull(v3),4,3),node(4,tin(v3,b1),2,3),node(3,empty(b1),0,2),node(2,0.7,-1,-1),node(0,0.0,-1,-1)],
    pp(D),	
    gfodd_reduction_by_ve(D,R),
    write(R),nl,
    pp(R).	


doit1 :- 
    recorda(rewdPred, empty, _),
    recorda(rewdLit, empty(b1), _),
    asserta(testPreds([not_p(_A,_B,_C,_D,_E),not_q(_F,_G,_H,_I,_J)])),
    asserta(arg_types([pred(p, [obj,obj,obj,obj,obj]), pred(q, [obj,obj,obj,obj,obj])])),
    asserta(templates([p(_x1,_x2,_x3,_x4,_x5), not_p(_x6,_x7,_x8,_x9,_x10), q(_x11,_x12,_x13,_x14,_x15), not_q(_x16,_x17,_x18,_x19,_x20)])), 
    ObjList = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10],
    Dom = [objects(obj,ObjList)],
    all(p(A,a1,C,D,E), (member(A,ObjList),member(C,ObjList),member(D,ObjList),member(E,ObjList)), All1),
    all(q(a1,A,C,D,E), (member(A,ObjList),member(C,ObjList),member(D,ObjList),member(E,ObjList)), All2),
    conc(All1, All2, I),
    completeStateDescription(I, Dom, I1),
    convert_Ex(I1, I2),
    D = [node(5,p(v1,v2,v3,v4,v5),4,3), node(4,q(v2,v6,v7,v8,v9),2,0), node(3,q(v2,v6,v7,v8,v9),1,0), node(2,2.0,-1,-1), node(1,1.0,-1,-1), node(0,0.0,-1,-1)],
    pp(D),	
    gfodd_evaluation_by_ve(D,I2,Dom,Map),
    write(R),nl,
    pp(R).	

	  


compare_test :-
	D1 = [node(10,eq(a,v3),9,7),node(9,tfull(v4),8,0),node(8,eq(b,v5),5,7),node(7,empty(b),0,6),node(6,1.594,-1,-1),node(5,tin(v4,b),4,0),node(4,shop(b),3,0),node(3,empty(b),2,0),node(2,0.594,-1,-1),node(0,0.0,-1,-1)],
	D2 = [node(9,tfull(v3),8,7),node(8,eq(b,v4),6,7),node(7,empty(b),0,3),node(6,tin(v3,b),5,0),node(5,shop(b),4,0),node(4,empty(b),2,3),node(3,1.594,-1,-1),node(2,0.594,-1,-1),node(0,0.0,-1,-1)], 
	interpretations3579(empty,Ints), 
	interpretationObjs3579(empty,Objs), 
	findall(X, (member(I,Ints),once(gfodd_evaluation_by_ve(D1,I,Objs,X))),Z1),
	findall(X, (member(I,Ints),once(gfodd_evaluation_by_ve(D2,I,Objs,X))),Z2),
	(Z1 = Z2, write('same'),nl ; write('diff'),nl).





hmm(V1,V2):- 
      NT = [[x,y,b,v3,v4],[0.594,[[]],s1,t1,s1],[0.0,[[]],s1,t2,s1],[0.0,[[]],s2,t1,s2],[0.0,[[]],s2,t2,s2],[0.0,[[]],s3,t1,s3],[0.0,[[]],s3,t2,s3],[0.9564,[[]],s4,t1,s4],[0.9564,[[]],s4,t2,s4],[0.9564,[[]],d,t2,d],[0.9564,[[]],d,t1,d],[0.0,[[]],s1,_136130,s2],[0.0,[[]],s1,_136142,s3],[0.0,[[]],s1,_136154,s4],[0.0,[[]],s1,_136166,d],[0.0,[[]],s2,_136178,s1],[0.0,[[]],s2,_136190,s3],[0.0,[[]],s2,_136202,s4],[0.0,[[]],s2,_136214,d],[0.0,[[]],s3,_136226,s1],[0.0,[[]],s3,_136238,s2],[0.0,[[]],s3,_136250,s4],[0.0,[[]],s3,_136262,d],[0.9564,[[]],s4,_136274,s1],[0.9564,[[]],s4,_136286,s2],[0.9564,[[]],s4,_136298,s3],[0.9564,[[]],s4,_136310,d],[0.9564,[[]],d,_136322,s1],[0.9564,[[]],d,_136334,s2],[0.9564,[[]],d,_136346,s3],[0.9564,[[]],d,_136358,s4]],
      all(_, (member([M, _, B, V1, V2], NT), once((write(B), write(' '), write(M), nl))), _).

    

tryeval(D, Round, Op, Stage) :-
%    (Round = 1 -> trace ; true),
    write('Round '), write(Round), nl, write(Op), write(' -- '), write(Stage), nl,
    Dom = [objects(location, [s1,s2, d]), objects(truck, [t1]), objects(maxobj, [a,a1])],
    Ex1 = [depot(d), shop(s1), shop(s2), tin(t1,s1), tfull(t1), empty(s2), empty(d)],
    completeStateDescription(Ex1, Dom, Ex2),
    convert_Ex(Ex2, Ex),
    operator(Op, ActParams, _,_),
    getSubsKey(ActParams, Key),
    variablizeNumList(ActParams, Key, VarActParams),
    variablizeDD(D, Key, VarD),
    all([X, Res], (member(X, [s2,d]), once((VarActParams = [t1,s1,X], gfodd_evaluation_by_ve(VarD, Ex, Dom, Res)))), AllRes), 
    write('Test Example Values are: '),
    write(AllRes).




tryeval1(D, Round, Op, NewAction, Stage) :-
%    (Round = 1 -> trace ; true),
    write('Round '), write(Round), nl, write(Op), write(' -- '), write(Stage), nl,
    Dom = [objects(location, [s1,s2, d]), objects(truck, [t1]), objects(maxobj, [a,a1])],
    Ex1 = [depot(d), shop(s1), shop(s2), tin(t1,s1), tfull(t1), empty(s2), empty(d)],
    completeStateDescription(Ex1, Dom, Ex2),
    convert_Ex(Ex2, Ex),
    NewAction =.. [_ | ActParams],
    getSubsKey(ActParams, Key),
    variablizeNumList(ActParams, Key, VarActParams),
    variablizeDD(D, Key, VarD),
    all([X, Res], (member(X, [s2,d]), once((VarActParams = [t1,s1,X], gfodd_evaluation_by_ve(VarD, Ex, Dom, Res)))), AllRes), 
    write('Test Example Values are: '),
    write(AllRes).




tryeval2(D, Round, Stage) :-
    (Round = 1 -> true
    ;
    write('Round '), write(Round), nl, write(Stage), nl,
    Dom = [objects(location, [s1,s2, d]), objects(truck, [t1]), objects(maxobj, [a,a1])],
    Ex1 = [depot(d), shop(s1), shop(s2), tin(t1,s1), tfull(t1), empty(s2), empty(d)],
    completeStateDescription(Ex1, Dom, Ex2),
    convert_Ex(Ex2, Ex),
    write('Entering gfe'), nl, 
    !,
    gfodd_evaluation_by_ve(D, Ex, Dom, Res),
    write('Test Example Values are: '),
    write(Res), nl,nl).




