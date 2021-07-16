
% --------------------------------------------------

% simple wrapper for action selection
% now only using mygetA4 (other given below are older variants)

mygetA4(State,Goal,Action) :-
    fixStateTD(State,S2,Goal,G2,TestDomain), 
    completeStateDescription(S2, TestDomain, ExPath1),
    convert_Ex(ExPath1, ExPath),
    !,
    diff_goal_state(G2, ExPath, G3),
    (G3=[], write('No visible Goals --> random exploration'),nl;true),
    %write('Diff Goal: '), write(G3), nl,
    %write('State: '), write(ExPath), nl,
    %write('TD: '), write(TestDomain), nl,
    asserta(testDomain(TestDomain)),
   rank_goals(G3, 1.0, G4),
    !,
    getAction(ExPath,G4,Action),
    retractall(testDomain(_)).

% ----------------------------------------------------

% this function takes simple state (list of atoms), goal and 
% test domain (the objects appropriately formatted) and returns the action.

% format of arguments as in:
%  State = [at(a),road(a,endloc),road(endloc,a)]
%  TestDomain = [objects(location,[a,b,c,d,e,f,g,h,endloc])] 
%  Goal =  [goal(1.0,at(endloc))]
%  For more examples See the multiple answers from demoC and demoB

onlineActionChoice(State,Goal,TestDomain,Action) :-
               completeStateDescription(State, TestDomain, ExPath1),
               convert_Ex(ExPath1, ExPath),
               !,
               getAction(ExPath,Goal,Action).

% ----------------------------------------------------

mygetA(State,Goal,Action) :-
  findTheObj(State,TestDomain),
%  all(X,objinI(X,State),All),
%  TestDomain =  [objects(location,All)],
%  write(TestDomain),
  G2 = [goal(1.0,Goal)],
  onlineActionChoice(State,G2,TestDomain,Action).

mygetA2(State,Goal,Action) :-
  fixStateTD(State,S2,TestDomain),
%  write(S2),nl,write(TestDomain),nl,
  G2 = [goal(1.0,Goal)],
  onlineActionChoice(S2,G2,TestDomain,Action).

mygetA3(State,Goal,Action) :-
   fixStateTD(State,S2,TestDomain),
%  write(S2),nl,write(TestDomain),nl,
   completeStateDescription(S2, TestDomain, ExPath1),
   convert_Ex(ExPath1, ExPath),
   !,
    diff_goal_state(Goal, ExPath, Goal1),
%    write('Diff Goal: '), write(Goal1), nl,
    rank_goals(Goal1, 1.0, Goal2),
    !,
    getAction(ExPath,Goal2,Action).




% ---------------------------------------------------

% this picks out states from the iFile and chooses action
demoD(A) :- interpretations3579(P,IS),
            member(I,IS),
            G=at(endloc),
            write('State is: '), write(I),nl,
            mygetA(I,G,A).


% this picks out states from the iFile and chooses action
demoC(A) :- interpretations3579(P,IS),member(I,IS),interpretationObjs3579(TD),onlineActionChoice(I,[goal(1.0,at(endloc))],TD,A).



%this picks out states from the initial states of test problems
demoB(A) :-    problems([H|_T]),
               initState(H,S),
               goal(H,[G],_R),
               testDomain(H,TD),
               onlineActionChoice(S, [goal(1.0,G)],TD,A).



%this picks out states from the initial states of test problems
% mimicks what happens in code during normal run although most 
% of it is not needed
% keep it here for future reference
demoA(A) :- problems([H|_T]),
%               write(H),
               initState(H,S),
               goal(H,[G],_R),
               testDomain(H,TD),
%               write(S),write(G),write(TD),
               asserta(initState(S)), 
               asserta(goal(G)), 
               asserta(testDomain(TD)),
% up to here is setting up what happens before 
% seems silly to recall the same value but do it anyway for now
               testDomain(TestDom),
               initState(InitState),
               goal(Goal),
               completeStateDescription(InitState, TestDom, ExPath1),
               convert_Ex(ExPath1, ExPath),
%plan calls exec which does some goal-order manipulation
% appears ok to ignore for single goal atom so ignore for now
               !,
               getAction(ExPath,Goal,A).

