:- dynamic
    t3579/2.

:- use_module(library(terms)).
:- use_module(library(timeout)).

subsumes2([], _) :-
    !.
subsumes2([Lit | Rest], Ex) :-
    member(Lit, Ex),
    once(optimize_path_order(Rest, Rest1)),
    subsumes2(Rest1, Ex).


subsumes3(X, Y) :- 
    convert_Ex(Y, Ex),
    !,
    dc_subsumption(X, Ex).



subsumes4(Path, Ex) :-
    django_subsumption(Path, Ex).

subsumes1(X, Y) :-
%    subsumption(X, Y, [], 1).
%    statistics(cputime, [T1,_]),
    time_out((subsumes3(X, Y), A = 1 ; A = 0), 1000, Res),
%    !, Res = success.
%    statistics(cputime, [T2,_]),
%    T3 is T2 - T1,
%    open('tireworld_exec_time_stats_10', append, S),
%    tell(S), 
%    (Res == time_out,
%     write('timeout'), nl, nl
%     ;
%     once((A == 1)),
%     T3 > 1.0,
%     write(T3), nl, nl
%     ;
%     true),
%    close(S),
    !, 
    once((A == 1)), Res == success.

%    subsumes2(X, Y).
%    subsumes3(X, Y).
%    subsumes4(X, Y).




get_unbound_count([], C, C) :-
    !.
get_unbound_count([X | Rest], CsoFar, C) :-
    var(X),
    !,
    CsoFar1 is CsoFar + 1,
    get_unbound_count(Rest, CsoFar1, C).
get_unbound_count([X | Rest], CsoFar, C) :-
    get_unbound_count(Rest, CsoFar, C).
    
    
    
get_lit_ranks([], []) :- 
    !.
get_lit_ranks([Lit | Path], [[C, Lit] | Rest]) :-
    Lit =.. [P | Vars],
    get_unbound_count(Vars, 0, C),
    get_lit_ranks(Path, Rest).
    

retrieve_path([], []) :- 
    !.
retrieve_path([[_, Lit] | Other], [Lit | Rest]) :-
    retrieve_path(Other, Rest).

    

optimize_path_order(Path, OptPath) :-
    get_lit_ranks(Path, LitRanks),
    sort(LitRanks, LitRanks1),
    retrieve_path(LitRanks1, OptPath).




subsumes_unique([], _) :-
    !.
subsumes_unique([Lit | Rest], Ex) :-
    del(Lit, Ex, Ex1),
    once(optimize_path_order(Rest, Rest1)),
    subsumes_unique(Rest1, Ex1).




subsumption1([], Ex, Objs, Class) :-
%    once(Class =.. [_ | ClassVars]),
%    once(validHead(ClassVars, Objs, Constr)),
    once(not(member(Class, Ex))).
subsumption1([Lit | Rest], Ex, Objs, Class) :-
    member(Lit, Ex),
    once(optimize_path_order(Rest, Rest1)),
    subsumption1(Rest1, Ex, Objs, Class).


subsumption2(W, X, Y, Z) :- 
    ((Y == noconv) -> (Ex = X) ; (convert_Ex(X, Ex))),
    !,
    dc_subsumption1(W, Ex, Y, Z).






subsumption(W, X, Y, Z) :- 
%    statistics(cputime, [T1,_]),
    time_out((subsumption2(W, X, Y, Z), A = 1 ; A = 0), 1000, Res),
%    statistics(cputime, [T2,_]),
%    T3 is T2 - T1,
%    open('tireworld_exec_time_stats_10', append, S),
%    tell(S), 
%    (Res == time_out,
%     write('timeout'), nl, nl
%     ;
%     once((A == 1)),
%     T3 > 1.0,
%     write(T3), nl, nl
%     ;
%     true),
%    close(S),
    !, 
    once((A == 1)), Res == success.
%    subsumption2(W, X, Y, Z).






subsumption_all(Path, Ex, Lit, LitsSoFar, Lits) :- 
    once(copy_term([Lit | Path], [Lit1 | Path1])),
    once(subsumption(Path1, Ex, noconv, Lit1)),
    add_Lit_to_conv_State(Lit1, Ex, Ex1),
    subsumption_all(Path, Ex1, Lit, [Lit1 | LitsSoFar], Lits),
    !.
subsumption_all(_, _, _, Lits, Lits).



subsumption_all(Path, Ex, Lit, Lits) :- 
    Lit =.. [P | _],
    subsumption_all(Path, [[P] | Ex], Lit, [], Lits).


pathSatisfiable([], _) :- 
    !.
pathSatisfiable([Lit | Rest], Path) :- 
    Lit =.. [Pr | V],
    name(Pr, Nr),
    (Nr = [110, 111, 116, 95 | Nrr],
     name(P, Nrr)
     ;
     name(P, [110, 111, 116, 95 | Nr])),
    Lit1 =.. [P | V],
    !,
    not(member(Lit1, Path)),
    pathSatisfiable(Rest, Path).





 

getPathObjects([], V, V) :- 
    !.
getPathObjects([Lit | Path], SoFar, Objects) :- 
    Lit =.. [_ | V],
    batchInsert(V, SoFar, SoFar1),
    getPathObjects(Path, SoFar1, Objects).
    


getBasicBkgd(Path, QVars, Objs, ExPath) :- 
     getPathObjects(Path, [], Objs1),
     (all(X, (member(X, Objs1) ; member(X, QVars)), Objs) ; Objs = []),
     (all(not_eq(X, Y), (member(X, Objs), member(Y, Objs), domConst(X), domConst(Y), X \= Y), BasicInEq) ; BasicInEq = []),
     (all(eq(X,X), member(X, Objs), BasicEq) ; BasicEq = []),
     (all(not_eq(X, d3579), member(X, Objs), BasicInEq1) ; BasicInEq1 = []),
     (all(not_eq(d3579, X), member(X, Objs), BasicInEq2) ; BasicInEq2 = []),
     flatten([Path, BasicEq, [not_eq(d3579, d3579) | BasicInEq1], BasicInEq2, BasicInEq], ExPath).





validHead([], _, []).
validHead([X | Rest], Objs, [1 | Rest1]) :- 
    ground(X),
    !,
    validHead(Rest, Objs, Rest1).
validHead([X | Rest], Objs, [0 | Rest1]) :- 
    member(X, Objs),
    validHead(Rest, Objs, Rest1).


   

collectNewBkgd1(A, B, C) :-
    statistics(runtime, [T1, T11]),
    once((collectNewBkgd1(A, B, C) ; write('CNBK failed'), nl, !, fail)),
    statistics(runtime, [T2, T22]),
    T3 is T2 - T1,
    T4 is T3/1000,
    T5 is T22/1000,
%    write(T4), write(' '), write(T22), nl,
    retract(t3579(r7, [T6, T7])),
    T8 is T6 + T4,
    T9 is T7 + T5,
    asserta(t3579(r7, [T8, T9])).



collectNewBkgd1(Ex, Objs, Bkgd) :-  
    (all(Head, (rule(Head, Constr, Body), subsumes1(Body, Ex), once(member(Head, Ex)), once(Head =.. [_ | V]),  validHead(V, Objs, Constr)), Bkgd) ; Bkgd = []).






collectNewBkgd2(_, [], Bkgd, Bkgd) :- 
    !.
collectNewBkgd2(Ex, Rules, BkgdSoFar, Bkgd) :- 
    once(member(rule(Head1, _, Body1), Rules)),
    once(copy_term(rule(Head1, _, Body1), rule(Head, _, Body))),
    once(subsumption1(Body, Ex, _, Head)),
    collectNewBkgd2([Head | Ex], Rules, [Head | BkgdSoFar], Bkgd).
collectNewBkgd2(Ex, [_ | Rules], BkgdSoFar, Bkgd) :- 
    collectNewBkgd2(Ex, Rules, BkgdSoFar, Bkgd).







collectNewBkgd(Ex, Objs, Bkgd) :-      
    (all(Head, (rule(Head, _, Body), subsumption(Body, Ex, Objs, Head)), Bkgd) ; Bkgd = []).






floodState1(A, B, C) :-
    statistics(runtime, [T1, T11]),
    floodState1(A, B, C),
    statistics(runtime, [T2, T22]),
    T3 is T2 - T1,
    T4 is T3/1000,
    T5 is T22/1000,
%    write(T4), write(' '), write(T5), nl,
    (retract(t3579(r7, [T6, T7])) ; (T6 = 0.0, T7 = 0.0)),
    T8 is T6 + T4,
    T9 is T7 + T5,
    asserta(t3579(r7, [T8, T9])).



floodState(Ex, Objs, TotalEx) :-
    once(all(rule(X,Y,Z), rule(X,Y,Z), Rules)),
    once(collectNewBkgd2(Ex, Rules, [], Bkgd)),
%    once(collectNewBkgd(Ex, Objs, Bkgd)),
    once(not(Bkgd == [])),
%    once((not(member(false, Bkgd)))),
    once(conc(Ex, Bkgd, NewEx)),
    floodState(NewEx, Objs, TotalEx),
    !.
floodState(Ex, _, Ex).
 



translate([], []) :- 
    !.
translate([First | Rest], [First | TRest]) :- 
    ground(First),
    translate(Rest, TRest),
    !.
translate([_ | Rest], [var | TRest]) :- 
    translate(Rest, TRest).

 






checkImplication1(Path, QForm, Objs, Res) :- 
    once(collectNewBkgd(Path, Objs, NewPath)),
    NewPath \= [], 
    (member(QForm, NewPath),
     Res = 1
     ;
     conc(Path, NewPath, ExPath),
     checkImplication1(ExPath, QForm, Objs, Res)),
    !.
checkImplication1(_, _, _, 0).
 


checkImplication(Path, QForm, Objs, Res) :- 
    once(collectNewBkgd(Path, Objs, NewPath)),
    NewPath \= [], 
    once(conc(Path, NewPath, ExPath)),
    (not(pathSatisfiable(ExPath, ExPath)),
     Res = 1
     ;
     assertEx(NewPath),
      (debug(4) -> write('path asserted: '), write(NewPath), nl ; true),
     (not(not(call(QForm))),
      Res = 1
      ;
      checkImplication(ExPath, QForm, Objs, Res)),
     retractEx(NewPath),
     (debug(4) -> write('path retracted'), nl ; true)),
    !.
checkImplication(_, _, _, 0).
     



 
%% replaceNodeFormVars(+, +, -)
%% replaceNodeFormVars(NF, K, F)
%% Same as replaceNFVars except that the node formula NF is 'interpreted' (see below)

 

splitDisjunct((First ; Rest), AllLits) :-
    (prove(First, AllLits)    
     ;
     splitDisjunct(Rest, AllLits)),
    !.
splitDisjunct((First), AllLits) :-
    prove(First, AllLits).

 

 
%% replaceEdgeFormVars(+, +, -)
%% replaceEdgeFormVars(EF, K, F)
%% Same as replaceEFVars except that the edge formula EF is 'interpreted' (see below)

prove((true, (Form)), AllLits) :-
    splitDisjunct(Form, AllLits),
    !.
prove((Lit, (Form)), AllLits) :-
    member(Lit, AllLits),
    splitDisjunct(Form, AllLits),
    !.
prove(Lit, AllLits) :-
    !,
    member(Lit, AllLits).




 
prove_all([], _) :-
    !.
prove_all([ExPath | ExPaths], Q) :-
    !,
%    not(not(prove(Q, ExPath))),
    get_a_path(Q, [], Path), 
    not(not(subsumes2(Path, ExPath))),
    !,
    prove_all(ExPaths, Q).

 
%% proveFormula(+, +, +, -)
%% proveFormula(EF2, [], EF1, Res)
%% ProveFormula is an implication theorem prover. EF2 and EF1 are edge formulas.
%% The second argument [], is a dummy argument (it is aresidue of a previous version and will be removed soon).
%% Representation of edge formulas is explained in the README file. The important point is that the first
%% clause fires when the edge formula contains only one literal and the second clause fires otherwise.
%% The theorem proving works as follows. The Edge formulas EF2 and EF1 are passed to proveFormula(+, +, +, -).
%% We want to prove EF2 --> EF1. Prove formula (2nd clause) strips the first literal from EF2 leaving a
%% Node formula say NF, which is passed to assertDisjunct(+, +, +, -) after asserting the literal into the database
%% then assertDisjunct(+, +, +, -) breaks NF into individual edge formulas and passes each one to proveFormula(+, +, +, -)
%% one by one. Note that EF1 is unaffected so far. proveFormula and assertDisjunct keep calling each other
%% mutually, breaking down the complex EF2 and asserting literals into the database. The 1st clause of
%% proveFormula(+, +, +, -) fires when one path of EF2 (except the root node) is has been stripped and asserted.
%% This root literal Lit is then asserted. Now the database contains one path of EF2. Now EF1 is "prove"d
%% (See prove(+, +)). If it succeeds, Res is bound to 1. The mutual calling now recoils and extends again to
%% assert another path into the database and EF1 is "prove"d again. This goes on until all paths of EF2 have been
%% exhausted. Finally, Res is the product of all the "Res" results of individual paths. Thus Res, is 1 at the
%% end only is all paths have individually managed to prove EF1. This proves the implication we want to prove.
%% More details and a trace of a real proof is provided in the README file.



proveFormula1([Lit], Path, QForm, Res) :- 
    QForm =.. [_ | V],
    getBasicBkgd([Lit | Path], V, Objs, ExPath),
    (member(QForm, ExPath),
     Res = 1
     ;
     checkImplication1(ExPath, QForm, Objs, Res)),
    !.
proveFormula1([Lit | Form], Path, QForm, Res) :-
%    write('three'), nl, 
     assertDisjunct1(Form, [Lit | Path], QForm, Res).




proveFormula([Lit], Path, QForm, QVars, Res) :-
%    write('two'), nl, 
    getBasicBkgd([Lit | Path], QVars, Objs, ExPath),
    (debug(4) -> write('Path: '), write(ExPath), nl ; true),
    (not(pathSatisfiable(ExPath, ExPath)), 
     (debug(4) -> write('path is unsat'), nl ; true),
      Res = 1
      ;
      once(assertEx(ExPath)),
      (debug(4) -> write('path asserted: '), write(ExPath), nl ; true),
      once(goal3579(QForm1)),
      (not(not(call(QForm1))),
       Res = 1
       ;
       checkImplication(ExPath, QForm1, Objs, Res)),
      once(retractEx(ExPath)),
      (debug(4) -> write('path retracted'), nl ; true)),
    !.
proveFormula([Lit | Form], Path, QForm, QVars, Res) :-
%    write('three'), nl, 
     assertDisjunct(Form, [Lit | Path], QForm, QVars, Res).

 

 
proveFormulaTiming(A, B, C, D, E) :-
    statistics(runtime, [T1, _]),
    proveFormula(A, B, C, D, E),
    statistics(runtime, [T2, _]),
    T3 is T2 - T1,
    T4 is T3/1000,
    retract(t3579(r7, T5)),
    T6 is T5 + T4,
    asserta(t3579(r7, T6)).



%% assertDisjunct(+, +, +, -)
%% assertDisjunct(NF2, [], EF1, Res)
%% This procedured works hand in glove with proveFormula(+, +, +, -). See ProveFormula(+, +, +, -) for details.

assertDisjunct([First], Path, QForm, QVars, Res) :-
    !,
    proveFormula(First, Path, QForm, QVars, Res),
    !.
assertDisjunct([First | Rest], Path, QForm, QVars, Res) :-
    proveFormula(First, Path, QForm, QVars, Res1),
    !,
    (Res1 = 0,
     Res = 0
     ;
     assertDisjunct(Rest, Path, QForm, QVars, ResAll),
     Res is Res1 * ResAll).






assertDisjunct1([First], Path, QForm, Res) :-
    !,
    proveFormula1(First, Path, QForm, Res),
    !.
assertDisjunct1([First | Rest], Path, QForm, Res) :-
    proveFormula1(First, Path, QForm, Res1),
    !,
    (Res1 = 0,
     Res = 0
     ;
     assertDisjunct1(Rest, Path, QForm, ResAll),
     Res is Res1 * ResAll).





proveFormula2([Lit], Path, QForm, QVars, Res) :-
    getBasicBkgd([Lit | Path], QVars, Objs, ExPath1),
    floodState(ExPath1, Objs, ExPath),
    (debug(4) -> write('Path: '), write(ExPath), nl ; true),
    (not(pathSatisfiable(ExPath, ExPath)), 
      (debug(4) -> write('path is unsat'), nl ; true),
      Res = 1
      ;
%      once(assertEx(ExPath)),
%      (debug(4) -> write('path asserted: '), write(ExPath), nl ; true),
%%      once(goal3579(QForm1)),
%      (not(not(call(QForm))),
	not(not(prove(QForm, ExPath))),
       Res = 1
       ;
       Res = 0),
%      once(retractEx(ExPath)),
%      (debug(4) -> write('path retracted'), nl ; true)),
    !.
proveFormula2([Lit | Form], Path, QForm, QVars, Res) :-
%    write('three'), nl, 
     assertDisjunct(Form, [Lit | Path], QForm, QVars, Res).









canProve([], _) :- 
    !.
canProve([X | XList], Ex) :- 
    member(X, Ex),
    canProve(XList, Ex),
    !.
canProve([X | XList], Ex) :-
    rule(X, _, Y),
    once(conc(Y, XList, XYList)),
    canProve(XYList, Ex).




implication(State, []).
implication(State, [Lit | Rest]) :- 
    member(Lit, State),
    implication(State, Rest).
implication(State, [Lit | Rest]) :- 
    rule(Lit, _, Body),	   	   
    implication(State, Body),
    implication(State, Rest).

