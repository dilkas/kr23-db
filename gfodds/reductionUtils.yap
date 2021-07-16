:- dynamic 
    indexCount/1,
    nf3579/2,
    ss3579/5,
    reach3579/6.

varToNumConv([], []) :- 
    !.
varToNumConv([Var | Rest], [Num | Rest1]) :- 
    name(Var, [118 | NumList]),
    name(Num, NumList),
    varToNumConv(Rest, Rest1).
varToNumConv([X | Rest], [X | Rest1]) :-
    varToNumConv(Rest, Rest1).

numToVarConv([], []) :- 
    !.
numToVarConv([Num | Rest], [Var | Rest1]) :-
    integer(Num),
    name(Num, NumList),
    name(Var, [118 | NumList]),
    numToVarConv(Rest, Rest1).
numToVarConv([X | Rest], [X | Rest1]) :-
    numToVarConv(Rest, Rest1).



%% getParents(+, +, -)
%% getParents(I, D, P)
%% True when D is a FODD, I is the index of a node in D and P is a list of the 
%% parent nodes of I. 

getParents(_, [], []) :- 
    !.
getParents(I, [node(I1, Lit, I, R) | Rest], [node(I1, Lit, I, R) | Rest1]) :- 
    getParents(I, Rest, Rest1),
    !.
getParents(I, [node(I1, Lit, L, I) | Rest], [node(I1, Lit, L, I) | Rest1]) :- 
    getParents(I, Rest, Rest1),
    !.
getParents(I, [_ | Rest], Rest1) :- 
    getParents(I, Rest, Rest1).






%% getNFVars(+, -)
%% getNFVars(NF, V)
%% True when V is a list of all the domain variables appearing in the node formula NF. 
%% Duplicates are removed.

getNFVars([], _, []) :- 
    !.
getNFVars([First], SoFar, Vars) :- 
    getEFVars(First, SoFar, Vars),
    !.
getNFVars([First | Rest], SoFar, Vars) :- 
    getEFVars(First, SoFar, V1),
    getNFVars(Rest, V1, Vars).



%% getEFVars(+, -)
%% getEFVars(EF, V)
%% True when V is a list of all the domain variables appearing in the edge formula EF. 
%% Duplicates are removed.
 

getEFVars([Lit], SoFar, Vars) :- 
    Lit =.. [_ | V1],
    (all(X, (member(X, V1), domVar(X)), V) ; V = []),
    batchInsert(V, SoFar, Vars),
    !.
getEFVars([Lit | Form], SoFar, Vars) :- 
    Lit =.. [_ | V1],
    (all(X, (member(X, V1), domVar(X)), V) ; V = []),
    batchInsert(V, SoFar, V2),
    getNFVars(Form, V2, Vars).



%% replaceNFVars(+, +, -)
%% replaceNFVars(NF, K, F)
%% True when F is the node formula NF with of all the domain variables appearing in NF 
%% replaced according to key K 

replaceNFVars([], _, []) :- 
    !.
replaceNFVars([First], Key, [VarFirst]) :- 
    replaceEFVars(First, Key, VarFirst),
    !.
replaceNFVars([First | Rest], Key, [VarFirst | VarRest]) :- 
    replaceEFVars(First, Key, VarFirst),
    replaceNFVars(Rest, Key, VarRest).



%% replaceEFVars(+, +, -)
%% replaceEFVars(EF, K, F)
%% True when F is the edge formula EF with of all the domain variables appearing in EF 
%% replaced according to key K 

replaceEFVars([Lit], Key, [VarLit]) :- 
    variablizeTerm(Lit, Key, VarLit),
    !.
replaceEFVars([Lit | Form], Key, [VarLit | VarForm]) :- 
    variablizeTerm(Lit, Key, VarLit),
    replaceNFVars(Form, Key, VarForm).


%% replaceNodeFormVars(+, +, -)
%% replaceNodeFormVars(NF, K, F)
%% Same as replaceNFVars except that the node formula NF is 'interpreted' (see below)

replaceNodeFormVars((First ; Rest), Key, (VarFirst ; VarRest)) :- 
    replaceEdgeFormVars(First, Key, VarFirst),
    replaceNodeFormVars(Rest, Key, VarRest),
    !.
replaceNodeFormVars((First), Key, (VarFirst)) :- 
    replaceEdgeFormVars(First, Key, VarFirst).



%% replaceEdgeFormVars(+, +, -)
%% replaceEdgeFormVars(EF, K, F)
%% Same as replaceEFVars except that the edge formula EF is 'interpreted' (see below)

replaceEdgeFormVars((Lit, (Form)), Key, (VarLit, (VarForm))) :- 
    variablizeTerm(Lit, Key, VarLit),
    replaceNodeFormVars(Form, Key, VarForm),
    !.
replaceEdgeFormVars(Lit, Key, (VarLit)) :- 
    variablizeTerm(Lit, Key, VarLit).
    


%% parentUpdate(+, +, +, -, -)
%% parentUpdate(D1, C, P, D2, PL)
%% True when D2 is the FODD D1 with all parents of node P now pointing to node C. And PL is 
%% a list of those parents. Needed for R1 and R2

parentUpdate([], _, _, [], []) :- 
    !.
parentUpdate([node(I, L, Parent, Parent) | Rest], Child, Parent, [node(I, L, Child, Child) | Other], [I | OPL]) :- 
    parentUpdate(Rest, Child, Parent, Other, OPL),
    !.
parentUpdate([node(I, L, Parent, Rt) | Rest], Child, Parent, [node(I, L, Child, Rt) | Other], [I | OPL]) :- 
    parentUpdate(Rest, Child, Parent, Other, OPL),
    !.    
parentUpdate([node(I, L, Lt, Parent) | Rest], Child, Parent, [node(I, L, Lt, Child) | Other], [I | OPL]) :- 
    parentUpdate(Rest, Child, Parent, Other, OPL),
    !.    
parentUpdate([N | Rest], Child, Parent,[N | Other], PL) :- 
    parentUpdate(Rest, Child, Parent, Other, PL).



%% Note: Since the FODDs are represented as lists, the routine collectNodeFormula (below) 
%% returns a node formula in the form of a list. However, if this formula has to be queried 
%% against a prolog database as required by our theorem prover, then it must be represented 
%% as a prolog clause. We call this conversion 'interpretation' of the formula. 



batchParentUpdate([], _, _, [], []) :- 
    !.
batchParentUpdate([node(I, Lit, L, R) | Rest], Child, ParentNodes, [node(I, Lit, Child, Child) | Other], [I | OPL]) :- 
    once(member(L, ParentNodes)),
    member(R, ParentNodes),
    batchParentUpdate(Rest, Child, ParentNodes, Other, OPL),
    !.
batchParentUpdate([node(I, Lit, L, R) | Rest], Child, ParentNodes, [node(I, Lit, Child, R) | Other], [I | OPL]) :- 
    member(L, ParentNodes),
    batchParentUpdate(Rest, Child, ParentNodes, Other, OPL),
    !.
batchParentUpdate([node(I, Lit, L, R) | Rest], Child, ParentNodes, [node(I, Lit, L, Child) | Other], [I | OPL]) :- 
    member(R, ParentNodes),
    batchParentUpdate(Rest, Child, ParentNodes, Other, OPL),
    !.
batchParentUpdate([node(I, Lit, L, R) | Rest], Child, ParentNodes, [node(I, Lit, L, R) | Other], PL) :- 
    batchParentUpdate(Rest, Child, ParentNodes, Other, PL).




get_leaves([], []) :- 
    !.
get_leaves([node(I, Lit, -1, -1) | Rest], [leaf(Lit, I) | Rest1]) :-
    get_leaves(Rest, Rest1),
    !.
get_leaves([_ | Rest], Rest1) :-
    get_leaves(Rest, Rest1).





getLeafIndices([], []) :- 
    !.
getLeafIndices([node(I, _, -1, -1) | Rest], [I | Other]) :- 
    getLeafIndices(Rest, Other),
    !.
getLeafIndices([_ | Rest], Other) :-
    getLeafIndices(Rest, Other).


%% getDisjunct(+, -)
%% getDisjunct(NF, (EF1 ; NF2))
%% True when the node formula NF is separated as EF1 (an interpreted edge formula) and 
%% NF2 (NF without the edge formula EF1).
 
getDisjunct([], true) :- 
    !.
getDisjunct([First], (One)) :- 
    interpretFormula(First, One), 
    !.
getDisjunct([First | Rest], ((One) ; (Other))) :- 
    interpretFormula(First, One),
    getDisjunct(Rest, Other).



%% interpretFormula(+, -)
%% interpretFormula(EF, EF2)
%% True when EF is an edge formula and EF2 is EF interpreted. 

interpretFormula([Lit], (Lit)) :- 
    !.
interpretFormula([Lit | Form], ((Lit), (IForm))) :- 
    getDisjunct(Form, IForm).

    
%% getNodeFormula(+, +, -)
%% getNodeFormula(I, D, NF)
%% True when NF is the interpreted node formula of node I in FODD D.

getNodeFormula(I, D, NF) :- 
    collectNodeFormula(I, D, NF1),
    getDisjunct(NF1, NF).



%% getEdgeFormula(+, +, +, -)
%% getEdgeFormula(edge(I, Lit, C), D, V, EF)
%% True when EF is the interpreted edge formula for the edge C of node I (with literal Lit), in FODD D 
%% with domain variables in list V appearing in the edge formula replaced by new prolog variables.

getEdgeFormula(edge(I, Lit, C), D, Vars, EF) :- 
    collectNodeFormula(I, D, NF),
    (C = t,
     !,
     NLit = Lit
     ;
     getNeg(Lit, NLit)),
    getEFVars([NLit | NF], [], EFVars),
    diff(EFVars, Vars, V),
    getSubsKey(V, Key),
    replaceEFVars([NLit | NF], Key, VarEF),
    interpretFormula(VarEF, EF).
  


getEdgeFormula1(edge(I, Lit, C), D, Vars, EF) :- 
    collectNodeFormula(I, D, NF),
    (C = t,
     !,
     NLit = Lit
     ;
     getNeg(Lit, NLit)),
    getEFVars([NLit | NF], [], EFVars),
    diff(EFVars, Vars, V),
    getSubsKey(V, Key),
    replaceEFVars([NLit | NF], Key, EF).
%    interpretFormula(VarEF, EF).





%% collectNodeFormula(+, +, -)
%% collectNodeFormula(I, D, NF)
%% True when NF is the uninterpreted node formula for node I in FODD D. 
%% This routine also asserts the NF into the prolog database for reference.


collectNodeFormula(I, D, NF) :- 
    recorded(nf,arc(I, NF)),
    !.
collectNodeFormula(I, D, NF) :- 
    (all([XLit | NF1], (member(node(XI, XLit, I, _), D), once(collectNodeFormula(XI, D, NF1))), NFt) ; NFt = []),
     (all([NXLit | NF1], (member(node(XI, XLit, _, I), D), once((getNeg(XLit, NXLit), collectNodeFormula(XI, D, NF1)))), NFf) ; NFf = []),
     conc(NFt, NFf, NF),
     recorda(nf,arc(I, NF),_).



%% descendent(+, -, +)
%% descendent(I1, I2, D)
%% True when node I1 is a descendent of node I2 in FODD D
    
descendent(I, I, _) :- 
    !.
descendent(I1, I2, D) :- 
    member(node(I2, _, L, R), D),
    L \= -1,
    !,
    (descendent(I1, L, D)
     ;
     descendent(I1, R, D)).



%% related(+, +, +)
%% related(I1, I2, D)
%% True when node I1 is a descendent of node I2 or node I2 is a descendent of 
%% node I1 in FODD D

related(I1, I2, D) :- 
    (descendent(I1, I2, D)
     ;
     descendent(I2, I1, D)).

  


%% getP(+, +, -)
%% getP(P1, P2, P)
%% During supersub (see below), construction of every node I in the resultant FODD requires 
%% acquiring the two subFODDs L and R rooted at its left child. We invent a variable P 
%% associated with each subFODD generated in the supersub process that assumes the value 1 
%% when all leaves of that subFODD are positive, -1 when all leaves are negative, 0 when the 
%% subFODD is [node(0, 0.0, -1, -1)] and 2 when some leaves are positive and some are negative. 
%% The value of P is a shortcut to figure out if some subFODD are is clearly greater than 
%% another or not. get(P1, P2, P) is true when P reflects the variable value associated with 
%% the entire FODD whose children are associated with the variable values P1 and P2.    

getP(X, X, X) :- 
    !.
getP(X, 0, X) :- 
    !.
getP(0, X, X) :- 
    !.
getP(_, _, 2).



%% superSub1(+, +, +, +, -, -, -)
%% superSub1(N1, D1, N2, D2, I, V, P)
%% True when the subFODD of FODD D1 rooted at node N1 is greater than or equal to the 
%% subFODD of FODD D2 rooted at node N2. 

superSub1(node(I1, N1, L1, R1), _, node(I2, N2, L2, R2), _, I, V, P) :- 
    (debug(5) -> (write('I1: '), write(node(I1, N1, L1, R1)), write(' I2: '), write(node(I2, N2, L2, R2)), nl) ; true),
%    (debug(5) -> (ss3579(Ia, Ib, node(Ic, Litc, Lc, Rc), Vc, Pc), write(ss3579(Ia, Ib, node(Ic, Litc, Lc, Rc), Vc, Pc)), nl, fail ; true) ; true),
%    indexCount(X), write('indexCount = '), write(X), 
%    (debug(5) -> nl ; true), 
    ss3579(I1, I2, node(I, _, _, _), V, P),
    (debug(5) -> (write('found1 '), write(ss3579(I1, I2, node(I, _, _, _), V, P)), nl) ; true),
    (debug(5) -> write('CACHED: '), write(N1), write(' x '), write(N2), nl ; true),    
    (debug(5) -> write('returned V = '), write(V), write(' I = '), write(I), write(' P = '), write(P), nl ; true),
    !.
%superSub1(node(I1, _, _, _), _, node(I2, _, _, _), _, I, V, P) :- 
%    ss3579(I2, I1, node(_, Lit, L, R), V, P1),
%    (debug(5) -> (write('found2 '), write(ss3579(I2, I1, node(_, _, _, _), V, P1)), nl) ; true),
%    getIndexCount(I),
%    (P1 = 1,
%     P = -1
%     ;
%     P1 = -1,
%     P = 1
%     ;
%     P = P1),
%    asserta(ss3579(I1, I2, node(I, _, _, _), V, P)),
%    (debug(5) -> write('returned V = '), write(V), write(' I = '), write(I), write(' P = '), write(P), nl ; true),
%    !.
superSub1(node(I1, N1, -1, -1), _, node(I2, N2, -1, -1), _, I, [], P) :-
    (debug(5) -> write(N1), write(' x '), write(N2), nl ; true),
    N is N1 - N2,
%    write(N), nl, 
%    (ss3579(I1, I2, node(I, N, -1, -1), [], P),
%     (debug(5) -> (write('found3 '), write(ss3579(I1, I2, node(I, N, -1, -1), [], P)), nl) ; true)
%     ;
     getIndexCount(I),
%     write(I), nl, 
     (N > 0,
      P = 1
      ;
      N < 0,
      P = -1
      ;
      P = 0),
     asserta(ss3579(I1, I2, node(I, N, -1, -1), [], P)),
    (debug(5) -> write('returned V = '), write([]), write(' I = '), write(I), write(' P = '), write(P), nl ; true),
    !.
superSub1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, I, V, P) :- 
    order(N1, N2, lt),
    (debug(5) -> write(N1), write(' lt '), write(N2), nl ; true),
    member(node(L1, Llit, Llt, Lrt), D1),
    member(node(R1, Rlit, Rlt, Rrt), D1),
    !,
    superSub1(node(L1, Llit, Llt, Lrt), D1, node(I2, N2, L2, R2), D2, L, V1, P1),
    !,
    superSub1(node(R1, Rlit, Rlt, Rrt), D1, node(I2, N2, L2, R2), D2, R, V2, P2),
    (L = R,
     I = L,
     V = V1,
     P = P1
     ;
     ss3579(_, _, node(I, N1, L, R), V, P)
     ;
     getIndexCount(I),
     getP(P1, P2, P),
     (P = 2 -> V = [] ; batchInsert(V1, V2, V))),
      asserta(ss3579(I1, I2, node(I, N1, L, R), V, P)),    
    (debug(5) -> write('V1 = '), write(V1), write(' L = '), write(L), write(' V2 = '), write(V2), write(' R = '), write(R), nl ; true),
    (debug(5) -> write('returned V = '), write(V), write(' I = '), write(I), write(' P = '), write(P), nl ; true),
    !.
superSub1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, I, V, P) :- 
    order(N1, N2, gt),
    (debug(5) -> write(N1), write(' gt '), write(N2), nl ; true),
    member(node(L2, Llit, Llt, Lrt), D2),
    (debug(5) -> write(node(L2, Llit, Llt, Lrt)), nl ; true),
    member(node(R2, Rlit, Rlt, Rrt), D2),
    (debug(5) -> write(node(R2, Rlit, Rlt, Rrt)), nl ; true),
    !,
    superSub1(node(I1, N1, L1, R1), D1, node(L2, Llit, Llt, Lrt), D2, L, V1, P1),
    !,
    superSub1(node(I1, N1, L1, R1), D1, node(R2, Rlit, Rlt, Rrt), D2, R, V2, P2), 
%    write('P1 = '), write(P1), write(' P2 = '), write(P2), nl, 
%    write('L = '), write(L), write(' R = '), write(R), nl, 
    (L = R,
     I = L,
     V = V1,
     P = P1
     ;
     ss3579(Ia, Ib, node(I, N2, L, R), V, P),
     (debug(5) -> (write('found4 '), write(ss3579(Ia, Ib, node(I, N2, L, R), V, P)), nl) ; true)
     ;
     getIndexCount(I),    
     getP(P1, P2, P),
 %    write('Here P = '), write(P), nl, 
     (P = 2 -> V = [] ; batchInsert(V1, V2, V))),
     asserta(ss3579(I1, I2, node(I, N2, L, R), V, P)),
    (debug(5) -> write('V1 = '), write(V1), write(' L = '), write(L), write(' V2 = '), write(V2), write(' R = '), write(R), nl ; true),
    (debug(5) -> write('returned V = '), write(V), write(' I = '), write(I), write(' P = '), write(P), nl ; true),
    !.
superSub1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, I, V, P) :- 
    (debug(5) -> write(N1), write(' eq '), write(N2), nl ; true),
    N1 =.. [_ | V00],
%    extractVarsFromVarConsts(V000, V00),
    sort(V00, V0),
    member(node(L1, Llit1, Llt1, Lrt1), D1),
    member(node(R1, Rlit1, Rlt1, Rrt1), D1),
    member(node(L2, Llit2, Llt2, Lrt2), D2),
    member(node(R2, Rlit2, Rlt2, Rrt2), D2),
    (debug(5) -> write('L1 = '), write(L1), write(' R1 = '), write(R1), write(' L2 = '), write(L2), write(' R2 = '), write(R2), nl ; true),
    !,
    superSub1(node(L1, Llit1, Llt1, Lrt1), D1, node(L2, Llit2, Llt2, Lrt2), D2, L, V1, P1),
    !, 
    superSub1(node(R1, Rlit1, Rlt1, Rrt1), D1, node(R2, Rlit2, Rlt2, Rrt2), D2, R, V2, P2), 
    getP(P1, P2, P12),
%    write('P12 = '), write(P12), nl, 
    (P12 = 2,
     V = [], 
     getIndexCount(I),
     P = P12
     ;
%     write('V1 = '), write(V1), write(' V2 = '), write(V2), nl,  
     batchInsert(V1, V2, V12),
     (debug(5) -> write('L = '), write(L), write(' R = '), write(R), nl ; true),
     (my_intersection(V0, V12, V0),
      (debug(5) -> write('V is a subset of V1 union V2'), nl ; true),
      (L = R,
       I = L,
       V = V1,
       P = P1
       ;
       ss3579(_, _, node(I, N1, L, R), V, P)
       ;
       getIndexCount(I),
       V = V12,
       getP(P1, P2, P)),
       asserta(ss3579(I1, I2, node(I, N1, L, R), V, P))
      ;     
      once(superSub1(node(L1, Llit1, Llt1, Lrt1), D1, node(R2, Rlit2, Rlt2, Rrt2), D2, LR, V3, P3)),
      once(superSub1(node(R1, Rlit1, Rlt1, Rrt1), D1, node(L2, Llit2, Llt2, Lrt2), D2, RL, V4, P4)), 
      once(getP(P1, P3, PL)),
      once(getP(P2, P4, PR)),
      once(getP(PL, PR, P)),
      P \= 2,
      (debug(5) -> write('It is ok to std apart'), nl ; true),
      (L = LR,
       IL = L,
       VL = V1
       ;
       ss3579(_, _, node(IL, N1, L, LR), VL, PL)
       ;
       getIndexCount(IL),
       batchInsert(V1, V3, VL)),
      asserta(ss3579(L1, I2, node(IL, N2, L, LR), VL, PL)),
      (R = RL,
       IR = R,
       VR = V2
       ;
       ss3579(_, _, node(IR, N1, RL, R), VR, PR)
       ;
       getIndexCount(IR),
       batchInsert(V2, V4, VR)),
      asserta(ss3579(R1, I2, node(IR, N2, RL, R), VR, PR)),
      (IL = IR, 
       I = IL,
       V = VL
       ; 
       ss3579(_, _, node(I, N1, IL, IR), V, P)
       ;
       getIndexCount(I),      
       batchInsert(VL, VR, V)),
      asserta(ss3579(I1, I2, node(I, N1, IL, IR), V, P))
      ;
      (debug(5) -> write('cannot std apart'), nl ; true),
      (L = R,
       I = L,
       (debug(5) -> write('L=R'), nl ; true),
       V = V1,
       P = P1
       ;
       ss3579(_, _, node(I, N1, L, R), V, P),
       (debug(5) -> write(node(I, N1, L, R)), nl ; true)
       ;
       getIndexCount(I),
       batchInsert(V0, V12, V),
       getP(P1, P2, P)),
       asserta(ss3579(I1, I2, node(I, N1, L, R), V, P)))),
     (debug(5) -> write('V1 = '), write(V1), write(' L = '), write(L), write(' V2 = '), write(V2), write(' R = '), write(R), nl ; true),
    (debug(5) -> write('V3 = '), write(V3), write(' LR = '), write(LR), write(' V4 = '), write(V4), write(' RL = '), write(RL), nl ; true),
    (debug(5) -> write('V0 = '), write(V0), write(' V12 = '), write(V12), write(' V012 = '), write(V012), nl ; true),
    (debug(5) -> write('returned V = '), write(V), write(' I = '), write(I), write(' P = '), write(P), nl; true).





%%%%%%%%%% SUPERSUB SHORTCUTS BEGIN %%%%%%%%%%%%%
superSub(_, _, node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], []) :- 
    !.
superSub(node(I, _, _, _), D, node(I, _, _, _), D, V) :- 
    getDVars(I, D, V),
    !.
%%%%%%%%%% SHORTCUTS END  %%%%%%%%%%%%%


%% superSub(+, +, +, +, -)
%% superSub(N1, D1, N2, D2, V)
%% True when the subFODD of FODD D1 rooted at node N1 is greater than or equal to the 
%% subFODD of FODD D2 rooted at node N2, when D1 and D2 are std apart over all variables 
%% except those discovered in V.


superSub(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, V) :- 
%      asserta(indexCount(2)),
    (superSub1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, _, V1, P) -> true ; (write('superSub1 failed'), nl)),
      
%      retract(indexCount(_)),
      extractVarsFromVarConsts(V1, V),
      !,
    (P = 1 ; P = 0).




%% eraseSubDDNF(+, +)
%% eraseSubDDNF(I, D)
%% retracts from the prolog database, node formulas of all nodes in the subFODD of 
%% FODD D rooted at node I 

eraseSubDDNF(I, D) :- 
      member(node(I, _, L, R), D),
      retractall(nf3579(I, _)),
      retractall(reach3579(I, _, _, _, _, _)),
      retractall(reach3579(_, _, I, _, _, _)),
      (L = -1
       ;
       eraseSubDDNF(L, D),
       eraseSubDDNF(R, D)).
      




%% eraseSuperss(+, +)
%% eraseSuperss(I, D)
%% retracts from the prolog database, all records of the smart-subtract operation for all 
%% ancestors of node I in FODD D. 

eraseSuperss(I, D) :- 
    retractall(ss3579(I, _, _, _, _)),
    retractall(ss3579(_, I, _, _, _)),
    (all(X, ((member(node(X, _, I, _), D) ; member(node(X, _, _, I), D)), eraseSuperss(X, D)), _) ; true).







%% replaceDVars(+, +, -)
%% replaceDVars(D, K, R)
%% True when R is the FODD D with all variables replaced according to key K.

replaceDVars([], _, []) :- 
    !.
replaceDVars([node(I, Lit, L, R) | Rest], Key, [node(I, Lit1, L, R) | Rest1]) :- 
    variablizeTerm(Lit, Key, Lit1),
    replaceDVars(Rest, Key, Rest1).


%% hasParent(+, +)
%% hasParent(I, D)
%% True when node I in FODD D has atleats one  parent
    
hasParent(I, [node(_, _, I, _) | _]) :- 
    !.
hasParent(I, [node(_, _, _, I) | _]) :- 
    !.
hasParent(I, [node(P, _, _, _) | D]) :- 
    P > I,
    hasParent(I, D).
    
 

%% getSubDD(+, +, -)
%% getSubDD(I, D, S)
%% True when S is the subFODD of FODD D rooted at node I.
   

getSubDD(I, [node(I, Lit, L, R) | Rest], [node(I, Lit, L, R) | Rest]) :- 
    !.
getSubDD(I, D, SubDD) :- 
    member(node(I, Lit, L, R), D),
    (L \= -1 -> getSubDD(L, D, LSub), 
     batchInsert([node(I, Lit, L, R)], LSub, SubDDL),
     getSubDD(R, D, RSub),
     batchInsert(SubDDL, RSub, SubDD)
     ;
     SubDD = [node(I, Lit, L, R)]).


%% diffPosition(+, +, -, -, -)
%% diffPosition(L1, L2, A, B, C)
%% True when list L1 and list L2 differ at a particular position. L1 has element A in that 
%% position, and L2 has element B in that position. A \= B. C is the prefix set of elements 
%% before A in L1 and B in L2 that is common to the 2 lists.
    
diffPosition(X, Y, -1, -1, []) :- 
    (X = [] ; Y = []),
    !.
diffPosition([X | Rest], [X | Rest1], A, B, [X | Comm]):- 
    diffPosition(Rest, Rest1, A, B, Comm),
    !.
diffPosition([X | _], [Y | _], X, Y, []).




%% nonV1V2diff(+, +, +, +, -, -, -)
%% nonV1V2diff(L1, L2, V1, V2, A, B, Comm)
%% True when A and B are computer by diffPosition except they are the first differing 
%% positions that are not V1 and V2.

nonV1V2diff(L1, L2, _, _, -1, -1) :- 
    (L1 = [] ; L2 = []),
    !.
nonV1V2diff(L1, L2, V1, V2, A, B) :- 
    diffPosition(L1, L2, A1, B1, Comm1),
    (A1 = V1, 
     B1 = V2,
     conc(Comm1, [V1], Comm2),
     delAll(Comm2, L1, L11),
     conc(Comm1, [V2], Comm3),
     delAll(Comm3, L2, L22),
     nonV1V2diff(L11, L22, V1, V2, A, B)
     ;
     A = A1,
     B = B1).



%% getHighestChildVar(+, +, +, +, -)
%% getHighestChildVar(L, D, V1, SF, V)
%% True when L is the list of terms that will be affected if domain variable V1 
%% is replaced by something else and V is the lexicographically smallest domain 
%% variable among the domain variables of the children of the nodes in L. If V1 
%% is replaced by any domain variable greater than V, D would become unsorted. 
    
getHighestChildVar([], _, _, _, V, V) :- 
    !.
getHighestChildVar([rt(_, [P | Vars], L, R) | Rest], D, V1, V2, SoFar, V) :- 
      member(node(L, LLit, _, _), D),
      LLit =.. [LP | LVars],
      (LP \== P,
       MinSoFar1 = SoFar
       ;
       diffPosition([P | Vars], [LP | LVars], XL, YL, Comm1),
       (once(XL = V1),
	YL = V2,
	nonV1V2diff([P | Vars], [LP | LVars], V1, V2, A, B),
	!,
	(A = -1, 
	 B = -1
	 ;
	 getVarOrder(A, B, lt)),
	min(SoFar, YL, MinSoFar1)
	;
	min(SoFar, YL, MinSoFar1))),
%	MinSoFar1 = SoFar)),
      member(node(R, RLit, _, _), D),
      RLit =.. [RP | RVars],
      (RP \== P,
       MinSoFar = MinSoFar1
       ;
       diffPosition([P | Vars], [RP | RVars], XR, YR, _),
       (once(XR = V1),
	YR = V2,
	nonV1V2diff([P | Vars], [RP | RVars], V1, V2, C, E),
	!,
	(C = -1, 
	 E = -1
	 ;
	 getVarOrder(C, E, lt)),
	min(MinSoFar1, YR, MinSoFar)
	;
	min(MinSoFar1, YR, MinSoFar))),
%	MinSoFar = MinSoFar1)),
       !,
       getHighestChildVar(Rest, D, V1, V2, MinSoFar, V).
       


%% getLowestVar(+, +, +, +, -)
%% getLowestVar(L, N, V1, SF, V)
%% True when L is the list of parents of node N. V is the lexicographically largest 
%% domain variable among the domain variables of the nodes in L. If V1 
%% is replaced by any domain variable smaller than V, D would become unsorted. 

    
getLowestVar([], _, _, _, V, V) :- 
    !.   
getLowestVar([node(_, Lit, _, _) | Rest], [P | _], _, _, V, V) :-
      once(Lit =.. [P1 | _]),
      P1 \== P,
      !.
getLowestVar([node(_, Lit, _, _) | Rest], [P | Vars], V1, V2, SoFar, V) :- 
    Lit =.. [P | Vars1],
    diffPosition([P | Vars], [P | Vars1], X, Y, _),
    (once(X = V1),
     Y = V2,
     nonV1V2diff([P | Vars], [P1 | Vars1], V1, V2, A, B),
     !,
     (A = -1, 
      B = -1
      ;
      getVarOrder(A, B, gt)),
     max(Y, SoFar, MaxSoFar)
     ;
     max(Y, SoFar, MaxSoFar)),
%     MaxSoFar = SoFar),
      !,
    getLowestVar(Rest, [P | Vars], V1, V2, MaxSoFar, V).
    
    
%% getLowestParentVar(+, +, +, +, -)
%% getLowestParentVar(L, D, V1, SF, V)
%% True when L is the list of terms that will be affected if domain variable V1 
%% is replaced by something else and V is the lexicographically largest domain 
%% variable among the domain variables of the parents of the nodes in L. If V1 
%% is replaced by any domain variable smaller than V, D would become unsorted. 
    
getLowestParentVar([], _, _, _, V, V) :- 
    !.
getLowestParentVar([rt(I, [P | Vars], _, _) | Rest], D, V1, V2, SoFar, V) :- 
    getParents(I, D, Par),
    getLowestVar(Par, [P | Vars], V1, V2, SoFar, MaxSoFar),
    getLowestParentVar(Rest, D, V1, V2, MaxSoFar, V).



%% getReplTerms(+, +, -)
%% getReplTerms(D, V, L)
%% True when L is a list of terms that would change if domain variable V1 
%% were to be replaced by something else (which is basically the list of 
%% nodes in FODD D containing variable V1).


getReplTerms([], _, []) :- 
    !.
getReplTerms([node(I, Lit, L, R) | Rest], V1, [rt(I, [P | Vars], L, R) | Rest1]) :- 
    Lit =.. [P | Vars],
    member(V1, Vars),
    getReplTerms(Rest, V1, Rest1),
    !.
getReplTerms([_ | Rest], V1, Rest1) :- 
     getReplTerms(Rest, V1, Rest1).





%% getReplVar(+, +, -)
%% getReplVar(D, [V1, V2], R)
%% True when R is the domain variable such that it is safe (does not result in an unsorted FODD) 
%% to replace domain variables V1 and V2 by R in FODD D.

    
getReplVar(_, _, [V, V], V) :- 
%    write('one'), nl, 
    !.
getReplVar(D, BnL, [V1, V2], ReplVar) :- 
%    write('two'), nl, 
    once(getVarOrder(V1, V2, lt)),
    !,
    once(getReplTerms(BnL, V1, ReplTerms1)), 
    (ReplTerms1 = [],
     ReplVar = V2
     ;
     once(getHighestChildVar(ReplTerms1, BnL, V1, V2, w, Ceil)),
     (Ceil == w,
      ReplVar = V2
      ;
      getVarOrder(V2, Ceil, gt),
      !,
      once(domVar(V2)),
      once(getReplTerms(D, V2, ReplTerms2)),
      once(getLowestParentVar(ReplTerms2, D, V2, V1, u, Floor2)),
      Floor2 @< Ceil,
      max(V1, Floor2, Floor),
%      write('This happened'), nl, 
      !,
      Floor @< Ceil,
      getIntermediateVar(Floor, Ceil, ReplVar1),
      getDVars(D, DVars),
      padOneTillDiff(ReplVar1, DVars, ReplVar)
      ;
      ReplVar = V2)),
    !.
getReplVar(D, BnL, [V1, V2], ReplVar) :- 
%    write('three'), nl, 
    getVarOrder(V1, V2, gt),
    !,
    once(getReplTerms(BnL, V1, ReplTerms1)), 
    (ReplTerms1 = [],
     ReplVar = V2
     ;
     once(getLowestParentVar(ReplTerms1, BnL, V1, V2, u, Floor)),
     (Floor == u,
      ReplVar = V2
      ;
      getVarOrder(V2, Floor, lt),    
      !,
      domVar(V2),
      once(getReplTerms(D, V2, ReplTerms2)),
      once(getHighestChildVar(ReplTerms2, D, V2, V1, w, Ceil2)),
      Ceil2 @> Floor,
      min(V1, Ceil2, Ceil),
      !,
      Floor @< Ceil,
%      write('This happened'), nl, 
      getIntermediateVar(Floor, Ceil, ReplVar1),
      getDVars(D, DVars),
      padOneTillDiff(ReplVar1, DVars, ReplVar)
      ;
      ReplVar = V2)).




%% padOneTillDiff(+, +, -)
%% padOneTillDiff(X, DV, Y)
%% True when Y is a domain variable created by padding 1s to the end of domain 
%% variable X until it becomes a new variable (one that does not appear in the FODD 
%% domain variables DV).

padOneTillDiff(X, DVars, X) :- 
    not(member(X, DVars)),
    !.
padOneTillDiff(X, DVars, ReplVar) :- 
    name(X, [118 | Num]),
    conc(Num, [49], Num1),
    name(Y, [118 | Num1]),
    padOneTillDiff(Y, DVars, ReplVar).



%% getIntermediateVar(+, +, -)
%% getIntermediateVar(F, C, V)
%% True when V is the domain variable which is the lexicographic middle of the 
%% domain variables F ad C

getIntermediateVar(Floor, Ceil, V) :- 
    name(Floor, [118 | F]),
    name(Ceil, [118 | C]),
    diffPosition(F, C, X, Y, Comm),
    Z is integer(floor((X + Y)/2)),
    conc([118 | Comm], [Z], Vname),
    name(V, Vname).



%% getHighestLeaf(+, +, +, -, -)
%% getHighestLeaf(D, L1, I1, L, I)
%% True when I is the node index and L is the value of the largest leaf in FODD D. 

getHighestLeaf([], L, I, L, I) :- 
    !.
getHighestLeaf([node(I, Lit, -1, -1) | Rest], SoFar, NodeSoFar, L1, I1) :- 
    max(Lit, SoFar, MaxSoFar),
    (Lit > SoFar -> MaxSoFar = Lit, NodeSoFar1 = I ; MaxSoFar = SoFar, NodeSoFar1 = NodeSoFar),
    getHighestLeaf(Rest, MaxSoFar, NodeSoFar1, L1, I1),
    !.
getHighestLeaf([_ | Rest], SoFar, NodeSoFar, L1, I1) :- 
    getHighestLeaf(Rest, SoFar, NodeSoFar, L1, I1).



get_all_edges([], []) :-
      !.
get_all_edges([node(_, _, -1, -1) | Rest], Edges) :-
      get_all_edges(Rest, Edges),
      !.
%get_all_edges([node(I, _, 0, _) | Rest], [I-f | Edges]) :-
%      get_all_edges(Rest, Edges),
%      !.
%get_all_edges([node(I, _, _, 0) | Rest], [I-t | Edges]) :-
%      get_all_edges(Rest, Edges),
%      !.
get_all_edges([node(I, _, _, _) | Rest], [I-t, I-f | Edges]) :-
      get_all_edges(Rest, Edges).




get_all_nonzero_target_edges([], []) :- 
    !.
get_all_nonzero_target_edges([node(_, _, -1, -1) | Rest], Edges) :-
    get_all_nonzero_target_edges(Rest, Edges),
    !.
get_all_nonzero_target_edges([node(I, _, 0, _) | Rest], [I-f | Edges]) :-
    get_all_nonzero_target_edges(Rest, Edges),
    !.
get_all_nonzero_target_edges([node(I, _, _, 0) | Rest], [I-t | Edges]) :-
    get_all_nonzero_target_edges(Rest, Edges),
    !.
get_all_nonzero_target_edges([node(I, _, _, _) | Rest], [I-t, I-f | Edges]) :-
    get_all_nonzero_target_edges(Rest, Edges).




get_paths(0, _, _, _, []) :- 
      !.
get_paths(I, D, SoFar, SoFarEdges, Paths) :-
      once(member(node(I, Lit, L, R), D)),
      L \= -1,       
      getNeg(Lit, NLit),
      get_paths(L, D, [Lit | SoFar], [I-t | SoFarEdges], LPaths),
      get_paths(R, D, [NLit | SoFar], [I-f | SoFarEdges], RPaths),
      conc(LPaths, RPaths, Paths),
      !.
get_paths(I, D, Path, EdgeList, [path(N, L, EdgeList, Path)]) :-
      member(node(I, N, -1, -1), D),
      length(Path, Len),
      N1 is 0 - N,
      L is 0 - Len.




get_all_paths(I, D, SoFar, SoFarEdges, Paths) :-
      once(member(node(I, Lit, L, R), D)),
      L \= -1,       
      getNeg(Lit, NLit),
      get_all_paths(L, D, [Lit | SoFar], [I-t | SoFarEdges], LPaths),
      get_all_paths(R, D, [NLit | SoFar], [I-f | SoFarEdges], RPaths),
      conc(LPaths, RPaths, Paths),
      !.
get_all_paths(I, D, Path, EdgeList, [path(N, L, EdgeList, Path)]) :-
      member(node(I, N, -1, -1), D),
      length(Path, Len),
      N1 is 0 - N,
      L is 0 - Len.






get_paths_rev(0, _, _, _, []) :- 
      !.
get_paths_rev(I, D, SoFar, SoFarEdges, Paths) :-
      once(member(node(I, Lit, L, R), D)),
      L \= -1,       
      getNeg(Lit, NLit),
%      insert(Lit, SoFar, PathSoFar),
%      insert(NLit, SoFar, NPathSoFar),
      get_paths_rev(L, D, [Lit | SoFar], [I-t | SoFarEdges], LPaths),
      get_paths_rev(R, D, [NLit | SoFar], [I-f | SoFarEdges], RPaths),
      conc(LPaths, RPaths, Paths),
      !.
get_paths_rev(I, D, Path, EdgeList, [path(N1, L, EdgeList, Path)]) :-
      member(node(I, N, -1, -1), D),
      length(Path, Len),
      N1 is 0 - N,
      L is 0 - Len.



add_action_to_paths(_, [], []) :- 
    !.
add_action_to_paths(Action, [path(N, L, EL, Path) | Rest], [path(N, L, EL, Path, Action) | Other]) :-
    add_action_to_paths(Action, Rest, Other).



get_edge_pairs([_], []) :- 
    !.
get_edge_pairs([X,Y | Rest], [[Y,X] | EdgePairs]) :-
    get_edge_pairs([Y | Rest], EdgePairs).



get_all_edge_pairs([], []) :- 
    !.
get_all_edge_pairs([node(_, _, -1, -1) | Rest], Other) :- 
    get_all_edge_pairs(Rest, Other),
    !.
get_all_edge_pairs([node(I, _, L, R) | Rest], Other) :- 
    once(member(node(L, _, -1, -1), Rest)),	
    member(node(R, _, -1, -1), Rest),		    
    get_all_edge_pairs(Rest, Other),
    !.
get_all_edge_pairs([node(I, _, L, R) | Rest], [[I-f,R-t], [I-f,R-f] | Other]) :- 
    member(node(L, _, -1, -1), Rest),	    
    get_all_edge_pairs(Rest, Other),
    !.
get_all_edge_pairs([node(I, _, L, R) | Rest], [[I-t,L-t], [I-t,L-f] | Other]) :- 
    member(node(R, _, -1,-1), Rest),	    
    get_all_edge_pairs(Rest, Other),
    !.
get_all_edge_pairs([node(I, _, L, R) | Rest], [[I-t,L-t], [I-t,L-f], [I-f,R-t], [I-f,R-f] | Other]) :- 
    get_all_edge_pairs(Rest, Other).





interpretPath([Lit], (Lit)) :- 
      !.
interpretPath([Lit | Rest], (Lit, (Other))) :-
      interpretPath(Rest, Other).



eraseBatchSuperss([], _) :- 
      !.
eraseBatchSuperss([I-E | Rest], D) :-
      eraseSuperss(I, D),
      eraseBatchSuperss(Rest, D).



eraseBatchNF([], _) :- 
      !.
eraseBatchNF([I-t | Rest], D) :-
      member(node(I, _, L, R), D),
      eraseSubDDNF(L, D),
      eraseBatchNF(Rest, D),
      !.
eraseBatchNF([I-f | Rest], D) :-
      member(node(I, _, L, R), D),
      eraseSubDDNF(R, D),
      eraseBatchNF(Rest, D).





get_a_path([Lit], Path, [Lit | Path]) :-
      !.
get_a_path([Lit | Form], Path, Res) :-
     get_a_disjunct(Form, [Lit | Path], Res).

 


get_a_disjunct([], Path, Path) :-
      !.
get_a_disjunct([First], Path, Res) :-
      !,
      get_a_path(First, Path, Res).
get_a_disjunct(Form, Path, Res) :-
      member(First, Form),
      get_a_path(First, Path, Res).

