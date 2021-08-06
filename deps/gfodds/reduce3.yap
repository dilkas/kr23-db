:- dynamic
    rFive3579/2,
    notrel3579/4,
    goal3579/1,
    nf3579/2,
    node3579/4,
    reach3579/6.

%% r1(+, +, -, -)
%% r1(H, D, R, Rem)
%% True when R is the FODD D after performing the r1 reduction on the nodes in list H, 
%% one by one. The list H is generated by the calling procedure such that it suspects 
%% that r1 could be applicable on the nodes mentioned in H. If r1 is applicable and 
%% applied (2nd clause fires), then some more candidates for r1 are generated, namely 
%% the parents of the node on which r1 was applicable. These are added to the list H 
%% and r1 is called recursively. Note that the parent nodes are added to Hintlist (i.e. H) 
%% to create NewHL and NewHL is always sorted in ascending order making sure that r1 is 
%% tested on lower nodes first. This is important because it makes sure that we cannot 
%% miss nodes on which r1 could have been applicable. And if r1 was not applicable on a 
%% node, it is stored in Rem. Rem, then becomes the fringe of nodes in the FODD that r1 was 
%% not applicable on and is used in r1r2Cycle(+, +, -). 


r1([], D, D, []) :-
    !.
r1([I | HintList], D, R, Rem) :- 
    del(node(I, _, C, C), D, D1),
    C \= -1,
    parentUpdate(D1, C, I, D2, PL),
    batchInsertRev(PL, HintList, NewHL),
%    ord_union(PL, HintList, NewHL),
    r1(NewHL, D2, R, Rem),
    !.
r1([I | HintList], D, R, [I | Rem]) :- 
    r1(HintList, D, R, Rem).



%% r2(+, +, -, -)
%% r2(H, D, R, Rem)
%% True when R is the FODD D after performing the r2 reduction on the nodes in list H, 
%% one by one. The list H is generated by the calling procedure such that it suspects 
%% that r2 could be applicable on the nodes mentioned in H. If r2 is applicable and 
%% applied (2nd clause fires), then some more candidates for r2 are generated, namely 
%% the parents of the node on which r2 was applicable. These are added to the list H 
%% and r2 is called recursively. Note that the parent nodes are added to Hintlist (i.e. H) 
%% to create NewHL and NewHL is always sorted in ascending order making sure that r2 is 
%% tested on lower nodes first. This is important because it makes sure that we cannot 
%% miss nodes on which r2 could have been applicable. And if r2 was not applicable on a 
%% node, it is stored in Rem. Rem, then becomes the fringe of nodes in the FODD that r2 was 
%% not applicable on and is used in r1r2Cycle(+, +, -). 

r2([], D, D, []) :- 
    !.   
r2([I1 | HintList], D, R, Rem) :- 
    once(member(node(I1, Lit, Lt, Rt), D)),
    member(node(I2, Lit, Lt, Rt), D),
    I2 \= I1,
    (I1 > I2 -> (del(node(I1, Lit, Lt, Rt), D, D1), parentUpdate(D1, I2, I1, D2, PL)) 
     ; 
     (del(node(I2, Lit, Lt, Rt), D, D1),
      parentUpdate(D1, I1, I2, D2, PL))),
    batchInsertRev(PL, HintList, NewHL),
%    ord_union(PL, HintList, NewHL),
    r2(NewHL, D2, R, Rem),
    !.
r2([I | HintList], D, R, [I | Rem]) :- 
    r2(HintList, D, R, Rem).



%% r1r2Cycle(+, +, -)
%% r1r2Cycle(H, D, R)
%% True when R is the result of running r1 and r2 on D in cycles such that each 
%% reductions works on the Rem list of the other. When this concludes, no r1 and 
%% r2 reduction are applicable on the FODD any longer. 

r1r2Cycle([], D, D) :- 
    !.
r1r2Cycle(HintList, D, Res) :- 
%    write('I entered this r1r2cycle with '), write(HintList), nl, 
    r1(HintList, D, D1, Rem),
    r2(Rem, D1, D2, Rem1),
%    ord_subtract(Rem1, Rem, Rem2),
    diff(Rem1, Rem, Rem2),
    r1r2Cycle(Rem2, D2, Res).


r1r2Cycle1(I, _, I) :-
    node3579(I, _, _, _),
    !.
r1r2Cycle1(I, D, Index) :- 
    member(node(I, Lit, -1, -1), D),
    (node3579(Index, Lit, -1, -1)
     ;
     Lit = 0.0,
     asserta(node3579(0, 0.0, -1, -1)),
     Index = 0
     ;
     Lit = 1.0,
     asserta(node3579(1, 1.0, -1, -1)),
     Index = 1
     ;
     Index = I,
     asserta(node3579(I, Lit, -1, -1))),
    !.
r1r2Cycle1(I, D, Index) :- 
%    (I = 197, trace ; true),
    member(node(I, Lit, L, R), D),    
    r1r2Cycle1(L, D, LIndex),
    r1r2Cycle1(R, D, RIndex),
    (LIndex = RIndex,
     Index = LIndex
     ;
     node3579(Index, Lit, LIndex, RIndex)
     ;
     Index = I,
     asserta(node3579(I, Lit, LIndex, RIndex))).
    


r1r2Cycle2(_, D, Res) :-
    (node3579(_, _, _, _), write('locha'), nl ; true),
    D = [node(Root, _, _, _) | _],

    r1r2Cycle1(Root, D, _),
    !,
    (all(node(I, Lit, L, R), retract(node3579(I, Lit, L, R)), D1) ; D1 = []),
    insertsort(D1, Res).

    






%% r3(+, -)
%% r3(D, R)
%% If the root node is an equality of the form eq(A, A), which is always true, this procedure removes 
%% the root and returns the left subFODD. Alternatively if the root is an equality of the form eq(A, B) 
%% where A and B are both domain constants (which can never be equal), this procedure removes the root 
%% and returns the right subFODD. 


r3(D, Res) :- 
    D = [node(I, eq(A, B), L, R) | Rest],
    (A == B -> removeUnreachable(L, [R], Rest, Res, _) 
     ;
     domConst(A),
     domConst(B),
     removeUnreachable(R, [L], Rest, Res, _)),
    !.
r3(D, D).

    


focusedR5(I, EF, C, D, HLsoFar, HL, Res) :- 
    once(not(rFive3579(I, C))),
    once(asserta(rFive3579(I, C))),
    once(del(node(I, Lit, L, R), D, D1)),
    once(member(node(C, Lit1, L1, R1), D1)),
    once(getNeg(Lit1, NLit1)),
    member([QLit, F], [[Lit1, L1], [NLit1, R1]]),
    once(proveFormula1(EF, [], QLit, T)),
    T = 1,
    (C = L, 
     !,
     insert(node(I, Lit, F, R), D1, D2)
     ;
     insert(node(I, Lit, L, F), D1, D2)),
    insert(C, HLsoFar, HLsoFar1),
    focusedR5(I, EF, F, D2, HLsoFar1, HL, Res),
    !.
focusedR5(_, _, _, D, HL, HL, D).
    




%% r5(+, +, -)
%% r5(H, D, R)
%% True when R is the result of applying the r5 reduction on the nodes of 
%% FODD D appearing in list H. The list H is generated by the calling procedure such that it suspects 
%% that r5 could be applicable on the nodes mentioned in H. This procedure works as follows. I is 
%% the node considered for r5 reduction. One of its children is chosen as the node C. Now the 
%% procedure checks to see if C or not C can be proved given the edge formula of the edge going 
%% from I to C. All this choosing of nodes and edges is done by backtracking as will be clear 
%% from the code. Clearly neither I nor C can be a leaf node. If, C or not C can be proved, the 
%% FODD is updated in the next step. the node I, now, instead of pointing to C points to the left 
%% child of C (if C was proved) or right child of C (if not C was proved). This This procedure 
%% continues for the entire subtree of I by adding in children of I to the HintList and calling r5 
%% recursively (3rd clause). The Hintlist is always sorted in descending order making sure that higher 
%% nodes are tried first. 

r5([], D, D) :-     
    eraseall(rFive3579),
%    nl, write('before r1r2Cycle: '), write(D), nl, 
%    r1r2Cycle(_, D, R),
    !.
r5([I | HintList], D, Res) :- 
    (debug(r5) -> (write('R5 entered'), nl, pp(D), nl, write('HintList = '), write([I | HintList]), nl) ; true),
       
    once(del(node(I, Lit, L, R), D, D1)),
    L \= -1,
    once(getNeg(Lit, NLit)),
    member([C, LitEF], [[L, Lit], [R, NLit]]),
    once(not(rFive3579(I, C))),
    once(asserta(rFive3579(I, C))),
    once(member(node(C, Lit1, L1, R1), D)),
    L1 \= -1,
    (debug(3) -> (write('I = '), write(I), write(' C = '), write(C), nl) ; true), 
    once(getNeg(Lit1, NLit1)),
    once(collectNodeFormula(I, D, NF)),
    (debug(3) -> (write('NF: '), write(NF), nl) ; true), 
    once(EF = [LitEF | NF]),    
    
    once(Lit1 =.. [_ | QVars]),
    once(all(ExPath, (get_a_path(EF, [], Path), once((getBasicBkgd(Path, QVars, Objs, ExPath1), floodState(ExPath1, Objs, ExPath)))), ExPaths)),
    
    (debug(3) -> (write('EF: '), write(EF), nl) ; true),
    (debug(3) -> (write('ExPaths: '), write(ExPaths), nl) ; true),
    member([QLit, F], [[Lit1, L1], [NLit1, R1]]),
    (debug(3) -> (write('QLit: '), write(QLit), nl) ; true),
    
%%%%%%%%%%%%%% Passing the formulas to a thm prover to see if the %%%%%%%%%%%%%%
%%%%%%%%%%%%%% r5 condition holds. It holds if T gets bound to 1.  %%%%%%%%%%%%%%
    
    once(prove_all(ExPaths, [QLit])),    
%    once(proveFormula1(EF, [], QLit, T)),
%    T = 1,
    
%%%%%%%%%%%%%% If proved modify the FODD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    (debug(3) -> (write('strong proved'), nl) ; true),
    eraseSubDDNF(C, D),
    (C = L, 
     !,
     insert(node(I, Lit, F, R), D1, D2)
     ;
     insert(node(I, Lit, L, F), D1, D2)),
    focusedR5(I, EF, F, D2, [C], HL, D2a),
    (debug(3) -> write('focused r5 performed'), nl ; true),
%    removeUnreachable(_, [C], D2, D3, Rem),
    removeUnreachable(_, HL, D2a, D3, Rem),
    (debug(3) -> (write('D3 post r1:'), nl, write(D3), nl) ; true),  
    (debug(3) -> (write('Unreachabe Removed after R5'), nl, write(' Rem = '), write(Rem), nl) ; true),  
    batchInsert(Rem, [I | HintList], NewHL),
    r1r2Cycle([I], D3, D4),
%    D4 = D3,
    (debug(3) -> (write('r5 performed on '), write(I), write(', '), write(C), nl, write(D4), nl) ; true), 
    r5(NewHL, D4, Res),
    !.
r5([I | HintList], D, Res) :-
    (debug(r5) -> (write('r5 part 3 entered with HL = '), write([I | HintList]),  nl) ; true),
    member(node(I, _, L, R), D),
    (L = -1,
     !,
     r5(HintList, D, Res)
     ;
     member(node(L, _, L1, R1), D),
     (L1 = -1,
      HL1 = HintList
      ;
      insert(L, HintList, HL1)),
     member(node(R, _, L2, R2), D),
     (L2 = -1,
      HL2 = HL1
      ;
      insert(R, HL1, HL2)),
     r5(HL2, D, Res)),
     !.
r5([_ | HintList], D, R) :- 
    r5(HintList, D, R).



%% removeUnreachable(+, +, +, -, -)
%% removeUnreachable(C1, H, D, U, Rem)
%% True when U is the FODD D after nodes in the subFODDs of nodes in list H have been removed. 
%% The list H is generated by the calling procedure such that it suspects that 
%% removeUnreachable(+, +, +, -, -) could be applicable on the nodes mentioned in H. The 
%% procedure is simple. It just checks for every node in HintList (or H), if it has a parent in 
%% the FODD. If it does not, it removes the node from the FODD, adds its children to the HintList 
%% and calls removeUnrechable(+, +, +, -, -) recursively. The first argument is interesting. It 
%% is a way to specify that some node should not be considered for removal. The necessity for 
%% this argument became clear because of an example in which removeUnreachable(+, +, +, -, -) 
%% removed the entire FODD. 

removeUnreachable(_, [], D, D, []) :- 
    !. 
removeUnreachable(C1, [X | HintList], D, U, Rem) :- 
    (var(C1) ; X \= C1),
    not(hasParent(X, D)),
    del(node(X, _, L, R), D, D1),
    (L = -1, 
     NewHL = HintList 
     ; 
     batchInsert([L, R], HintList, NewHL)), 
    removeUnreachable(C1, NewHL, D1, U, Rem),    
    !.
removeUnreachable(C1, [X | HintList], D, U, [X | Rem]) :- 
    removeUnreachable(C1, HintList, D, U, Rem).
    
    



%% pickEdgePair(+, -, -, -, -, -, -)
%% pickEdgePair(D, C1, C2, C1, Edge1, [I, Lit, L, R, N2, E2], D1)
%% True when a pair of edges e1 and e2. has been chosen from FODD D to perform r7 on. 
%% The edges have to be unrelated (neither is the ancestor of the other). The r7 routine 
%% will backtrack into this procudure to choose the next pair of edges if the r7 conditions 
%% are not met. This procedure has been separated from r7 so that different algorithms 
%% to choose edge pairs can be tried (the resulting FODD is dependent on the order in which 
%% reductions are applied). The 1st clause chooses edges according to r6 and the second 
%% according to r7 with e2 being chosen as high as possible in order to eliminate larger parts 
%% of the FODD first. This procedure returns many arguments. It returns nodes I1, I2, the 
%% literals associated with them, Lit1 and Lit2. The edges (t or f) E1 and E2, and the 
%% corresponding children C1 and C2. Also it returns the Sibling node of C2, C2sib. Finally 
%% it returns the FODD D1 resulting from the removal of node I2. 

pickEdgePair(D, C1, C2, C1, E1, Edge1, [I, Lit, L, R, N2, E2]) :- 
    member(node(I, Lit, L, R), D),
    once(L \= -1),    
%    write('r6 to be done: I = '), write(I), nl,
%    all(_, (nf3579(I, _), write(nf3579(I, x)), nl), _),
%    all(_, (ss3579(I1, I2, _, V, P), write(ss3579(I1, I2, x, V, P)), nl), _),
%    once(not((nf3579(I, _), ss3579(I, I, _, _, _)))),
    once(getNeg(Lit, NLit)),
    del([C1, _, E1], [[L, Lit, t], [R, NLit, f]], [[C2, N2, E2]]),
    once(Edge1 = edge(I, Lit, E1)),  
    (debug(1) -> write('I1 = '), write(I), write(' I2 = '), write(I), write(' E1 = '), write(E1), write(' E2 = '), write(E2), nl ; true),
    (debug(1) -> write('Not the same edge'), nl ; true).
pickEdgePair(D, C1, C2, C2sib, E2sib, Edge1, [I2, Lit2, L2, R2, N2, E2]) :- 
%    del(node(I2, Lit2, L2, R2), D, D1),
    member(node(I2, Lit2, L2, R2), D),
    once(L2 \= -1),
    member(node(I1, Lit1, L1, R1), D),
    once(L1 \= -1),
    once(I1 \= I2),
%    write('r7 to be done: I1 = '), write(I1), write(' I2 = '), write(I2), nl,
%    all(_, (nf3579(I, _), write(nf3579(I, x)), nl), _),
%    all(_, (ss3579(I1, I2, _, V, P), write(ss3579(I1, I2, x, V, P)), nl), _),
%    once(not((nf3579(I1, _), nf3579(I2, _), (ss3579(I1, I2, _, _, _) ; ss3579(I2, I1, _, _, _))))),
    once(getNeg(Lit2, NLit2)),
    del([C2, N2, E2], [[L2, Lit2, t], [R2, NLit2, f]], [[C2sib, _, E2sib]]),
    member([C1, E1], [[L1, t], [R1, f]]),
    once(C1 \= 0),
    once(Edge1 = edge(I1, Lit1, E1)),
    (debug(1) -> write('I1 = '), write(I1), write(' I2 = '), write(I2), write(' E1 = '), write(E1), write(' E2 = '), write(E2), nl ; true),
    (debug(1) -> write('Not the same edge'), nl ; true),
%    (notrel3579(I1, E1, I2, E2) 
%     ; 
%     notrel3579(I2, E2, I1, E1) 
%     ; 
     once(not(descendent(I2, C1, D))),
     once(not(descendent(I1, C2, D))).
%     asserta(notrel3579(I1, E1, I2, E2))).




%% r7(+, -)
%% r7(D, R)
%% True when R is the result of running r7 reduction on FODD D, checking every pair of edges. 
%% The order of edges is chosen according to pickEdgePair(+, -, -, -, -, -, -). One the pair 
%% of edges has been chosen, (the choosing is done by backtracking), the supersub is run on 
%% their subFODDs. If the superSub fails, we backtrack. Otherwise their respective edge 
%% formulas are produced and proveFormula(+, +, +, -) is called to prove the implication. 
%% Again if this fails, we backtrack in to the edge choosing procedure. Otherwise the node 
%% dropping condition is checked in a similar way. Depending on whether the node is to be dropped 
%% or not, the FODD is modified. Now r5 could be applicable on all nodes below C2 because the node 
%% fomulas have changes. Thus r5 is called with C2 in the Hintlist. Finally r7 is called again. 
%% This happens until r7 is no longer applicable and the diagram (modified) is returned. 


r7(D, Res) :- 
%    write('r7 commenced'), nl,    
    once(domainConsts(DomConsts)),
    once(getDVars(D, DVars)),
    once(conc(DomConsts, DVars, QVars)),

    once((debug(1) -> write('FODD: '), nl, write(D), nl ; true)), 
    once(reverse(D, Drev)),
    pickEdgePair(Drev, C1, C2, C2sib, E2sib, Edge1, [I2, Lit2, L2, R2, N2, E2]),
    del(node(I2, Lit2, L2, R2), D, D1),
    (debug(1) -> write('Unrelated'), nl ; true), 
    once(member(node(C1, CLit1, CL1, CR1), D)),
    once(member(node(C2, CLit2, CL2, CR2), D)),
    (debug(1) -> write('C1 = '), write(node(C1, CLit1, CL1, CR1)), write(' C2 = '), write(node(C2, CLit2, CL2, CR2)), nl ; true), 

%%%%%%%%%%%%%%% This means that if the target of e2 is 0, then the source of e2 must be dropped %%%%%%%%%%%%% 

    once((C2 \= 0
     ;
     Drop = 1)),

%%%%%%%%%%%%%%% Perform the supersub on the identified subFODDs %%%%%%%%%%%%%%%%%%%

    once(superSub(node(C1, CLit1, CL1, CR1), D, node(C2, CLit2, CL2, CR2), D, V1)),
    once(sort(V1, V)),
%    write(superSub(node(C1, CLit1, CL1, CR1), D, node(C2, CLit2, CL2, CR2), D, V)), nl,
%    once(apply(node(C1, CLit1, CL1, CR1), D, node(C2, CLit2, CL2, CR2), D, -, SubD)),
%    not((member(node(_, Num1, -1, -1), SubD), Num1 < 0.0)),
%    once(getDVars(C1, D, V1)),
%    once(getDVars(C2, D, V2)),
%    once(my_intersection(V1, V2, V)),

%%%%%%%%%%%%%%% If the supersub works, collect the relevant formulas %%%%%%%%%%%%%%%

    (debug(1) -> write('V = '), write(V), nl ; true),
    once(collectNodeFormula(I2, D, NF2)), 
    once(EF2 = [N2 | NF2]),
    once(all(ExPath, (get_a_path(EF2, [], Path), once((getBasicBkgd(Path, QVars, Objs, ExPath1), floodState(ExPath1, Objs, ExPath)))), ExPaths)),
    (debug(1) -> write('EF2 = '), write(EF2), nl ; true),   
    once(Edge1 = edge(I1, _, E1)),
    
	  
%%%%%%%%%%%%%% Now prove the r7 condition. Proved if T gets bound to 1 %%%%%%%%%%%%%

%    (once((C2 == 0))
%     ;
     once((reach3579(I1, E1, I2, E2, V, T)
	   ;	   
	   once(getEdgeFormula1(Edge1, D, V, EF1)),
	   (debug(1) -> write('EF1 = '), write(EF1), nl ; true),
	   once(asserta(goal3579(EF1))),
%	   once(proveFormula(EF2, [], EF1, V, T)),
	   once((prove_all(ExPaths, EF1), T = 1 ; T = 0)),
	   once(retract(goal3579(EF1))),
	   once(asserta(reach3579(I1, E1, I2, E2, V, T))))),

     (debug(1) -> write('T = '), write(T), nl ; true), 
     T = 1, 
    (debug(1) -> write('proved'), nl ; true),

%%%%%%%%%%%%%% If proved, check if the node can be dropped %%%%%%%%%%%%%%%%%%%%%%%%

    once(member(node(C2sib, CLit2sib, CL2sib, CR2sib), D)),
    (debug(1) -> write('C2sib identified as '), write(node(C2sib, CLit2sib, CL2sib, CR2sib)), nl ; true),
    (once(superSub(node(C1, CLit1, CL1, CR1), D, node(C2sib, CLit2sib, CL2sib, CR2sib), D, Vsib1)),    
     once(sort(Vsib1, Vsib)),
%      (once(apply(node(C1, CLit1, CL1, CR1), D, node(C2sib, CLit2sib, CL2sib, CR2sib), D, -, SubDsib)),
%       not((member(node(_, Num2, -1, -1), SubDsib), Num2 < 0.0)),
%       once(getDVars(C1, D, V1sib)),
%       once(getDVars(C2sib, D, V2sib)),
%       once(my_intersection(V1sib, V2sib, Vsib)),
              
       (debug(1) -> write('Vsib = '), write(Vsib), nl ; true),
       
	once((reach3579(I1, E1, I2, E2, Vsib, Tsib)
	     ;
	     once(getEdgeFormula1(Edge1, D, Vsib, EF1sib)),
	     (debug(1) -> write('EF1sib = '), write(EF1sib), nl ; true), 
	      once(asserta(goal3579(EF1sib))),
%	      once(proveFormula(EF2, [], EF1sib, Vsib, Tsib)),
	      once((prove_all(ExPaths, EF1sib), Tsib = 1 ; Tsib = 0)),
	      once(retract(goal3579(EF1sib))), 
	      once(asserta(reach3579(I1, E1, I2, E2, Vsib, Tsib))))),
	
	Tsib = 1,
	(debug(1) -> write('Ready to drop node'), nl ; true), 
	Drop = 1
	;
	Drop = 0),
     (debug(1) -> write('Drop = '), write(Drop), nl ; true), 

%%%%%%%%%%%%%%%%%%%%% In either case, modify the FODD %%%%%%%%%%%%%%%%%%%%%%%%%%%
    !,
      (E2 = t,
       !,
       (Drop = 1,
%        write('r7 case 1'), nl, 
	!,
	parentUpdate(D1, C2sib, I2, D2, _),
%	batchInsertRev(PL1, [], PL),
%	r1r2Cycle(PL, D2, D3)
	D3 = D2
	;
%       write('r7 case 2'), nl, 
	insert(node(0, 0.0, -1, -1), D1, D0),
	insert(node(I2, Lit2, 0, R2), D0, D2),
%      write('D2 = '), write(D2), nl,
	D3 = D2)
%	r1r2Cycle([I2], D2, D3))
       ;
       (Drop = 1,
	!,
%        write('r7 case 3'), nl, 
	parentUpdate(D1, C2sib, I2, D2, _),
%	batchInsertRev(PL1, [], PL),
%	r1r2Cycle(PL, D2, D3)
	D3 = D2
	;
%        write('r7 case 4'), nl, 
	insert(node(0, 0.0, -1, -1), D1, D0),
	insert(node(I2, Lit2, L2, 0), D0, D2),
	D3 = D2)),
%	r1r2Cycle([I2], D2, D3))),
%    write('r7 update done'), nl, 
%    all(_, (reach3579(Q1, Q2, Q3, Q4, Q5, Q6), write(reach3579(Q1, Q2, Q3, Q4, Q5, Q6)), nl), _),
    (Drop = 1 -> eraseSubDDNF(I2, D) ; eraseSubDDNF(C2, D)),
    eraseSuperss(I2, D),
    (debug(1) -> write('D3 = '), write(D3), nl ; true),   
    removeUnreachable(C2sib, [C2], D3, D4, Rem),
    !,
%    write('removed'), nl, 
    (debug(1) -> write('Unreachable removed'), write('Rem = '), write(Rem), nl ; true), 
    r3(D4, D5),
    !,
    r5(Rem, D5, D6),
%    write('r5 ed'), nl, 
    (debug(1) -> write('Strong Reduced'), nl ; true), 
    (debug(1) -> write('Modified FODD: '), nl, write(D6), nl ; true),
    !,
    r7(D6, Res),
    !.
r7(D, D).


   
    
%% r8(+, -)
%% r8(D, R)
%% True when R is the result of applying the equality reduction to the root node of FODD D. Only the root node is chosen to avoid cases where nodes/subFODDs have to be split etc. The procedure works as follows. The first check is to see that the root node is an equality node eq(X, Y), otherwise this reduction is not applicable. Then we check to see that atleast one of the X and Y is a domain variable, otherwise r8 is not applicable. Then come the r8 conditions, namely that the right subFODD of the root does not contain both X and Y. If, this is satisfied then we proceed. Now variable replacements have to be made depending on which variable is present on the right subFODD of the root. The variable to replace X and/or Y with is generated by the getReplVar(+, +, -) procedure found in the file reductionUtils.pl. Finally the root is chopped off and the left and right subFODDs are combined with the max procedure. 

r8(D, Res) :- 
    D = [node(_, eq(X, Y), L, R) | _],

%%%%%%%%%%% check for r8 condition that X and Y cannot both appear on the right subFODD %%%%%%%%%%%%
%%%%%%%%%%% of the root and atleast one of them has to be adomain variable. %%%%%%%%%%%%%%%%%%%%%%%

    (domVar(X) ; domVar(Y)),
    getSubDD(R, D, RD),
    getDVarsConsts(RD, [], Vars), 
    not((member(X, Vars), member(Y, Vars))),
    (debug(6) -> write('r8 applicable'), nl ; true),

%%%%%%%%%% Finding the replacement variables for X and/or Y %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    (not(member(Y, Vars)),
     domVar(Y),
%     write('r8 case 1'), nl, 
     (debug(6) -> write(Y), write(' to be replaced by '), write(X), nl ; true),
     getReplVar(D, [Y, X], ReplVar),
     (debug(6) -> write('Repl Var: '), write(ReplVar), nl ; true),
     (ReplVar = X -> Key = [Y:ReplVar] ; Key = [X:ReplVar, Y:ReplVar]),
     replaceDVars(D, Key, D1)
     ;
     not(member(X, Vars)),
     domVar(X),
%     write('r8 case 2'), nl, 
     (debug(6) -> write(X), write(' to be replaced by '), write(Y), nl ; true),
     getReplVar(D, [X, Y], ReplVar),
     (debug(6) -> write('Repl Var: '), write(ReplVar), nl ; true),
     (ReplVar = X -> Key = [X:ReplVar] ; Key = [X:ReplVar, Y:ReplVar]),
     replaceDVars(D, Key, D1)),

%%%%%%%%%% Perform the max %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    member(node(L, LLit, LL, LR), D1),
    member(node(R, RLit, RL, RR), D1),    
    apply(node(L, LLit, LL, LR), D1, node(R, RLit, RL, RR), D1, max, D2),
    r8(D2, Res),
    !.
r8(D, D).





%% r81(+, -)
%% r81(D, R)
%% True when R is the result of applying the equality reduction to the root node of FODD D. Only the root node is chosen to avoid cases where nodes/subFODDs have to be split etc. The procedure works as follows. The first check is to see that the root node is an equality node eq(X, Y), otherwise this reduction is not applicable. Then we check to see that atleast one of the X and Y is a domain variable, otherwise r8 is not applicable. Then come the r8 conditions, namely that the right subFODD of the root does not contain both X and Y. If, this is satisfied then we proceed. Now variable replacements have to be made depending on which variable is present on the right subFODD of the root. The variable to replace X and/or Y with is generated by the getReplVar(+, +, -) procedure found in the file reductionUtils.pl. Finally the root is chopped off and the left and right subFODDs are combined with the max procedure. 

r81(D, Res) :- 
    r9on,
%    once(length(D, DLen)),
    once(D = [node(Root, _, _, _) | _]),
    member(node(I, eq(X, Y), L, R), D),

%%%%%%%%%%% check for r8 condition that X and Y cannot both appear on the right subFODD %%%%%%%%%%%%
%%%%%%%%%%% of the root and atleast one of them has to be adomain variable. %%%%%%%%%%%%%%%%%%%%%%%

    once((domVar(X) ; domVar(Y))),
    once(getSubDD(R, D, BnR)),
    (debug(g) -> write('BnR obtained'), nl ; true),
    once(getDVarsConsts(BnR, [], Vars)),
    once(not((member(X, Vars), member(Y, Vars)))),
    (debug(6) -> write('r81 applicable on '), write(I), nl ; true),
    
%%%%%%%%%% Finding the replacement variables for X and/or Y %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    (once(not(member(Y, Vars))),
    once(domVar(Y)),
    once(collectNodeFormula(I, D, NF)),
    once(getNFVars(NF, [], NFVars)),
    once(not(member(Y, NFVars))),
    (debug(6) -> write('r81 case 1'), nl ; true), 
    (debug(6) -> write(Y), write(' to be replaced by '), write(X), nl ; true),
    once(getSubDD(L, D, BnL)),
%    write(getReplVar(D, BnL, [Y, X], ReplVar)),
    (debug(6) -> write('entering getReplVar'), nl ; true),
    getReplVar(D, BnL, [Y, X], ReplVar),
    (debug(6) -> write('Repl Var: '), write(ReplVar), nl ; true),
    once((ReplVar == X -> (getGroundKey([Y], [X], Key), replaceDVars(BnL, Key, BnLp), BnRp = BnR, D1 = D) 
	  ; 
	  getGroundKey([Y, X], [ReplVar, ReplVar], Key1),
	  getGroundKey([X], [ReplVar], Key2),
	  replaceDVars(BnL, Key1, BnLp), replaceDVars(BnR, Key2, BnRp), replaceDVars(D, Key2, D1)))
    ;
    once(not(member(X, Vars))),
    (debug(6) -> write('not member X Vars'), nl ; true),
    once(domVar(X)),
    once(collectNodeFormula(I, D, NF)),
    once(getNFVars(NF, [], NFVars)),
    once(not(member(X, NFVars))),
    (debug(6) -> write('r81 case 2'), nl ; true),
    (debug(6) -> write(X), write(' to be replaced by '), write(Y), nl ; true),
    once(getSubDD(L, D, BnL)),
    (debug(6) -> write('BnL obtained'), nl ; true),
    once(getReplVar(D, BnL, [X, Y], ReplVar)),
    (debug(6) -> write('Repl Var: '), write(ReplVar), nl ; true),
    once((ReplVar == Y -> getGroundKey([X], [Y], Key), replaceDVars(BnL, Key, BnLp), BnRp = BnR, D1 = D 
	  ; 
	  getGroundKey([Y, X], [ReplVar, ReplVar], Key1),
	  getGroundKey([Y], [ReplVar], Key2),
	  replaceDVars(BnL, [Y:ReplVar, X:ReplVar], BnLp), replaceDVars(BnR, [Y:ReplVar], BnRp), replaceDVars(D, [Y:ReplVar], D1)))),
    
%%%%%%%%%% Perform the max %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    retractall(nf3579(_, _)),
	  BnLp = [BnLpRoot | _],
	  BnRp = [BnRpRoot | _],
	  apply(BnLpRoot, BnLp, BnRpRoot, BnRp, max, Bnp),
	  (debug(6) -> write('Bnp: '), write(Bnp), nl ; true),
	  (I == Root -> D3 = Bnp
	   ;
	   insert(node(0, 0.0, -1, -1), D1, D0),
	   parentUpdate(D0, 0, I, D2a, _),
	   removeUnreachable(Root, [I], D2a, Ba, _),
	   (debug(6) -> write('Ba: '), write(Ba), nl ; true),
	   getLeafIndices(D1, LeafIndices),
	   batchParentUpdate(D0, 0, LeafIndices, D3b, PL),
	   insert(node(1, 1.0, -1, -1), D3b, D4b),
	   parentUpdate(D4b, 1, I, D5b, _),     
	   r1r2Cycle(PL, D5b, Bb),
	   (debug(6) -> write('Bb: '), write(Bb), nl ; true),
	   Ba = [BaRoot | _],
	   Bb = [BbRoot | _],
	   Bnp = [BnpRoot | _],
%          write(apply(BnpRoot, Bnp, BbRoot, Bb, *, Bnbp)), nl, 
	   apply(BnpRoot, Bnp, BbRoot, Bb, *, Bnbp), 
	   (debug(6) -> write('Bnbp: '), write(Bnbp), nl ; true),
	   Bnbp = [BnbpRoot | _],
	   apply(BnbpRoot, Bnbp, BaRoot, Ba, +, D3)),
%	  length(D3, D3Len))),
%    D3Len < DLen,
    (debug(6) -> write('r81 gives: '), write(D3), nl ; true),
    r81(D3, Res),
    !.
r81(D, D).
    








%replace_edge_target([], _, [], Rem, Rem) :- 
%    !.
%replace_edge_target([node(I, Lit, L, R) | Rest], Edges, [node(I, Lit, NewL, NewR) | Rest1], RemSoFar, Rem) :-
%    (member(I-t, Edges),
%     NewL = 0,
%    insert(L, RemSoFar, RemSoFar1)
%    ;
%     NewL = L,
%    RemSoFar1 = RemSoFar),
%    (member(I-f, Edges),
%    NewR = 0,
%    insert(R, RemSoFar1, RemSoFar2)
%    ;
%    NewR = R,
%    RemSoFar2 = RemSoFar1),
%    replace_edge_target(Rest, Edges, Rest1, RemSoFar2, Rem).



%replace_edge_target([node(I, Lit, L, R) | Rest], Edges, [node(I, Lit, 0, R) | Rest1], [L | Rem]) :-
%    member(I-t, Edges),
%    replace_edge_target(Rest, Edges, Rest1, Rem),
%    !.
%replace_edge_target([node(I, Lit, L, R) | Rest], Edges, [node(I, Lit, L, 0) | Rest1], [R | Rem]) :-
%    member(I-f, Edges),
%    replace_edge_target(Rest, Edges, Rest1, Rem),
%    !.
%replace_edge_target([N | Rest], Edges, [N | Rest1], Rem) :-
%    replace_edge_target(Rest, Edges, Rest1, Rem).







perform_r9([], [], _, Edges, _, Edges) :-
    !.
perform_r9([path(N, _, GPEdges, GroundPath) | GroundRestPaths], [_ | RestPaths], HigherPaths, EdgesSoFar, Dir, Edges) :-
    once(my_intersection(GPEdges, EdgesSoFar, CommEdges)),
    CommEdges \== [],
    once(getBasicBkgd(GroundPath, [], Objs, ExPath1)),
    (debug(1) -> nl, write('Bkgd Collected '), nl ; true),
    once(floodState(ExPath1, Objs, ExPath)),
%    once(GroundPath = [FirstLit | AfterPath]),
    (debug(1) -> write('State Flooded '), nl ; true),
    (not(pathSatisfiable(ExPath, ExPath))
     ;
     member(path(HN, _, _, Path), HigherPaths),    
%    once(interpretPath(Path, Path1)),
    once(HNDir is HN * Dir),
    once(NDir is N * Dir),
    NDir = HNDir,
    (debug(1) -> nl, write('GroundPath = '), write(GroundPath), nl ; true),    
    (debug(1) -> write('Path = '), write(Path), nl ; true),
    not(not(subsumes1(Path, ExPath)))),
    (debug(1) -> write('proved'), nl ; true),
    perform_r9(GroundRestPaths, RestPaths, HigherPaths, EdgesSoFar, Dir, Edges),
    !.
perform_r9([path(_, _, EdgeList, _) | GroundRestPaths], [HP | RestPaths], HigherPaths, EdgesSoFar, Dir, Edges) :-
    (debug(1) -> write('EdgeList: '), write(EdgeList), nl ; true),
    (debug(1) -> write('EdgesSoFar: '), write(EdgesSoFar), nl ; true),
    diff(EdgesSoFar, EdgeList, EdgesSoFar1),
    (debug(1) -> write('EdgesSoFar1: '), write(EdgesSoFar1), nl ; true),
    conc(HigherPaths, [HP], HigherPaths1),
%    HigherPaths1 = [HP | HigherPaths],
    perform_r9(GroundRestPaths, RestPaths, HigherPaths1, EdgesSoFar1, Dir, Edges).

    





r9(D, R) :-
    D = [node(Root, _, _, _) | _],
    get_all_edges(D, AllEdges),
    (debug(1) -> write('All edges'), write(AllEdges), nl ; true),
    get_paths(Root, D, [], [], GroundPaths1),
    length(GroundPaths1, LenGP1),
    (debug(1) -> write('Collected Ground Paths of length '), write(LenGP1), nl ; true),
    insertsort(GroundPaths1, GroundPaths),
    (debug(1) -> write('Sorted Ground Paths: '), nl, write(GroundPaths), nl ; true),
    getDVars(D, Vars),
    getSubsKey(Vars, Key),
    variablizeDD(D, Key, DVar),
    (debug(1) -> write('Variablized D'), nl ; true),
    get_paths(Root, DVar, [], [], Paths1),
    length(Paths1, LenP1),
    (debug(1) -> write('Collected Variablized Paths of length '), write(LenP1), nl ; true),
    insertsort(Paths1, Paths),
    (debug(1) -> write('Sorted Variablized Paths: '), nl, write(Paths), nl ; true),
    perform_r9(GroundPaths, Paths, [], AllEdges, 0, Edges),
    (debug(1) -> write('r9 performed: '), write(Edges), nl ; true),
    ((Edges = [] ; member(node(0, 0.0, -1, -1), D)), D0 = D ;  insert(node(0, 0.0, -1, -1), D, D0)),
    replace_edge_target(D0, Edges, D1, [], HL1),
    (debug(1) -> write('Target Edge Replaced, HL1 = '), write(HL1), nl ; true),
    insertsort(HL1, HL2),
    (debug(1) -> write('Sorted HL2 = '), write(HL2), nl ; true),
    removeUnreachable(_, HL2, D1, D2, HL3), 
    (debug(1) -> write('Unreachable Removed'), nl ; true),
%    r5(HL3, D2, R).
    (Edges = [],
     R = D2
     ;
     once(eraseBatchNF(Edges, D)),
    once(eraseBatchSuperss(Edges, D)),
    (debug(1) -> write('calling drop cycle'), nl ; true),
%    write('calling drop cycle'), nl,
     once(r9_drop_cycle(Edges, D2, HL3, D3)),
%    write('returning from drop cycle'), nl,
    (debug(1) -> write('returning from drop cycle'), nl ; true),
     (D3 = D2,
      R = D3
      ;
      r9(D3, R))).
    
    




r92(D, R) :-
    D = [node(Root, _, _, _) | _],
    get_all_edges(D, AllEdges),
    (debug(1) -> write('2 All edges '), write(AllEdges), nl ; true),
    get_paths_rev(Root, D, [], [], GroundPaths1),
    length(GroundPaths1, LenGP1),
    (debug(1) -> write('2 Collected Ground Paths of length '), write(LenGP1), nl ; true),
    sort(GroundPaths1, GroundPaths),
    (debug(1) -> write('2 Sorted Ground Paths: '), nl, write(GroundPaths), nl ; true),
    getDVars(D, Vars),
    getSubsKey(Vars, Key),
    variablizeDD(D, Key, DVar),
    (debug(1) -> write('2 Variablized D'), nl ; true),
    get_paths_rev(Root, DVar, [], [], Paths1),
    length(Paths1, LenP1),
    (debug(1) -> write('2 Collected Variablized Paths of length '), write(LenP1), nl ; true),
    sort(Paths1, Paths),
    (debug(1) -> write('2 Sorted Variablized Paths: '), nl, write(Paths), nl ; true),
    perform_r9(GroundPaths, Paths, [], AllEdges, 1, Edges),
    (debug(1) -> write('2 r9 performed: '), write(Edges), nl ; true),
    ((Edges = [] ; member(node(0, 0.0, -1, -1), D)), D0 = D ;  insert(node(0, 0.0, -1, -1), D, D0)),
    replace_edge_target(D0, Edges, D1, [], HL1),
    (debug(1) -> write('2 Target Edge Replaced, HL1 = '), write(HL1), nl ; true),
    insertsort(HL1, HL2),
    removeUnreachable(_, HL2, D1, D2, HL3), 
    (debug(1) -> write('2 Unreachable Removed HL3 = '), write(HL3), nl ; true),
%    r5(HL3, D2, R).
    (Edges = [],
     R = D2
     ;
     once(eraseBatchNF(Edges, D)),
     once(eraseBatchSuperss(Edges, D)),
    (debug(1) -> write('2 calling drop cycle'), nl ; true),
     once(r9_drop_cycle(Edges, D2, HL3, D3)),
    (debug(1) -> write('2 returning from drop cycle'), nl ; true),
     (D3 = D2,
      R = D3
      ;
      r92(D3, R))).



    

r9_drop([], D, _, [], [], D) :-
%    r5(ReplRem, D, Res),
    !.
r9_drop([I2-E2 | Rest], D, DVars, ReplRem, Rem, Res) :-
    once(domainConsts(DomConsts)),
    once(conc(DomConsts, DVars, QVars)),
    once(member(node(I2, Lit2temp, L2, R2), D)),
    once((E2 = t,
     Lit2 = Lit2temp,
     C2sib = R2
     ;
     getNeg(Lit2temp, Lit2),
     C2sib = L2)),
    once(collectNodeFormula(I2, D, NF2)),
    once(EF2 = [Lit2 | NF2]),
    once(all(ExPath, (get_a_path(EF2, [], Path), once((getBasicBkgd(Path, QVars, Objs, ExPath1), floodState(ExPath1, Objs, ExPath)))), ExPaths)),
    member(node(I1, Lit1, L1, R1), D),
    once(L1 \= -1),
%    once(getNeg(Lit1temp, NLit1temp)),
    member([Lit1, C1, E1], [[Lit1temp, L1, t], [NLit1temp, R1, f]]),
    once(member(node(C1, CLit1, CL1, CR1), D)),
    once(member(node(C2sib, CLit2sib, CL2sib, CR2sib), D)),

    (debug(1) -> write('I1 = '), write(I1), write(' I2 = '), write(I2), write(' E1 = '), write(E1), write(' E2 = '), write(E2), nl ; true),
    (debug(1) -> write('C1 = '), write(C1), write(' C2sib = '), write(C2sib), nl ; true),  
    once(superSub(node(C1, CLit1, CL1, CR1), D, node(C2sib, CLit2sib, CL2sib, CR2sib), D, Vsib)),     
%    write('atleast'), nl, 
   

    once((reach3579(I1, E1, I2, E2, Vsib, Tsib)
          ;
          once(getEdgeFormula1(edge(I1, Lit1, E1), D, Vsib, EF1sib)),           
	  (debug(1) -> write('ExPaths: '), write(ExPaths), nl ; true),
	  (debug(1) -> write('EF1sib = '), write(EF1sib), nl ; true),
    
	  (prove_all(ExPaths, EF1sib), Tsib = 1 ; Tsib = 0),
          once(asserta(reach3579(I1, E1, I2, E2, Vsib, Tsib))))),    
%    write('Tsib = '), write(Tsib), nl,
    Tsib = 1,
    (debug(1) -> write('drop proved'), nl ; true),
    parentUpdate(D, C2sib, I2, D1, Par),
    del(node(I2, _, _, _), D1, D2),
    eraseSubDDNF(I2, D),
%    retractall(nf3579(_, _)), 
    eraseSuperss(I2, D),
    
    r9_drop(Rest, D2, DVars, ReplRem1, Rem, Res),
    insert(C2sib, ReplRem1, ReplRem),
    !.
r9_drop([X | Rest], D, DVars, ReplRem, Rem, Res) :-    
    r9_drop(Rest, D, DVars, ReplRem, Rem1, Res),
    insert(X, Rem1, Rem).

    
    
    
r9_drop_cycle(EdgeList, D, ReplRem, Res) :-
    (debug(1) -> nl, write('drop cycle entered'), nl ; true), 
    getDVars(D, DVars),
%    write('drop cycle entered'), nl, write(D), nl, write(r9_drop(EdgeList, _, DVars, HL, Rem, R)), nl, 
    r9_drop(EdgeList, D, DVars, HL, Rem, R),
%    write('Rem = '), write(Rem), nl, 
    batchInsert(HL, ReplRem, HL1),
    (debug(1) -> write('Rem = '), write(Rem), write(' HL = '), write(HL), nl ; true),
    (D = R,
     !,
%     write('doing r5'), nl,  
     r5(HL1, R, Res),
%     write('back from r5 after r9-drop: '), write(Res), nl,
     (debug(1) -> write('back from r5 after r9-drop: '), write(Res), nl, pp(Res), nl ; true)
     ;
     r9_drop_cycle(Rem, R, HL1, Res)).





    
r10([node(_, eq(_, _), _, 0) | Rest], Res) :-
    r10(Rest, Res),
    !.
r10([node(_, eq(_, _), 0, _) | Rest], Res) :-
    r10(Rest, Res), 
    !.
r10(D, Res) :-
    del(node(I, eq(_, _), 0, R), D, D1),
    parentUpdate(D1, R, I, D2),
    r10(D2, Res),
    !.
r10(D, Res) :-
    del(node(I, eq(_, _), L, 0), D, D1),
    parentUpdate(D1, L, I, D2),
    r10(D2, Res),
    !.
r10(D, D).    






r11([], D, _, D) :-
    retractall(nf3579(_, _)),
    !.
r11([node(I, Lit, 0, R) | Candidates], D, Key, Res) :-
    once(getNeg(Lit, NLit)),
    once(collectNodeFormula(I, D, NF)),    
    once(get_paths(R, D, [], [], Paths)),
    once(all(Path, (get_a_disjunct(NF, [], SuperPath), member(path(_, _, _, SubPath), Paths), conc(SuperPath, SubPath, Path)), AllPaths)),
    once(length(AllPaths, Len)),
%    write('AllPaths: '), write(AllPaths), nl, 
    once(findall(1, (member(P, AllPaths), once((variablizeFormula([NLit | P], Key, VarP), getBasicBkgd([Lit | P], [], Objs, ExPath1), floodState(ExPath1, Objs, ExPath))), once(subsumes2(VarP, ExPath))), Z)),
%    write('Z: '), write(Z), nl, 
    length(Z, Len),
%    write(node(I, Lit, 0, R)), write('removed'), nl,
    del(node(I, Lit, 0, R), D, D1),
    parentUpdate(D1, R, I, D2, _),
%    write('Parent Updated'), nl, 
    retractall(nf3579(_, _)),
    r11(Candidates, D2, Key, Res),
    !.
r11([node(I, Lit, L, 0) | Candidates], D, Key, Res) :-
    once(getNeg(Lit, NLit)),
    once(collectNodeFormula(I, D, NF)),    
%    write(NF), nl, 
    once(get_paths(L, D, [], [], Paths)),
    once(all(Path, (get_a_disjunct(NF, [], SuperPath), member(path(_, _, _, SubPath), Paths), conc(SuperPath, SubPath, Path)), AllPaths)),
    once(length(AllPaths, Len)),
%    write('AllPaths: '), write(AllPaths), nl, 
    once(findall(1, (member(P, AllPaths), once((variablizeFormula([Lit | P], Key, VarP), getBasicBkgd([NLit | P], [], Objs, ExPath1), floodState(ExPath1, Objs, ExPath))), once(subsumes2(VarP, ExPath))), Z)),
%    write('Z: '), write(Z), nl, 
    length(Z, Len),
%    write(node(I, Lit, L, 0)), write('removed'), nl, 
    del(node(I, Lit, L, 0), D, D1),    
    parentUpdate(D1, L, I, D2, _), 
%    write('parent Updated'), nl, 
    retractall(nf3579(_, _)),
    r11(Candidates, D2, Key, Res),    
    !.    
r11([_ | Candidates], D, Key, Res) :-
    r11(Candidates, D, Key, Res).
    

r11(D, Res) :-
    r11on,
    !,
    getDVars(D, Vars),
    getSubsKey(Vars, Key),
    (all(node(I, eq(X, Y), L, 0), member(node(I, eq(X, Y), L, 0), D), CandEqL) ; CandEqL = []),
    (all(node(I, eq(X, Y), 0, R), member(node(I, eq(X, Y), 0, R), D), CandEqR) ; CandEqR = []),
    (all(node(I, Lit, 0, R), (member(node(I, Lit, 0, R), D), once((Lit =.. [P | _], not(P = eq), member(node(R, LitR, _, _), D))), LitR =.. [P | _]), CandR) ; CandR = []), 
    (all(node(I, Lit, L, 0), (member(node(I, Lit, L, 0), D), once((Lit =.. [P | _], not(P = eq), member(node(L, LitL, _, _), D))), LitL =.. [P | _]), CandL) ; CandL = []), 
    flatten([CandL, CandR, CandEqL, CandEqR], Candidates),
%    write('Candidates: '), write(Candidates), nl,
    r11(Candidates, D, Key, Res),
    !.
r11(D, D).
    


r12(D, R) :-
    r9(D, D1),
    r7(D1, R).