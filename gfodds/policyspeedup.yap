

idmember(X,[Y|_Z]) :- X==Y,!.
idmember(X,[_Y|Z]) :- idmember(X,Z).

head(X,Y) :- X=[Y|_Z].

% ----------------------------------------------

post_process_policy_pathsRoni(ActionVar,Ps, Goal, Rs) :- 
       %nl, write('Goal: '),write(Goal),nl,
       %write('Action: '),write(ActionVar),nl,
       rem_path_eq(ActionVar,Goal,Ps,Ps2),!,
       %write('removed eq: '),nl,head(Ps2,H2), write(H2),nl,
       %write(Ps2),nl,
       rem_goal_satisfied(Ps2, Goal, Ps3),!,
       %write('removed goal sat: '),head(Ps3,H3), write(H3),nl,
       %write(Ps3),nl,
       (postprocesspolicy3579atoms,!,
       %write('--> trying atoms'),nl,
	  rem_policy_atoms(Ps3, Rs),!; Rs=Ps3, !),
       true.
       %write('Done: '),head(Rs,H4), write(H4),nl.
       %write(Rs),nl.

% ----------------------------------------------

% rem_policy_atoms([path(a, b, c, [a(X),a(Y),b(Y)], e),path(a2, b2, c2, [a(X),b(X,Y),b(Y,Z),b(U,W)], e2)],Result).

rem_policy_atoms([],[]).
% :- nl, write('all paths done'), nl.

rem_policy_atoms([CPath|Rest], [CNewPath|OP]) :- 
   %nl,write('entered '),write(CPath),nl,
   CPath=path(A, B, C, Path, E),!,
   %nl,write('handling one path '),write(Path),write(E),nl,
   rem_policy_atoms_path([E|Path],[E|NewPath]),!,
   %nl,write('handling one path done '),write(A),write(C),nl,write(E),nl,write(NewPath),nl,
   CNewPath=path(A, B, C, NewPath, E),
   rem_policy_atoms(Rest,OP),!.

rem_policy_atoms_path(Path,NewPath) :- 
     length(Path,Len),!,
     rem_policy_atoms_path(Path,Len,NewPath).

rem_policy_atoms_path(Path,0,Path) :- !.

rem_policy_atoms_path(Path,Index,NewPath) :-
    copy_term(Path,P),
    nth(Index,P,_A,R),
    copy_term(R,R2),
    groundpath(1,R2),
    nth(Index,Path,Atom,Rest),
    NewI is Index -1,!,
    % R vars not allowed to unify therefore first ground them
    (subsumes2(P,R2),!,
     %write('skip '),write(Index),nl,
     rem_policy_atoms_path(Rest,NewI,NewPath);
     rem_policy_atoms_path(Path,NewI,NewPath)),!.
     

groundpath(_,[]) :- !.

groundpath(N,[A|P]) :-
    ground(A),!,groundpath(N,P).

groundpath(N,[A|P]) :-
    groundatom(N,A,N2),!, groundpath(N2,P).

groundatom(N,A,N2) :-
   A =.. [F|Args],
   groundlist(N,Args,N2).


groundlist(N,[],N).

groundlist(N,[H|T],N2) :-
   ground(H),!,
   groundlist(N,T,N2).

groundlist(N,[H|T],N2) :-
   name(g,G),
   name(N,NN),
   conc(G,NN,GNN),
   name(GH,GNN),
   H=GH,
   N3 is N+1,
   groundlist(N3,T,N2).
   

% ----------------------------------------------

rem_goal_satisfied([], _, []) :- 
       !.

rem_goal_satisfied([path(_, _, _, Path,_) | Rest], Goal, Res) :- 
       Goal=[GoalAtom],
       %write('V1: '),write(GoalAtom),write(Path),nl,
       idmember(GoalAtom,Path),!,
       %write('V2: '),write(GoalAtom),write(Path),nl,
       %write('Removed - Goal: '), write(Goal), 
       %write('   Path = '), write(Path), nl, 
       rem_goal_satisfied(Rest, Goal, Res), !.

rem_goal_satisfied([P | Rest], Goal, [P | Res]) :- 
       %write('V3: '),write(Goal),write(P),nl,!
       rem_goal_satisfied(Rest, Goal, Res).


% for future ref
%       subsumes2(Goal1, Path),
%       write('subsumed'), nl, 


% ----------------------------------------------

% this removes equalities from paths by finding and applying them
% in the process we also glue the Action predicate into the path compound

rem_path_eq(_AV,_Goal,[],[]).
 
rem_path_eq(ActVar,Goal,[P|Rest],[RP|RRest]):- 
   P=path(X,Y,Z,PP),
   %write('Original Path: '),write(PP),nl,
   % this makes sure the Goal vars remain the same but other vars are new
   copy_term([ActVar|[Goal|PP]],[MyActVar|[MyGoal|MyPP]]),
   MyGoal=Goal,
   %write('Modified Path: '),write(MyPP),nl,
   %write('Modified Action: '),write(MyActVar),nl,
   filter_equalities(MyPP,[],PP2,[],L),!,
   %write('Eq List: '),write(L),nl,
   %write('Remaining: '),write(PP2),nl,
   Goal=[GoalAtom], GoalAtom =.. [_F | GoalVars],
   applyEq(GoalVars,L,PP2,PP3),!,
   %write('Result Path: '),write(PP3),nl,
   %write('Modified Action: '),write(MyActVar),nl,
   RP=path(X,Y,Z,PP3,MyActVar),!,
   rem_path_eq(ActVar,Goal,Rest,RRest),!.

% ----------------------------------------------

% splits the path into Eq and regular predicates
filter_equalities([],P,P,L,L).

filter_equalities([Pred|PList],A1,A2,L1,[Pred|L2]) :-
    Pred = eq(_A,_B),!,
    filter_equalities(PList,A1,A2,L1,L2).

filter_equalities([Pred|PList],A1,[Pred|A2],L1,L2) :-
%    Pred not = eq(_A,_B),!,
    filter_equalities(PList,A1,A2,L1,L2).


% ----------------------------------------------


applyEq(_GoalVars,[],P,P).

applyEq(GoalVars,[eq(A,B)|L],PP2,PP3) :-
   (idmember(A,GoalVars),B=A; A=B),  
   applyEq(GoalVars,L,PP2,PP3). 

