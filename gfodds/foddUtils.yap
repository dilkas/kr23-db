
:- dynamic
      modelchecking/0.

get_largest_var_num([], Largest, Largest) :- 
    !.   
get_largest_var_num([Var | Rest], LargestNum, Largest) :- 
    name(Var, [118 | NumName]),
    name(Num, NumName),		
    (Num > LargestNum -> (LargestNum1 is Num) ; (LargestNum1 is LargestNum)),     
    get_largest_var_num(Rest, LargestNum1, Largest).



get_largest_var_num(VarList, LargestVarNum) :- 
    get_largest_var_num(VarList, 1, LargestVarNum).
			 

get_largest_var(VarList, LargestVar) :- 
    get_largest_var_num(VarList, LargestVarNum),
    name(LargestVarNum, NumName),
    name(LargestVar, [118 | NumName]).


get_var_nums([], []) :- 
    !.
get_var_nums([Var | Rest], [[VarAggType,Num] | Other]) :- 
    name(Var, [VarAggType | VarNumName]),
    name(Num, VarNumName),
    get_var_nums(Rest, Other). 	      


get_num_vars([], []) :- 
    !.
get_num_vars([[VarAggType,Num] | Rest], [Var | Other]) :-
    name(Num, NumName),
    name(Var, [VarAggType | NumName]),
    get_num_vars(Rest, Other).


sortDVars(Vars, SortedVars) :- 
    get_var_nums(Vars, Nums),
    sort(Nums, SortedNums),
    get_num_vars(SortedNums, SortedVars).



sortDVarsRev(Vars, SortedVarsRev) :-
    findall(X, (member(X,Vars), once(avgVar(X))), AvgVars),
    findall(X, (member(X,Vars), once(maxVar(X))), MaxVars), 		   
    sortDVars(AvgVars, SortedAvgVars),
    reverse(SortedAvgVars, SortedAvgVarsRev),
    sortDVars(MaxVars, SortedMaxVars),
    reverse(SortedMaxVars, SortedMaxVarsRev),
    conc(SortedAvgVarsRev, SortedMaxVarsRev, SortedVarsRev).



stdApart(D, Dvars, R) :- 
    recorded(varCount,Start,_),
    eraseall(varCount),
    length(Dvars, Len),
    getNewVars(Start, Len, NewVars),
    Start1 is Start + Len,
    recorda(varCount,Start1,_),
    getGroundKey(Dvars, NewVars, Key),
    variablizeDD(D, Key, R).

stdApart(D, R) :- 
    getMaxDVars(D, DVars),	
    sortDVars(DVars,SortedDVars),
    stdApart(D, SortedDVars, R).    






getVarRewdLit(VarRewdLit, Key) :-
    recorded(rewdLit,RewdLit,_),
    RewdLit =.. [_ | RewdLitArgs],
    (all(RewdLitArg, member(RewdLitArg, RewdLitArgs), RewdLitArgs1) ; RewdLitArgs1 = []),
    getSubsKey(RewdLitArgs1, Key),
    variablizeTerm(RewdLit, Key, VarRewdLit).


%% variablizeDD(+, +, -)
%% variablizeDD(D, K, R)
%% True when R is the FODD D variablized according to key K.
%% For further info see variablizeTerm(+, +, -) in utils.pl


variablizeDD([], _, []) :- 
    !.
variablizeDD([node(I, Lit, L, R) | Rest], Key, [node(I, VarLit, L, R) | VarRest]) :- 
    variablizeTerm(Lit, Key, VarLit),
    variablizeDD(Rest, Key, VarRest).



getAvgDVars(D, AvgDVars) :- 
    getDVarsConsts(D, [], VC),
    findall(V, (member(V, VC), once(name(V,[98 | _]))), AvgDVars).
    
getMaxDVars(D, MaxDVars) :- 
    getDVarsConsts(D, [], VC),
    findall(V, (member(V, VC), once(name(V,[118 | _]))), MaxDVars).


separateAvgMaxVars(DVars, AvgDVars, MaxDVars) :-
    findall(X, (member(X,DVars),once(avgVar(X))), AvgDVars),
    findall(X, (member(X,DVars),once(maxVar(X))), MaxDVars).


%% getDVars(+, +, -)
%% getDVars(I, D, V)
%% True when V is a list of domain variables appearing in the subFODD of FODD D, 
%% rooted at node I

getDVars(I, D, Vars) :- 
    getSubDD(I, D, D1),
    getDVars(D1, Vars).


%% getDVars(+, -)
%% getDVars(D, V)
%% True when V is a list of all domain variables appearing in FODD D.

getDVars(D, Vars) :- 
    getDVarsConsts(D, [], VC),
    findall(V, (member(V, VC), once(domVar(V))), Vars).



%% extractVarsFromVarConsts(+, -)
%% extractVarsFromVarConsts(L, V)
%% True when V is a list containing only and all the domain variables in list L 

extractVarsFromVarConsts([], []) :- 
    !.
extractVarsFromVarConsts([V | Rest], [V | Rest1]) :- 
    domVar(V),
    extractVarsFromVarConsts(Rest, Rest1),
    !.
extractVarsFromVarConsts([_ | Rest], Rest1) :- 
    extractVarsFromVarConsts(Rest, Rest1).



%% getDVarsConsts(+, +, -)
%% getDVarsConsts(D, Y, V)
%% True when V is a list of all the domain variables and domain constants appearing 
%% in FODD D. The argument Y must be the empty list [].

getDVarsConsts([], VarsConsts, VarsConsts) :- 
    !.
getDVarsConsts([node(_, _, -1, -1) | Rest], SoFar, VarsConsts) :- 
    getDVarsConsts(Rest, SoFar, VarsConsts),
    !.
getDVarsConsts([node(_, Lit, _, _) | Rest], SoFar, VarsConsts) :- 
    Lit =.. [_ | V],
%    ord_union(SoFar, V, SoFar1),
    batchInsert(V, SoFar, SoFar1),
    getDVarsConsts(Rest, SoFar1, VarsConsts).







reorder_diagram(I, D, [node(I, V, -1, -1)]) :- 
    member(node(I, V, -1, -1), D),		   
    !.
reorder_diagram(I, D, Res) :-
    member(node(I, Lit, L, R), D),			    
    reorder_diagram(L, D, LRes),
    reorder_diagram(R, D, RRes),
    LRes = [LResRoot | _],
    RRes = [RResRoot | _],
    apply(node(2,Lit,1,0), [node(2,Lit,1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)], LResRoot, LRes, *, LLitRes), 
    apply(node(2,Lit,0,1), [node(2,Lit,0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)], RResRoot, RRes, *, RLitRes),
    LLitRes = [LLitResRoot | _],
    RLitRes = [RLitResRoot | _],
    apply(LLitResRoot, LLitRes, RLitResRoot, RLitRes, +, Res).





reorder_diagram(D, Res) :- 
   D = [node(Root,_,_,_) | _],	
   (modelchecking -> 
   		  (true)
		  ; 
		  (asserta(modelchecking),
		   recorda(modelchekingadded, modelcheckingadded, _))),
   reorder_diagram(Root, D, Res),
   (recorded(modelcheckingadded, modelcheckingadded, Ref) ->
   		  (retract(modelchecking),
		   erase(Ref))
		  ;
		  (true)).
   	 
   


    
unify_max_avg_variable_for_sp_predicate([], _, _, []) :-
    !.
unify_max_avg_variable_for_sp_predicate([node(I,Lit,L,R) | Rest], Pred, SpLit, [node(I,Lit1,L,R) | Other]) :-
    (functor(Lit,Pred,_) ->
     (Lit1 = SpLit)
     ;
     (Lit1 = Lit)),
    unify_max_avg_variable_for_sp_predicate(Rest, Pred, SpLit, Other).
    


unify_max_avg_variable_for_sp_predicate(D, [], D) :-
    !.
unify_max_avg_variable_for_sp_predicate(D, [Sp | SpLits], Res) :-
    Sp =.. [Pred | _],
    unify_max_avg_variable_for_sp_predicate(D, Pred, Sp, Res1),
    unify_max_avg_variable_for_sp_predicate(Res1, SpLits, Res).




unify_max_avg_variable_for_sp_predicate(D, Res) :-
    special_predicates(SpLits),
    unify_max_avg_variable_for_sp_predicate(D, SpLits, Res).	   


