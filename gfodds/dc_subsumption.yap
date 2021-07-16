


split_example(_, [], []) :- 
    !.
split_example(Ex, [Lit | Rest], [[P | LitList] | Other]) :- 
    Lit =.. [P | _],
    (all(Lit, member(Lit, Ex), LitList) ; LitList = []),
    split_example(Ex, Rest, Other).




convert_Ex(Ex, Conv_Ex) :- 
    templates(T), 
    split_example(Ex, T, Conv_Ex).






del_Lit_from_conv_State(Lit, State, NewState) :- 	     
    functor(Lit, P, _),
    select([P | Lits], State, State1),
    (select(Lit, Lits, NewLits) ->
     (NewState = [[P | NewLits] | State1])
     ;
     (NewState = State)).


del_Lits_from_conv_State([], State, State) :- 
    !.
del_Lits_from_conv_State([Lit | Rest], State, NewState) :- 
    del_Lit_from_conv_State(Lit, State, State1),
    del_Lits_from_conv_State(Rest, State1, NewState).



add_Lit_to_conv_State(Lit, State, NewState) :- 
    functor(Lit, P, _),
    select([P | Lits], State, State1),
    (memberchk(Lit, Lits) ->
     (NewState = State)
     ;
     (NewState = [[P | [Lit | Lits]] | State1])).


add_Lits_to_conv_State([], State, State) :- 
    !.
add_Lits_to_conv_State([Lit | Rest], State, NewState) :- 
    add_Lit_to_conv_State(Lit, State, State1),
    add_Lits_to_conv_State(Rest, State1, NewState).









dc_subsumes(Lit, P, [[P | LitList] | Rest]) :- 
    member(Lit, LitList).
dc_subsumes(Lit, P, [_ | Rest]) :- 
    dc_subsumes(Lit, P, Rest).





dc_subsumption([], _).
dc_subsumption([Lit | Rest], Ex) :- 
    once(Lit =.. [P | _]),
    dc_subsumes(Lit, P, Ex),
    once(optimize_path_order(Rest, Rest1)),
    dc_subsumption(Rest1, Ex).
    


dc_subsumption1([], Ex, _, Class) :- 
    once(Class =.. [P | _]),
    not(dc_subsumes(Class, P, Ex)).
dc_subsumption1([Lit | Rest], Ex, Objs, Class) :- 
    once(Lit =.. [P | _]),
    dc_subsumes(Lit, P, Ex),
    once(optimize_path_order(Rest, Rest1)),
    dc_subsumption1(Rest1, Ex, Objs, Class).
