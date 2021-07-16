%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_readable_state(State, ReadableState) :-
    all(A, (member(A, [waiting(_,_), pin(_), at(_)]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [pin(_),at(_),waiting(_,_)]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


% Need to specify the training domain and the state to search from. 

trainingdomain3579(empty, [objects(floor, [f1,f2,f3,f4])]).


startstate3579(_, [rg([
at(f1)
])]).



generate_complete_set :-
    Dom = [objects(floor,[f1,f2]),objects(maxobj,[a,a1])],	
    all(S, (subset([waiting(f1,f2),waiting(f2,f1)],S1), subset([pin(f1),pin(f2)],S2), member(S3,[at(f1),at(f2)]), once((flatten([S1,S2,[S3]],S6), completeStateDescription(S6,Dom,S7),convert_Ex(S7,S)))), AllEx),
%    random_permutation(AllEx1,25,AllEx),
    write('interpretationObjs3579(waiting, '), write(Dom), write(').'), nl,nl,
    write('interpretations3579(waiting,'),    
    write(AllEx),
    write(').'),
    nl.
