
%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_readable_state(State, ReadableState) :-
    all(A, (member(A, [at(_), pup(_,_), pdown(_,_), pinup(_), pindown(_),up]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [at(_), pup(_,_), pdown(_,_), pinup(_), pindown(_),up]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


% Need to specify the training domain and the state to search from. 


trainingdomain3579(pup, [objects(floor, [f1,f2,top,bottom])]).


startstate3579(_, [rg([
at(bottom),
above(f1,bottom),
above(f2,f1),
above(top,f2),
up
])]).



generate_complete_set :-
    	Dom = [objects(person,[p1,p2,p3,p4]),objects(floor, [top,bottom]),objects(maxobj, [a,a1])],
	all(St, (member(S1,[at(top),at(bottom)]), subset([pup(p1,bottom),pdown(p2,top),pinup(p3),pindown(p4)],S2), once((flatten([[S1], S2, [above(top,bottom)]], S3), completeStateDescription(S3, Dom, S4), convert_Ex(S4,St)))), All),
%	random_permutation(All, 50, All50),
	write('interpretationObjs3579(pinup,'), write(Dom), write(').'), nl, nl,
	write('interpretations3579(pinup,'), write(All), write(').'), nl, nl.
