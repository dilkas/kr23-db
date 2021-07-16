
%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_readable_state(State, ReadableState) :-
    all(A, (member(A, [at(_), pup(_), pdown(_), pinup, pindown,up]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [at(_), pup(_), pdown(_), pinup, pindown,up]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


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
    	Dom = [objects(floor, [top,f1,bottom]),objects(maxobj, [a,a1])],
	all(St, (member(S1,[at(top),at(f1),at(bottom)]), subset([pup(bottom),pup(f1),pdown(f1),pdown(top),up,pinup,pindown],S2), once((flatten([[S1], S2, [above(top,bottom),above(top,f1),above(f1,bottom)]], S3), completeStateDescription(S3, Dom, S4), convert_Ex(S4,St)))), All),
%	random_permutation(All, 30, All30),
	write('interpretationObjs3579(pinup,'), write(Dom), write(').'), nl, nl,
	write('interpretations3579(pinup,'), write(All), write(').'), nl, nl.
