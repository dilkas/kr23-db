%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_readable_state(State, ReadableState) :-
    all(A, (member(A, [cust(_,_), empty(_), tfull(_), tin(_,_), shop(_), depot(_)]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [cust(_,_), empty(_),tfull(_),tin(_,_)]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


% Need to specify the training domain and the state to search from. 


trainingdomain3579(empty, [objects(person,[]), objects(location, [s1,s2,s3,s4,d]), objects(truck, [t1,t2])]).


startstate3579(_, [rg([
depot(d),
shop(s1),empty(s1),
shop(s2),empty(s2),
shop(s3),
shop(s4),
tfull(t1),
tin(t1,d),
tin(t2,d)
])]).



generate_complete_set :-
    	Dom = [objects(person,[p1]), objects(location, [s1,s2,d]), objects(truck,[t1]), objects(maxobj, [a,a1])],
	all(St, (subset([empty(s1),empty(s2),cust(p1,s1),tfull(t1)], S1), member(S2,[tin(t1,s1),tin(t1,s2),tin(t1,d)]), once((conc([S2 | S1], [shop(s1),depot(d),empty(d)], S3), completeStateDescription(S3, Dom, S4), convert_Ex(S4,St)))), All),
	write('interpretationObjs3579(cust,'), write(Dom), write(').'), nl, nl,
	write('interpretations3579(cust,'), write(All), write(').'), nl, nl.
