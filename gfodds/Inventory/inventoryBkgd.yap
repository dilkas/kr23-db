%%%%%%%%%%%%% GENERAL BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%

rule(eq(A, B), [1, 1], [eq(B, A)]).
rule(eq(A, B), [1, 1], [eq(A,C), eq(B,D), eq(C, D)]).

rule(not_eq(A, B), [1, 1], [not_eq(B, A)]).
rule(not_eq(A, B), [1, 1], [eq(A,C), eq(B,D), not_eq(C, D)]).


%%%%%%%%%%%%% DOMAIN SPECIFIC BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%

rule(not_eq(A,B), [1,1], [shop(A),not_shop(B)]).
rule(shop(A), [1], [shop(B),eq(A,B)]).
rule(not_shop(A), [1], [not_shop(B),eq(A,B)]).
rule(shop(A), [1], [not_empty(A)]). 

rule(not_eq(A,B), [1,1], [tfull(A),not_tfull(B)]).
rule(tfull(A), [1], [tfull(B),eq(A,B)]).
rule(not_tfull(A), [1], [not_tfull(B),eq(A,B)]).

rule(not_eq(A,B), [1,1], [tin(A,C),not_tin(B,D),eq(C,D)]).
rule(eq(A,B), [1,1], [tin(T1,A),tin(T2,B),eq(T1,T2)]).
rule(tin(A,B), [1,1], [tin(C,D),eq(A,C),eq(B,D)]).
rule(not_tin(A,B), [1,1], [not_tin(C,D),eq(A,C),eq(B,D)]).
rule(not_tin(A,B), [1,1], [tin(C,D),eq(A,C),not_eq(B,D)]).

rule(not_eq(A,B), [1,1], [empty(A),not_empty(B)]).
rule(empty(A), [1], [empty(B), eq(A,B)]).
rule(not_empty(A), [1], [not_empty(B),eq(A,B)]).

rule(not_eq(A,load), [1,1], [eq(A,unload)]).
rule(not_eq(A,load), [1,1], [eq(A,drive)]).
rule(not_eq(A,unload), [1,1], [eq(A,load)]).
rule(not_eq(A,unload), [1,1], [eq(A,drive)]).
rule(not_eq(A,drive), [1,1], [eq(A,load)]).
rule(not_eq(A,drive), [1,1], [eq(A,unload)]).



rule(false, [], [tin(T,L),not_tin(T,L)]).
rule(false, [], [empty(L),not_empty(L)]).
rule(false, [], [shop(L), not_shop(L)]).
rule(false, [], [depot(L), not_depot(L)]).
rule(false, [], [tfull(T), not_tfull(T)]).
rule(false, [], [eq(A,B), not_eq(A,B)]).




%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_readable_state(State, ReadableState) :-
    all(A, (member(A, [empty(_), tfull(_), tin(_,_), shop(_), depot(_)]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [empty(_),tfull(_),tin(_,_)]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


% Need to specify the training domain and the state to search from. 


trainingdomain3579(empty, [objects(location, [s1,s2,s3,s4,d]), objects(truck, [t1,t2])]).


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
    	Dom = [objects(location, [s1,s2,d]), objects(truck,[t1]), objects(maxobj, [a,a1])],
	all(St, (subset([empty(s1),empty(s2),tfull(t1)], S1), member(S2,[tin(t1,s1),tin(t1,s2),tin(t1,d)]), once((conc([S2 | S1], [shop(s1),shop(s2),depot(d),empty(d)], S3), completeStateDescription(S3, Dom, S4), convert_Ex(S4,St)))), All),
	write('interpretationObjs3579(empty,'), write(Dom), write(').'), nl, nl,
	write('interpretations3579(empty,'), write(All), write(').'), nl, nl.
