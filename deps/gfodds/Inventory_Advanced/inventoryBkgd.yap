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
    all(A, (member(A, [level0(_), level1(_), tfull(_), tin(_,_)]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [sc(_), level0(_),level1(_),tfull(_),tin(_,_)]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


% Need to specify the training domain and the state to search from. 


trainingdomain3579(empty, [objects(location, [s1,s2,s3,s4,d]), objects(truck, [t1])]).


startstate3579(_, [rg([
depot(d),
shop(s1),level0(s1),
shop(s2),level1(s2),
shop(s3),level0(s3),
shop(s4),
tfull(t1),
tin(t1,d),
freq(s1),
freq(s2)
])]).



generate_complete_set :-
    Dom = [objects(location,[s1,s2,d]),objects(truck,[t1]),objects(maxobj,[a,a1])],	
    all(S, (subset([level0(s1),level1(s1)],S1), once(length(S1, L1)), once((L1 < 2)), subset([level0(s2),level1(s2)],S2), once(length(S2, L2)), once((L2 < 2)), member(S3,[tin(t1,s1),tin(t1,s2),tin(t1,d)]), subset([tfull(t1)], S4), once((flatten([S1,S2,[S3],S4,[shop(s1),shop(s2),depot(d),freq(s1)]],S6), completeStateDescription(S6,Dom,S7),convert_Ex(S7,S)))), AllEx),
    write('interpretationObjs3579(level0, '), write(Dom), write(').'), nl,nl,
    write('interpretations3579(level0,'),    
    write(AllEx),
    write(').'),
    nl.


generate_complete_set1 :-
    Dom = [objects(location,[s1,d]),objects(truck,[t1]),objects(maxobj,[a,a1])],	
    all(S, (subset([level0(s1),level1(s1)],S1), once(length(S1, L1)), once((L1 < 2)), member(S3,[tin(t1,s1),tin(t1,d)]), subset([tfull(t1)], S4), once((flatten([S1,[S3],S4,[shop(s1),depot(d),freq(s1)]],S6), completeStateDescription(S6,Dom,S7),convert_Ex(S7,S)))), AllEx),
    write('interpretationObjs3579(level0, '), write(Dom), write(').'), nl,nl,
    write('interpretations3579(level0,'),    
    write(AllEx),
    write(').'),
    nl.
