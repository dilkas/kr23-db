%%%%%%%%%%%%% GENERAL BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%

rule(eq(A, B), [1, 1], [eq(B, A)]).
rule(eq(A, B), [1, 1], [eq(A,C), eq(B,D), eq(C, D)]).

rule(not_eq(A, B), [1, 1], [not_eq(B, A)]).
rule(not_eq(A, B), [1, 1], [eq(A,C), eq(B,D), not_eq(C, D)]).


%%%%%%%%%%%%% DOMAIN SPECIFIC BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%


rule(eq(C1, C2), [1, 1], [bin(B1, C1), bin(B2, C2), eq(B1, B2)]).
rule(eq(C1, C2), [1, 1], [tin(T1, C1), tin(T2, C2), eq(T1, T2)]).
rule(eq(T1, T2), [1, 1], [ont(B1, T1), ont(B2, T2), eq(B1, B2)]).
rule(eq(P1, P2), [1, 1], [onp(B1, P1), onp(B2, P2), eq(B1, B2)]).
rule(eq(C1, C2), [1, 1], [dest(B1, C1), dest(B2, C2), eq(B1, B2)]).



rule(not_eq(C1, C2), [1, 1], [bin(B1, C1), not_bin(B2, C2), eq(B1, B2)]).
rule(not_eq(C1, C2), [1, 1], [tin(T1, C1), not_tin(T2, C2), eq(T1, T2)]).
rule(not_eq(C1, C2), [1, 1], [pin(P1, C1), not_pin(P2, C2), eq(P1, P2)]).


rule(not_eq(T1, T2), [1, 1], [ont(B1, T1), not_ont(B2, T2), eq(B1, B2)]).
rule(not_eq(P1, P2), [1, 1], [onp(B1, P1), not_onp(B2, P2), eq(B1, B2)]).
rule(not_eq(T1, T2), [1, 1], [tin(T1, C1), tin(T2, C2), not_eq(C1, C2)]).
rule(not_eq(P1, P2), [1, 1], [pin(P1, C1), pin(P2, C2), not_eq(C1, C2)]).


rule(not_eq(B1, B2), [1, 1], [bin(B1, C1), bin(B2, C2), not_eq(C1, C2)]).
rule(not_eq(B1, B2), [1, 1], [ont(B1, T1), ont(B2, T2), not_eq(T1, T2)]).
rule(not_eq(B1, B2), [1, 1], [onp(B1, P1), onp(B2, P2), not_eq(P1, P2)]).
rule(not_eq(B1, B2), [1, 1], [bin(B1, _), ont(B2, _)]).
rule(not_eq(B1, B2), [1, 1], [bin(B1, _), onp(B2, _)]).

rule(bin(B1, C1), [1, 1], [bin(B2, C2), eq(B1, B2), eq(C1, C2)]).

rule(not_bin(B1, C1), [1, 0], [ont(B2, _), eq(B1, B2), eq(C1, C1)]).
rule(not_bin(B1, C1), [1, 0], [onp(B2, _), eq(B1, B2), eq(C1, C1)]).


rule(not_bin(B1, C1), [1, 1], [bin(B2, C2), eq(B1, B2), not_eq(C1, C2)]).
rule(not_bin(B1, C1), [1, 1], [not_bin(B2, C2), eq(B1, B2), eq(C1, C2)]).

rule(tin(T1, C1), [1, 1], [tin(T2, C2), eq(T1, T2), eq(C1, C2)]).
rule(pin(P1, C1), [1, 1], [pin(P2, C2), eq(P1, P2), eq(C1, C2)]).

rule(not_tin(T1, C1), [1, 1], [tin(T2, C2), eq(T1, T2), not_eq(C1, C2)]).
rule(not_tin(T1, C1), [1, 1], [not_tin(T2, C2), eq(T1, T2), eq(C1, C2)]).

rule(not_pin(P1, C1), [1, 1], [pin(P2, C2), eq(P1, P2), not_eq(C1, C2)]).
rule(not_pin(P1, C1), [1, 1], [not_pin(P2, C2), eq(P1, P2), eq(C1, C2)]).


rule(ont(B1, T1), [1, 1], [ont(B2, T2), eq(B1, B2), eq(T1, T2)]).
rule(onp(B1, P1), [1, 1], [onp(B2, P2), eq(B1, B2), eq(P1, P2)]).

rule(not_ont(B1, T1), [1, 0], [bin(B2, _), eq(B1, B2), eq(T1, T1)]).
rule(not_ont(B1, T1), [1, 1], [not_ont(B2, T2), eq(B1, B2), eq(T1, T2)]).
rule(not_ont(B1, T1), [1, 1], [ont(B2, T2), eq(B1, B2), not_eq(T1, T2)]).
rule(not_ont(B1, T1), [1, 0], [onp(B2, P2), eq(B1, B2), eq(T1, T1)]).

rule(not_onp(B1, P1), [1, 0], [bin(B2, _), eq(B1, B2), eq(P1, P1)]).
rule(not_onp(B1, P1), [1, 1], [not_onp(B2, P2), eq(B1, B2), eq(P1, P2)]).
rule(not_onp(B1, P1), [1, 1], [onp(B2, P2), eq(B1, B2), not_eq(P1, P2)]).
rule(not_onp(B1, P1), [1, 0], [ont(B2, T2), eq(B1, B2), eq(P1, P1)]).

rule(dest(B1, C1), [1, 1], [dest(B2, C2), eq(B1, B2), eq(C1, C2)]).
rule(not_dest(B1, C1), [1, 1], [dest(B2, C2), eq(B1, B2), not_eq(C1, C2)]).




%%%%%%%%%%%%% MODEL GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_readable_state(State, ReadableState) :-
    all(A, (member(A, [at(_), pup(_), pdown(_,_), pinup, pindown,open,up]), member(A, State)), ReadableState).

get_readable_state_conv(State, ReadableState) :- 
    all(A, (member(A, [at(_), pup(_), pdown(_,_), pinup, pindown,open,up]), once((functor(A,P,_), member([P | LitList], State))), member(A, LitList)), ReadableState).


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
    	Dom = [objects(floor, [f1,top,bottom]),objects(maxobj, [a,a1])],
	all(St, (member(S1,[at(top),at(f1),at(bottom)]), subset([pup(bottom),pup(f1),pdown(top),pdown(f1),open,up,pinup,pindown],S2), once((flatten([[S1], S2, [above(f1,bottom),above(top,f1)]], S3), completeStateDescription(S3, Dom, S4), convert_Ex(S4,St)))), All),
	random_permutation(All, 30, All30),
	write('interpretationObjs3579(pup,'), write(Dom), write(').'), nl, nl,
	write('interpretations3579(pup,'), write(All30), write(').'), nl, nl.
