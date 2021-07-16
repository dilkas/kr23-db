:- dynamic 
    indexCount/1,
    varCount/1,
    redVarCount/1.


:- use_module(library(lists)).
:- use_module(library(ordsets)).


float_precision1(X, P, Y) :-
    Y is truncate((X * 10^P))/(10^P).



float_precision(X, P, Y) :-
    Z is integer(X),
    Y is 1.0 * (Z - (Z rem P)).



%% min(+, +, -)
%% True when the 3rd argument is lexicographically the minimum of the first two.

min(X, Y, X) :- 
    X @< Y,
    !.
min(_, Y, Y).



%% max(+, +, -)
%% True when the 3rd argument is lexicographically the maximum of the first two.

max(X, Y, X) :- 
    X @> Y,
    !.
max(_, Y, Y).



%% maxNum(+, +, -)
%% True when the 3rd argument is numerically the maximum of the first two.


maxNum(X, Y, X) :- 
    X > Y,
    !.
maxNum(_, Y, Y).


%% maxList(+, -)
%% True when the first argument is a list and the 2rd argument is lexicographically the maximum element of the list.

maxList([Elem], Elem) :- 
    !.
maxList([Elem1, Elem2 | Rest], Max) :- 
    maxList([Elem2 | Rest], MaxRest), 
    max(Elem1, MaxRest, Max).


maxNumList([Elem], Elem) :-
    !.
maxNumList([Elem | Rest], Max) :-
    maxNumList(Rest, MaxRest),
    maxNum(Elem, MaxRest, Max).



%% del(+, +, -)
%% True when the 2nd argument is a list, the 1st is an element of the list and the 3rd argument is the list with the element deleted.

del(X, [X | Tail], Tail).
del(X, [Y | Tail], [Y | Tail1]) :-
    del(X, Tail, Tail1).



%% delAll(+, +, -)
%% True when the 2nd argument is a list, the 1st is a sublist of the list and the 
%% 3rd argument is the list with the elements in the sublist deleted deleted.

delAll([], L, L) :- 
    !.
delAll([X | Rest], L, R) :- 
    del(X, L, L1),
    delAll(Rest, L1, R).




%% delPrefix(+, +, -)
%% delPrefix(L, P, R)
%% True when P is a prefix list of list L and R is the remaining part of L after the prefix.

delPrefix(L, P, R) :-
    delAll(P, L, R).



%% conc(+, +, -)
%% True when the 1st and 2nd arguments are lists and the3rd argument is the 
%% concatenation of the two lists.

conc([], L, L) :- 
    !.
conc([X | L1], L2, [X | L3]) :- 
    conc(L1, L2, L3).





%reverse([], []) :- 
%    !.
%reverse([First | Rest], Rev) :- 
%    reverse(Rest, RevRest),
%    conc(RevRest, [First], Rev).

%remove_duplicates(L1, L) :- 
%    reverse(L1, L2),
%    remDup(L2, L3), 
%    reverse(L3, L).


%% remDup(+, -)
%% True when the 1st argment is a list and the 2nd argument is the same list with duplicate elements removed. 

remDup([], []) :- 
    !.
remDup([First | Rest], ResList) :- 
    member(First, Rest),
    remDup(Rest, ResList),
    !.
remDup([First | Rest], [First | ResList]) :- 
    remDup(Rest, ResList).


%% member(-, +)
%% member(X, Y)
%% True when X is an element of list Y. 

%member(X, [X | _]).
%member(X, [_ | Rest]) :- 
%    member(X, Rest).


abstract_member(X, [Y | _]) :- 
    X == Y.
abstract_member(X, [_ | Rest]) :- 
    abstract_member(X, Rest).



%% diff(+, +, -)
%% diff(L1, L2, L)
%% True when list L is the list L1 with those elements deleted that are also in L2.

diff(L, [], L) :- 
    !.
diff(L, [First | Rest], D) :- 
    del(First, L, L1),
    diff(L1, Rest, D),
    !.
diff(L, [_ | Rest], D) :- 
    diff(L, Rest, D).



%% my_intersection(+, +, -)
%% my_intersection(L1, L2, L)
%% True when L contains the elements that are in list L1 and list L2 and none other. 

my_intersection(_, [], []) :- 
    !.
my_intersection(L1, [X | Rest], [X | L]) :- 
    member(X, L1),
    my_intersection(L1, Rest, L),
    !.
my_intersection(L1, [_ | Rest], L) :- 
    my_intersection(L1, Rest, L).





%% union(+, +, -)
%% union(L1, L2, L)
%% True when L contains the elements that are in list L1 or list L2 and none other. 

union([], L, L) :- 
    !.
union([X | Rest], L2, [X | L]) :- 
    not(memberchk(X, L2)),
    union(Rest, L2, L),
    !.
union([_ | Rest], L2, L) :- 
    union(Rest, L2, L).






%% subset(+, +)
%% subset(L1, L2)
%% True when L1 is a subset of L2

%subset([], _) :-
%    !.
%subset([X | L1], L2) :-
%    member(X, L2),
%    subset(L1, L2).



subset([], []).
subset([X | L], [X | L1]) :-
    subset(L, L1).
subset([_ | L], L1) :-
    subset(L, L1).


%%%% random subset of length N


random_permutation(_, 0, []) :-
    !.
random_permutation(L, N, [Item | Rest]) :-
    length(L, Len),
    random(0, Len, X),
    nth0(X, L, Item, L1),
    N1 is N - 1,
    random_permutation(L1, N1, Rest).


   



%% insert(+, +, -)
%% insert(X, L, Y)
%% True when L is a list, and Y is the list with X inserted at the position where 
%% all elements following it are lexicographically less than X. 
%% E.g. insert(4, [1, 2, 3, 5], Y) would yeild Y = [4, 1, 2, 3, 5] 
%% and insert(4, [5, 3, 2, 1], Y) would yeild Y = [5, 4, 3, 2, 1]. 

insert(X, [], [X]) :- 
    !.
insert(X, [Y | Rest], [X, Y | Rest]) :- 
    X @> Y,
    !.
insert(X, [X | Rest], [X | Rest]) :- 
    !.
insert(X, [Y | Rest], [Y | Res]) :- 
    X @< Y, 
    !,
    insert(X, Rest, Res).



%% insertRev(+, +, -)
%% insertRev(X, L, Y)
%% True when L is a list, and Y is the list with X inserted at the position where 
%% all elements following it are lexicographically greater than X. 
%% E.g. insert(4, [1, 2, 3, 5], Y) would yeild Y = [1, 2, 3, 4, 5] 
%% and insert(4, [5, 3, 2, 1], Y) would yeild Y = [4, 5, 3, 2, 1]. 

insertRev(X, [], [X]) :- 
    !.
insertRev(X, [Y | Rest], [X, Y | Rest]) :- 
    X @< Y,
    !.
insertRev(X, [X | Rest], [X | Rest]) :- 
    !.
insertRev(X, [Y | Rest], [Y | Res]) :- 
    X @> Y, 
    !,
    insertRev(X, Rest, Res).



%% insertsort(+, -)
%% insertsort(L1, L2)
%% This is a simple insertion sort routine that uses insert(X, L, Y) rather than 
%% insertRev(X, L, Y) and thus sorts in descending order.

insertsort([], []) :- 
    !.
insertsort([X | Rest], Sorted) :- 
    insertsort(Rest, SortedRest),
    insert(X, SortedRest, Sorted).


%% batchInsert(+, +, -)
%% batchInsert(L1, L2, L)
%% True when L is the list L1 with all elements in L2 inserted using insert(X, L, Y)

batchInsert([], L2, L2) :- 
    !.
batchInsert([X | Rest], L2, L) :- 
    insert(X, L2, L3),	       
    batchInsert(Rest, L3, L).
    


%% batchInsertRev(+, +, -)
%% batchInsertRev(L1, L2, L)
%% True when L is the list L1 with all elements in L2 inserted using insertRev(X, L, Y)

batchInsertRev([], L2, L2) :- 
    !.
batchInsertRev([X | Rest], L2, L) :- 
    batchInsertRev(Rest, L2, L3),
    insertRev(X, L3, L).



%% maxVar(+)
%% maxVar(X)
%% True when X is a domain variable i.e. it is a string starting with a 'v' followed 
%% by a nonnegative integer. E.g. v0113

maxVar(X) :- 
    name(X, [118 | N]),
    name(Y, N),
    integer(Y).



%% avgVar(+)
%% avgVar(X)
%% True when X is a domain variable i.e. it is a string starting with a 'b' followed 
%% by a nonnegative integer. E.g. b0113

avgVar(X) :- 
    name(X, [98 | N]),
    name(Y, N),
    integer(Y).


%% domVar(+)
%% domVar(X)
%% True when X is a domain variable i.e. it is a string starting with a 'v' or 'b' followed 
%% by a nonnegative integer. E.g. v0113 or b12

domVar(X) :- 
    maxVar(X),
    !.
domVar(X) :-
    avgVar(X).


%% actPar(+)
%% actPar(X)
%% True when X is an action parameter i.e. it is a string starting with a 'p' followed 
%% by a nonnegative integer. E.g. p014

actPar(X) :- 
    name(X, [112 | N]),
    name(Y, N),
    integer(Y).
    


%% domConst(+)
%% domConst(X)
%% True when X is a domain constant. 

domConst(X) :- 
    domainConsts(Y),
    member(X, Y).




%% getIndexCount(-)
%% getIndexCount(X)
%% Called by the 'apply' routine, this clause returns the next node index available when 
%% building the resultant FODD in an apply operation. 

getIndexCount(I) :- 
    recorded(indexCount,I,RefIndexCount),
    erase(RefIndexCount),
    C is I + 1,
    !,
    recorda(indexCount,C,_).



%% getVarCount(-)
%% getVarCount(X)
%% Returns the next available domain variable

getVarCount(C) :- 
    recorded(varCount,C,RefVarCount),
    erase(RefVarCount),
    C1 is C + 1,
    recorda(varCount,C1,_).


%% getRedVarCount(-)
%% Returns the next available domain variable to bind to a prolog variable during 
%% theorem proving. Only used with rich background knowledge.

getRedVarCount(C) :- 
    recorded(redVarCount,C,RefRedVarCount),
    erase(RefRedVarCount),
    C1 is C + 1,
    recorda(redVarCount,C1,_).


%% getNeg(+, -) 
%% getNeg(X, Y)
%% True when X is an atom and Y is the same atom with the prefix 'not_'. 
%% E.g. getNeg(p(1), Y) will yeild Y = not_p(1). 

getNeg(L, N) :- 
    L =.. [P | V],
    name(P, PName),
    conc("not_", PName, NegPName),
    name(NegP, NegPName),
    N =.. [NegP | V].



%% isNeg(+)
%% isNeg(X)
%% True when X is a negative literal
%% E.g. isNeg(not_p(a)) will return true.

isNeg(L) :-
    once(L =.. [P | _]),
    name(P, [110,111,116,95 | _]).



    

%% getSubsKey(+, -)
%% getSubsKey(X, Y)
%% True when Y is a list of elements of the form A:B where A is an element of the list X and 
%% B is a new prolog variable. This routine effectively generates a key that is used to
%% variablize terms and formulas.

getSubsKey([], []) :- 
	!.
getSubsKey([First | Rest], [First:One | Other]) :- 
    copy_term(X, One), 
    !, 
    getSubsKey(Rest, Other).


%% getGroundKey(+, +, -)
%% getGroundKey(L1, L2, K)
%% True when K is a list of elements of the form A:B where A is an element of list L1 and
%% B is the corresponding element of list L2.

getGroundKey([], [], []) :- 
    !.
getGroundKey([X | Xrest], [Y | Yrest], [X:Y | XYRest]) :- 
    getGroundKey(Xrest, Yrest, XYRest).



%% variablizeNumList(+, +, -)
%% variablizeNumList(L1, K, L)
%% True when K is a 'key' list with elements of the form A:B, and L is the list L1
%% with all A's substituted by B's. E.g. variablizeNumList([1, 2, 3], [1:5, 2:6], L)
%% will yeild L = [5, 6, 3].

variablizeNumList([], _, []) :- 
    !.
variablizeNumList([First | Rest], Key, [One | Other]) :- 
%    member(First1:One, Key),
%    First == First1,
    member(First:One, Key), 
    variablizeNumList(Rest, Key, Other),
    !.
variablizeNumList([First | Rest], Key, [First | Other]) :- 
    variablizeNumList(Rest, Key, Other).



%% variablizeTerm(+, +, -) 
%% variablizeTerm(T1, K, T)
%% True when T is a the term T1 with variables substituted according the key K.

variablizeTerm(Term, [], Term) :- 
    !.
variablizeTerm(Term, Key, Res) :- 
    Term =.. [P | NumList], 
    variablizeNumList(NumList, Key, VarList), 
    Res =.. [P | VarList].


%% variablizeFormula(+, +, -)
%% variablizeFormula(F, K, L)
%% True when L is the formula (list of terms) F 
%% variablized (all variables substituted according to key K).


variablizeFormula([], _, []) :- 
    !.
variablizeFormula([First | Rest], Key, [One | Other]) :- 
    variablizeTerm(First, Key, One), 
    variablizeFormula(Rest, Key, Other).


%% getNewVars(+, +, -)
%% getNewVars(S, C, L)
%% True when L is a list of C lexicographically increasing domain variables starting from S.
%% E.g. getNewVars(2, 3, L) will yeild L = [v2, v3, v4]   

getNewVars(_, 0, []) :- 
    !.
getNewVars(Start, Count, [Var | Rest]) :- 
    name(Start, N), 
    name(Var, [118 | N]),
    Count1 is Count - 1,
    Start1 is Start + 1,
    getNewVars(Start1, Count1, Rest).



%% getNewParVars(+, +, -)
%% getNewParVars(S, C, I, L)
%% True when L is a list of C lexicographically increasing action parameters starting from S.
%% with a prefix I.
%% E.g. getNewParVars(2, 3, 0, L) will yeild L = [p02, p03, p04]   

getNewParVars(_, 0, _, []) :- 
    !.
getNewParVars(Start, Count, Prefix, [Var | Rest]) :- 
    Count1 is Count - 1,
    Start1 is Start + 1,
    name(Start1, N),
    name(Prefix, PI),
    flatten([[118], PI, N], Vname),
    name(Var, Vname),
    getNewParVars(Start1, Count1, Prefix, Rest).





lastElem([X], X) :- 
    !.
lastElem([_ | Y], Z) :- 
    last(Y, Z).



isMember(X, []) :- 
    !,
    fail.
isMember(X, [Y | L]) :-
    X == Y,
    !.
isMember(X, [_ | L]) :-
    isMember(X, L).


removeAll([], _, []) :- 
    !.
removeAll([X | L1], L2, L) :-
    isMember(X, L2),
    removeAll(L1, L2, L),
    !.
removeAll([X | L1], L2, [X | L]) :-
    removeAll(L1, L2, L).





mklist(Val,0,L,L).
mklist(Val,N,L1,L) :- N1 is N-1, L2=[Val|L1], mklist(Val,N1,L2,L).

get_mean(L, Mean) :-
    length(L, Len),
    sum_list(L, Sum),
    Mean is Sum/Len.



		
get_stdev(L, Stdev) :-
    length(L, Len),
    get_mean(L, Mean),
    findall(Numer, (member(X,L), once((Y is X - Mean)), once((Numer is Y*Y))), AllNumer),
    sum_list(AllNumer, AllNumerSum),
    Len1 is Len - 1,
    Variance is AllNumerSum/Len1,
    Stdev is sqrt(Variance).	 


%%% random subset
