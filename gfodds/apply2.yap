

%% applyOp(+, +, +, -)
%% applyOp(N1, N2, Op, N)
%% True when N is the result of applying the numerical operation Op on N1 and N2. 
    
applyOp(N1, N2, +, N) :-
    N is N1 + N2,
    !.
applyOp(N1, N2, -, N) :- 
    N3 is N1 - N2, 
    maxNum(N3, 0.0, N),
    !.
applyOp(N1, N2, *, N) :-
    N is N1 * N2,
    !.
applyOp(N1, N2, div, N) :-
    N is N1/N2,
    !.
applyOp(N1, N2, max, N) :-
    maxNum(N1, N2, N),
    !.
applyOp(N1, N2, min, N) :-
    min(N1, N2, N),
    !.
applyOp(1.0, 1.0, and, 1.0) :-
    !.
applyOp(0.0, 1.0, and, 0.0) :- 
    !.
applyOp(0.0, 0.0, and, 0.0) :-
    !.
applyOp(_, _, and, 0.0).



%%%%%%%%%%%%%%   apply1 SHORTCUTS   %%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% apply1(+, +, +, +, +, -)
%% apply1(N1, D1, N2, D2, Op, I)
%% True when I is the index of the root of the FODD generated as a result 
%% of running the apply operation on the subtrees of D1 and D2 rooted at 
%% N1 and N2 resp. The operator for the apply is Op

apply1(node(I1, N1, _, _), _, node(I2, N2, _, _), _, _, I) :- 
%   write('I1 = '), write(I1), write(' I2 = '), write(I2), nl,
%   write('N1 = '), write(N1), write('  I1 = '), write(I1), write('  N2 = '), write(N2), write('  I2 = '), write(I2), nl, 
    recorded(apply,arc(pair(I1, I2), node(I, _, _, _)), _),
%    write(I), write(' returned'), nl, 
    !.
apply1(node(I1, N1, -1, -1), _, node(I2, N2, -1, -1), _, Op, I) :-
%   write('I1 = '), write(I1), write(' I2 = '), write(I2), nl,
    applyOp(N1, N2, Op, D1),
    leafprecision(LP),
    (LP is 0,
     D is D1
     ; 
     ((D1 = 0.0 ; D1 = 1.0), D = D1 ; float_precision(D1, LP, D))),
    (recorded(apply,arc(pair(_, _), node(I, D, -1, -1)), _),
%     abs((D2 - D)) =< 0.0,
     D3 = D
     ;
     D = 0.0,
     D3 = D,
     I = 0
     ;
     D = 1.0,
     D3 = D,
     I = 1
     ;
     getIndexCount(I),
     D3 = D),
     recorda(apply,arc(pair(I1,I2), node(I,D3,-1,-1)),_),
%    write(D3), write(' is a the leaf'), nl, 
%    write(I), write(' returned'), nl,
    !.
apply1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, Op, I) :- 
%    write('three '), 
%    write('I1 = '), write(I1), write(' I2 = '), write(I2), nl,
    order(N1, N2, lt),
    member(node(L1, Llit, Llt, Lrt), D1),
    member(node(R1, Rlit, Rlt, Rrt), D1),
    apply1(node(L1, Llit, Llt, Lrt), D1, node(I2, N2, L2, R2), D2, Op, L),
    apply1(node(R1, Rlit, Rlt, Rrt), D1, node(I2, N2, L2, R2), D2, Op, R),
    (L = R, 
     I = L,
     recorded(apply,arc(pair(_,_),node(I,N,IL,IR)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N,IL,IR)),_)
     ;
     recorded(apply,arc(pair(_,_),node(I,N1,L,R)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N1,L,R)),_)
     ;
     getIndexCount(I),
     recorda(apply,arc(pair(I1,I2),node(I,N1,L,R)),_)),
%    write(I), write(' returned'), nl,
    !.
apply1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, Op, I) :- 
%   write('four '), write('I1 = '), write(I1), write(' I2 = '), write(I2), nl,
    order(N1, N2, gt),
    member(node(L2, Llit, Llt, Lrt), D2),
    member(node(R2, Rlit, Rlt, Rrt), D2),
    apply1(node(I1, N1, L1, R1), D1, node(L2, Llit, Llt, Lrt), D2, Op, L),
    apply1(node(I1, N1, L1, R1), D1, node(R2, Rlit, Rlt, Rrt), D2, Op, R), 
    (L = R, 
     I = L,
     recorded(apply,arc(pair(_,_),node(I,N,IL,IR)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N,IL,IR)),_)
     ;
     recorded(apply,arc(pair(_,_),node(I,N2,L,R)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N2,L,R)),_)
     ;
     getIndexCount(I),
     recorda(apply,arc(pair(I1,I2),node(I,N2,L,R)),_)),
%    write(I), write(' returned'), nl,
    !.
apply1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, Op, I) :- 
%    write('five '), write('I1 = '), write(I1), write(' I2 = '), write(I2), nl, 
    order(N1, N2, eq),
    member(node(L1, Llit1, Llt1, Lrt1), D1),
    member(node(R1, Rlit1, Rlt1, Rrt1), D1),
    member(node(L2, Llit2, Llt2, Lrt2), D2),
    member(node(R2, Rlit2, Rlt2, Rrt2), D2),
    apply1(node(L1, Llit1, Llt1, Lrt1), D1, node(L2, Llit2, Llt2, Lrt2), D2, Op, L),
    apply1(node(R1, Rlit1, Rlt1, Rrt1), D1, node(R2, Rlit2, Rlt2, Rrt2), D2, Op, R), 
    (L = R, 
     I = L,
     recorded(apply,arc(pair(_,_),node(I,N,IL,IR)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N,IL,IR)),_)
     ; 
     recorded(apply,arc(pair(_,_),node(I,N1,L,R)),_),
     recorda(apply,arc(pair(I1,I2),node(I,N1,L,R)),_)
     ;
     getIndexCount(I),      
     recorda(apply,arc(pair(I1,I2),node(I,N1,L,R)),_)).     
%    write(I), write(' returned'), nl.





%%%%%%% APPLY SHORTCUTS %%%%%%%%%%%%%
%%%%%%% * SHORTCUTS %%%%%%%%%%%%%%%%%
apply(_, _, node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], *, [node(0, 0.0, -1, -1)]) :- 
    !.
apply(node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], _, _, *, [node(0, 0.0, -1, -1)]) :- 
    !.
apply(_, D, node(1, 1.0, -1, -1), [node(1, 1.0, -1, -1)], *, D) :- 
    !.
apply(node(1, 1.0, -1, -1), [node(1, 1.0, -1, -1)], _, D, *, D) :- 
    !.


%%%%%% + SHORTCUTS %%%%%%%%%%%%%%%%%
apply(_, D, node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], +, D) :- 
    !.
apply(node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], _, D, +, D) :- 
    !.


%%%%%% max SHORTCUTS %%%%%%%%%%%%%%%%%
%apply(_, D, node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], max, D) :- 
%    !.
apply(node(0, 0.0, -1, -1), [node(0, 0.0, -1, -1)], _, D, max, D) :- 
    !.
apply(Root1, D1, Root2, D2, max, D) :- 
    recorded(varCount, Start, _),
    eraseall(varCount), 
    Start1 is Start + 1,
    recorda(varCount,Start1,_),
    getNewVars(Start,1,[NewVar]),
    apply(node(2,eq(a,NewVar),1,0), [node(2,eq(a,NewVar),1,0),node(1,1.0,-1,-1),node(0,0.0,-1,-1)], Root1, D1, *, D1eq),
    apply(node(2,eq(a,NewVar),0,1), [node(2,eq(a,NewVar),0,1),node(1,1.0,-1,-1),node(0,0.0,-1,-1)], Root2, D2, *, D2eq),
    D1eq = [D1eqRoot | _],
    D2eq = [D2eqRoot | _],
    apply(D1eqRoot, D1eq, D2eqRoot, D2eq, +, D).



%% apply(+, +, +, +, +, -)
%% apply(N1, D1, N2, D2, Op, Res)
%% True when Res is the FODD obtained by running the apply operation on the subFODDs of 
%% D1 and D2 rooted at nodes N1 and N2 resp. and strong reducing the result. Since 
%% the resulting FODD is build bottom up using dynamic programming, indexCount(X) is 
%% used to assign indices to the generated nodes of Res.  

apply(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, Op, Res) :- 
    eraseall(indexCount),
    recorda(indexCount,2,_),
    apply1(node(I1, N1, L1, R1), D1, node(I2, N2, L2, R2), D2, Op, _),
    eraseall(indexCount),
    all(X, recorded(apply, arc(_,X), _), L),
    eraseall(apply),
    insertsort(L, Res1),
    !,
    (modelchecking,
     Res = Res1
     ;
     r5(Res1, Res2),
     eraseall(nf),
     Res2 = [Res2Root | _],
     eraseall(indexCount),
     recorda(indexCount,2,_),    
     apply1(Res2Root, Res2, node(1,1.0,-1,-1),[node(1,1.0,-1,-1)],*,_),
     eraseall(indexCount),
     all(X, recorded(apply, arc(_,X), _), Res3),
     eraseall(apply),
     insertsort(Res3, Res)).
    

apply(D1, D2, Op, R) :- 
    D1 = [D1Root | _],
    D2 = [D2Root | _],	  
    apply(D1Root, D1, D2Root, D2, Op, R).
