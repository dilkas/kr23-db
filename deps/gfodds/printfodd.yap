
%%%%%%%%%%%%% PRINT (G)FODD %%%%%%%%%

pp(D) :-
    D = [node(I, _, _, _) | _],
    prettyprint(I, D, 0, _).

prettyprint(I, D, Level, Res) :- 
    del(node(I, Lit, -1, -1), D, Res),
    tab(Level),
    write(Lit), write(' - ['), write(I), write(']'), nl,
    !.
prettyprint(I, D, Level, Res) :- 
    del(node(I, Lit, L, R), D, D1),
    tab(Level), 
    write(Lit), write(' - ['), write(I), write(']'), nl,
    Level1 is Level + 5,
    prettyprint(L, D1, Level1, D2),
    prettyprint(R, D2, Level1, Res),
    !.
prettyprint(I, D, Level, D) :- 
    tab(Level),
    write('['), write(I), write(']'), nl.
     




%%%%%%%%% PRINT VE TABLE %%%%%%%%%

print_row([]) :- 
    !.
print_row([X | Rest]) :- 
    write(X),
    tab(2),
    print_row(Rest).	     




print_lit_table([]) :- 
    !.
print_lit_table([Row | Table]) :- 
    print_row(Row),
    nl, 
    print_lit_table(Table).





print_table([]) :- 
    !.
print_table([[x, y | Vars] | Table]) :-
    print_row(Vars),
    write(map), nl,
    print_table(Table),
    !.
print_table([[Value, _ | Row] | Table]) :-    		     
    print_row(Row),
    write(Value), nl,
    print_table(Table).






%%%%%%%%%%%%%%%%%% PRINT MDD %%%%%%%%%%%%%%%


ppmdd(D) :-
    D = [node(I, V, EL) | _],
    del(node(I,V,EL),D,D1),
    write(V), write(' - ['), write(I), write(']'), 
    (number(V) -> (write('--'), write(EL), nl) ; (nl, findall([Edge,Index], member(edge(Edge,Index),EL), EL1))),
    prettyprint_mdd(EL1, D1, 5, _).



prettyprint_mdd([], D, _, D) :- 
    !.
prettyprint_mdd([[E,I] | Rest], D, Level, Res) :- 
    once(del(node(I, Lit, EL), D, Res1)),
    number(Lit),
    tab(Level),
    write(E),write('--'),write('['),write(I),write(']--'),write(Lit),write('--'), write(EL),nl, 
    prettyprint_mdd(Rest, Res1, Level, Res),
    !.
prettyprint_mdd([[E,I] | Rest], D, Level, Res) :- 
    del(node(I, Lit, EL), D, D1),
    tab(Level), 
    write(E), write('--'),write('['),write(I),write(']--'),write(Lit),nl,
    Level1 is Level + 5,
    findall([Edge,Index], member(edge(Edge,Index),EL), EL1),
    prettyprint_mdd(EL1, D1, Level1, D2),
    prettyprint_mdd(Rest, D2, Level, Res),
    !.
prettyprint_mdd([[E,I] | Rest], D, Level, Res) :- 
    tab(Level),
    write(E), write('--'),write('['), write(I), write(']'), nl,
    prettyprint_mdd(Rest, D, Level, Res).





    











