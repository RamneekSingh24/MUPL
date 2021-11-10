:- dynamic(parent/2).
:- dynamic(father/2).
:- dynamic(mother/2).


parent(arti, babli).
parent(arti, bunty).
parent(babli, chitra).
parent(babli, chintan).
parent(bunty, divya).
parent(bunty, divesh).
male(bunty).
male(chintan).
male(divesh).


childless(P) :- 
    findall(1, parent(P, _), []).

% childless(P) :- 
%     findall(1, parent(P, _), []);
%     findall(1, mother(P, _), []);
%     findall(1, father(P, _), []).


update_rules :-
    parent(X, Y),
    male(X),
    retract(parent(X, Y)),
    asserta(father(X, Y)).

update_rules :-
    parent(X, Y),
    retract(parent(X, Y)),
    asserta(mother(X, Y)).
    
    

tower_of_hanoi(X) :-
    solve(X, left, right, centre).

% solve(X, From, To, EmptyRod)
% solve([], _, _, _).

solve([X|[]], From, To, _) :-
    format('Move disk ~w from ~w to ~w.\n', [X, From, To]).

solve([H|T], From, To, EmptyRod) :- 
    solve(T, From, EmptyRod, To),
    format('Move disk ~w from ~w to ~w.\n', [H, From, To]),
    solve(T, EmptyRod, To, From).


check1([X1,Y1], X2, Y2) :-
    \+ (X1 = X2),
    \+ (Y1 = Y2),
    D1 is X1 - X2,
    D2 is Y1 - Y2,
    \+ (abs(D1) =:= abs(D2)).

checkAll([], _, _).

checkAll([H|T], X, Y) :- 
    check1(H, X, Y),
    checkAll(T, X, Y).

write_sol([]).

write_sol([ [ Row, Col] | Rest]) :-
    format('(~w, ~w)\n', [Row, Col]),
    write_sol(Rest).

track(0, _, N, CurrQueens) :-
    N > 0,
    write_sol(CurrQueens).

track(X, 0, N, CurrQueens) :-
   X > 0, 
   NextX is X - 1,
   track(NextX, N, N, CurrQueens).


track(X, Y, N, CurrQueens) :-
    X > 0,
    Y > 0,
    checkAll(CurrQueens, X, Y),
    track(X, 0, N, [[X,Y] | CurrQueens]).

track(X, Y, N, CurrQueens) :-
    X > 0,
    Y > 1,
    NextY is Y - 1, 
    track(X, NextY, N, CurrQueens).


nQueens(N) :-
    N > 0, 
    track(N, N, N, []).


magicSquare :- 
    between(0, 9, A),
    between(0, 9, B),
    between(0, 9, C),
    between(0, 9, D),
    between(0, 9, E),
    S = A + B + C,
    G = S - A - D,
    G =:= S - C - E,
    G >= 0,
    G =< 9,
    H = S - B - E,
    H >= 0,
    H =< 9,
    F = S - D - E,
    F >= 0,
    F =< 9,
    I = S - A - E,
    I =:= S - G - H,
    I =:= S - C - F,
    I = S - A - E,
    I >= 0,
    I =< 9,
    format('(A = ~d, B = ~d, C = ~d, D = ~d, E = ~d, F = ~d, G = ~d, H = ~d, I = ~d)\n',
    [A,B,C,D,E,F,G,H,I]).













    

