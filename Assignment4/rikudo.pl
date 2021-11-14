

:- dynamic(linked/4).
:- dynamic(placedAt/3).
:- dynamic(adj/4).

bound(7, 1).
bound(19, 1).
bound(37, 3).
bound(61, 5).
bound(91, 7).

inBounds(X, Y, N) :-
    bound(N, R),
    \+ (X = Y, X = 0),
    AX is abs(X),
    AY is abs(Y),
    ( (X*Y =< 0, max(AX, AY) =< R) ; (X * Y > 0, (AX + AY =< R) ) ).



addLinks([]).

addLinks([[X1, Y1, X2, Y2]| T]) :-
    asserta(linked(X1, Y1, X2, Y2)),
    asserta(linked(X2, Y2, X1, Y1)),
    addLinks(T).

fill([]).

fill([[X1, Y1, K]| T]) :-
    asserta(placedAt(X1, Y1, K)),
    fill(T).


solve(N, Links, Prefilled) :-
    NN is N + 1,
    addLinks(Links),
    fill(Prefilled),
    place(1, NN), abort.


place(1, N) :- placedAt(_, _, 1), place(2, N).
place(N, N) :- printSol(1, N), abort.


place(V, N) :-
    \+ V = 1,
    placedAt(X, Y, V),
    Xp1 is X + 1,
    Xm1 is X - 1,
    Yp1 is Y + 1,
    Ym1 is Y - 1,
    PV is V - 1,
    (   
        placedAt(Xp1, Y, PV);
        placedAt(X, Yp1, PV);
        placedAt(Xm1, Yp1, PV);
        placedAt(Xm1, Y, PV);
        placedAt(X, Ym1, PV);
        placedAt(Xp1, Ym1, PV)
    ),
    NV is V + 1,
    place(NV, N).


place(V, N) :-
    \+ V = 1,
    \+ placedAt(_, _, V), 
    PV is V - 1,
    placedAt(X, Y, PV),
    (
        (
            linked(X, Y, X1, Y1),
            \+ placedAt(X1, Y1, _),
            place(X1, Y1, V, N)
        );

        (
            ( (linked(X, Y, X1, Y1), placedAt(X1, Y1, _) ) ; \+ linked(X, Y, _, _) ),  % Linked and placed, or not linked.
            Xp1 is X + 1,
            Xm1 is X - 1,
            Yp1 is Y + 1,
            Ym1 is Y - 1,
            (
                place(Xp1, Y, V, N);
                place(X, Yp1, V, N);
                place(Xm1, Yp1, V, N);
                place(Xm1, Y, V, N);
                place(X, Ym1, V, N);
                place(Xp1, Ym1, V, N)
            )

        )
    ).
    
place(X,Y,V,N) :-
    inBounds(X, Y, N),
    \+ placedAt(X, Y, _),
    asserta(placedAt(X, Y, V)),
    NV is V + 1,
    (   
        (place(NV, N), retract(placedAt(X, Y, V)));
        (retract(placedAt(X, Y, V)), false)
    ).


printSol(N, N).
printSol(V, N) :-
    V < N,
    placedAt(X, Y, V),
    format('~d at (~d, ~d)\n', [V, X, Y]),
    NV is V + 1,
    printSol(NV, N).


% placeNext(N,N). % TODO print the result.

% placeNext(V, N) :-
%     placedAt(X, Y, V),
%     (
%         linked(X, Y, X1, Y1),
%         \+ placedAt(X1, Y1, _),
%         (
%             (
%                 place(X1, Y1, V+1),
%                 placeNext(V+1, N)
%             ) ;
%             (
%                 remove(X1, Y1, V+1).
                
%             )
             

%         )
%     )
%     placedAt(X, Y, V),
%     linked(X, Y, X1, Y1)
%     \+ placedAt(X1, Y1, _),
%     place(X1, Y1, V+1),
%     placeNext(V+1, N);

%     placedAt(X, Y, V),





    

    

    
    



