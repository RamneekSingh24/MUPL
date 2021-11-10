adj(1,3).
adj(2,3).
adj(3,4).




write_sol([]).

write_sol([ X|Rest]) :-
    format('(~w)\n', [X]),
    length(X, L),
    % format('hi  len: ~w\n', [L]),  
    write_sol(Rest).

print :-
    findall(1, adj(_, _), X),
    length(X, L),
    format('len: ~w\n', [L]),    
    write_sol(X).