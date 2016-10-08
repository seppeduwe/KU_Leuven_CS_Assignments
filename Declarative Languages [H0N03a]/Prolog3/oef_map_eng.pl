inc(X,Y) :- Y is X + 1.

% map/3
map(_, [], []).
map(P, [X|Xs], [Y|Ys]) :-
    Call =.. [P,X,Y],
    call(Call),
    map(P,Xs,Ys).
