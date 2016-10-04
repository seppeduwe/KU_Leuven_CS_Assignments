% The implementation of fib/2.
fib(1,0).
fib(2,1).
fib(N,F) :-
    N > 2,
    N1 is N - 1,
    fib(N1,F1),
    N2 is N - 2,
    fib(N2,F2),
    F is F1 + F2.

% A more efficient implementation of fib2/2.
fib2(1,0).
fib2(2,1).
fib2(N,F) :-
    N > 2,
    fib2(N,2,1,0,F).

% fib2(N,I,F2,F1,F): F is Fibonacci number N if F2 is Fibonacci number I 
% and F1 is Fibonacci number I-1.
fib2(N,I,F2,F1,F) :-
    In is I + 1,
    Fn is F2 + F1,
    ( 
        N = In
    ->
        F = Fn
    ;
        fib2(N,In,Fn,F2,F)
    ).

% Note that the more efficient version requires only one recursive call, while
% the original version requires two recursive calls.
