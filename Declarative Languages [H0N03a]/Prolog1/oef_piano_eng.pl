% peano notation:
% 0 = zero, 1 = s(zero), 2 = s(s(zero)), ...

% min/3
%  min(X,Y,Z) : X - Y = Z
%  Call with at least 2 instantiated arguments.
%  Fails if argument 2 is larger than argument 1.
min(X,X,zero).
min(s(X),Y,s(Z)) :- min(X,Y,Z).

% greater_than/2
%  greater_than(X,Y) : succeeds iff X > Y.
%  First argument has to be instantiated.
greater_than(s(_),zero).
greater_than(s(X),s(Y)) :- greater_than(X,Y).

% maximum/3
%  maximum(X,Y,Z) : Z is the largest of {X,Y}
%  Call with at least 2 instantiated arguments.
maximum(X,X,X).
maximum(X,Y,X) :- greater_than(X,Y).
maximum(X,Y,Y) :- greater_than(Y,X).

% plus/3 predefined in SWI-prolog with normal arithmetic.
% peano_plus/3
peano_plus(zero,X,X).
peano_plus(s(X),Y,s(Z)) :- peano_plus(X,Y,Z).

% min_via_plus/3
%  min/3 solved using peano_plus/3
min_via_plus(X,Y,Z) :- peano_plus(Z,Y,X).

% times/3
%  times(X,Y,Z) : X * Y = Z
times(zero,_,zero).
times(s(X),Y,Z) :-
	times(X,Y,Z1),
	peano_plus(Z1,Y,Z).

% div/4
%  div(X,Y,D,R) : X mod Y = R, X / Y = D
%  Call with at least 2 instantiated arguments.
div(X,Y,zero,X) :- greater_than(Y,X).
div(X,Y,s(D),R) :-
	min(X,Y,X1),
	div(X1,Y,D,R).
