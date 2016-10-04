% The implementation of eval/3.
eval(int(X),_,X).
eval(var(X),List,Value) :-
    look_up(List,X,Value).
eval(plus(X,Y),List,Value) :-
    eval(X,List,WX),
    eval(Y,List,WY),
    Value is WX + WY.
eval(times(X,Y),List,Value) :-
    eval(X,List,WX),
    eval(Y,List,WY),
    Value is WX * WY.
eval(pow(X,Y),List,Value) :-
    eval(X,List,WX),
    eval(Y,List,WY),
    Value is WX ** WY.
eval(min(X),List,Value) :-
	eval(X,List,WX),
	Value is -WX.

% The implementation of look_up/3. The first argument is a list of 
% pair(Key,Value) terms, the second argument a key whose corresponding value
% needs to be looked up, and the third argument is the requested value.
look_up([pair(Key,Value)|_],Key,Value).
look_up([_|Xs],Key,Value) :-
	look_up(Xs,Key,Value).



% eval(min(int(3)),[],Value).

% eval(plus(int(2),var(x)),[pair(x,3)],Value).

% eval(plus(pow(var(x),var(y)),min(plus(times(int(3),var(z)),min(var(y))))), [pair(x,2),pair(y,3),pair(z,5)],Value).


