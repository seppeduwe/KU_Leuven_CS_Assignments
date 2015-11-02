% eval/2
eval(tru,tru).
eval(fal,fal).

eval(and(X,Y),tru) :-
	eval(X,tru),
	eval(Y,tru).
eval(and(X,Y),fal) :-
	(	eval(X,fal)
	;
		eval(Y,fal)
	).

eval(or(X,Y),tru) :-
	(	eval(X,tru)
	;
		eval(Y,tru)
	).
eval(or(X,Y),fal) :-
	eval(X,fal),
	eval(Y,fal).
	
eval(not(X),tru) :-
	eval(X,fal).
eval(not(X),fal) :-
	eval(X,tru).

% eval2/2
%  alternative version of eval/2
eval2(tru,tru).
eval2(fal,fal).
eval2(and(X,Y),Z) :-
	eval2(X,XE),
	eval2(Y,YE),
	and(XE,YE,Z).
eval2(or(X,Y),Z) :-
	eval2(X,XE),
	eval2(Y,YE),
	or(XE,YE,Z).
eval2(not(X),Y) :-
	eval2(X,XE),
	not(XE,Y).

% Truth tables for and, or, not
and(tru,tru,tru).
and(tru,fal,fal).
and(fal,tru,fal).
and(fal,fal,fal).

or(tru,tru,tru).
or(tru,fal,tru).
or(fal,tru,tru).
or(fal,fal,fal).

not(tru,fal).
not(fal,tru).

