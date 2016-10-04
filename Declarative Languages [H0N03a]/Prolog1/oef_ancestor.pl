% X is an ancestor of Y if X is the father of Y or 
%   if X is an ancestor of Z and Z is the father of Y.
ancestor(X,Y) :-
	father(X,Y).
ancestor(X,Y) :-
	father(Z,Y), % This has to come first!
	ancestor(X,Z).

father(anton,bart).
father(anton,daan).
father(anton,elisa).
father(fabian,anton).

% show_error:
% Toont wat er mis is met
% 		ancestor(X,Y) :-
%  		ancestor(X,Z),
%  		father(Z,Y).
% 		ancestor(X,Y) :-
%  		father(X,Y).

show_error :-
	write('Program:'), nl,
	write('   (1) ancestor(X,Y) :-'), nl,
	write('           ancestor(X,Y),'), nl,
	write('           father(X,Y).'), nl,
	write('   (2) ancestor(X,Y),'), nl,
	write('           father(X,Y).'), nl,
	write('Query of the program with X=anton and Y=bart:'), nl,	nl,
	show_error(anton,bart).

show_error(X,Y) :-
	show_error(X,Y,5).

show_error(_,_,0) :-
	write(' etc...\n').
show_error(X,Y,N) :-
	show_error_regel1(X,Y),
	N1 is N-1,
	show_error(X,Y,N1).

show_error_regel1(X,Y) :-
	write('Test rule (1) for "ancestor/2":'), nl,
	write('   ancestor('), write(X), write(','), write(Y), write(') :-'), nl,
	write('       ancestor('), write(X), write(','), write(Y), write('),'), nl,
	write('       father('), write(X), write(','), write(Y), write(').'), nl,
	write('Test first argument: "ancestor('),
	write(X), write(','), write(Y),
	write(')"'), nl, nl.
