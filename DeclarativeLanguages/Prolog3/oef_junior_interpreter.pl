interpret(Query) :-
	interpret(Query,[],_).

interpret(is(X,Y),Mem,Mem) :- !, 
	X is Y.
interpret('>'(X,Y),Mem,Mem) :- !, 
	X > Y.
interpret((G1,G2),Mem,MemOut) :- !, 
    interpret(G1,Mem,Mem1), 
    interpret(G2,Mem1,MemOut).
interpret(true,Mem,Mem) :- !.
interpret(Head,Mem,MemOut) :- 
	( member(Head,Mem) ->
		MemOut = Mem
	;
		clause(Head,Body), 
		interpret(Body,Mem,Mem1),
		MemOut = [Head|Mem1]
	).



%
% interpret2: The same as interpret, but it also memorizes builtins.
%
interpret2(Query) :-
	interpret2(Query,[],_).

interpret2(is(X,Y),Mem,MemOut) :- !, 
	( member(is(X,Y),Mem) ->
		MemOut = Mem
	;
		X is Y,
		MemOut = [is(X,Y)|Mem]
	).
interpret2('>'(X,Y),Mem,MemOut) :- !, 
	( member('>'(X,Y),Mem) ->
		MemOut = Mem
	;
		X > Y,
		MemOut = ['>'(X,Y)|Mem]
	).
interpret2((G1,G2),Mem,MemOut) :- !, 
    interpret2(G1,Mem,Mem1), 
    interpret2(G2,Mem1,MemOut).
interpret2(true,Mem,Mem) :- !.
interpret2(Head,Mem,MemOut) :- 
	( member(Head,Mem) ->
		MemOut = Mem
	;
		clause(Head,Body), 
		interpret2(Body,Mem,Mem1),
		MemOut = [Head|Mem1]
	).
