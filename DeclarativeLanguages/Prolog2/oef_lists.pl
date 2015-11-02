% The implementation of listlength2/2 using an accumulator.
listlength2(List,Length) :-
    listlength2(List,0,Length).
listlength2([],L,L).
listlength2([_|Rest],Accumulator,Length) :-
    NewAccumulator is Accumulator + 1,    
    listlength2(Rest,NewAccumulator,Length).

% The implementation of last/2.
last([X|[]],X).
last([_|Xs],E) :-
    last(Xs,E).

% The implementation of next_to/3.
next_to([A,B|_],A,B).
next_to([_|L],A,B) :-
    next_to(L,A,B).
	
% The implementation of vector_sum/3. This predicate only works if the first two
% lists have been instantiated and are equally long.
vector_sum([],[],[]).
vector_sum([A|As],[B|Bs],[S|Ss]) :-
    S is A + B,
    vector_sum(As,Bs,Ss).

% The implementation of look_up/3. The first argument is a list of 
% pair(Key,Value) terms, the second argument a key whose corresponding value
% needs to be looked up, and the third argument is the requested value.
look_up([pair(Key,Value)|_],Key,Value).
look_up([_|Xs],Key,Value) :-
	look_up(Xs,Key,Value).

