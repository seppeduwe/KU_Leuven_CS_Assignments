%% translate/2
%%
%% ?- translate([def(a), use(a), use(b), use(c), def(c), def(b)], L).
%% L = [asgn(a, 1), use(1), use(3), use(2), asgn(c, 2), asgn(b, 3)]
%%
%% The input list is a sequence of def/1 and use/1 terms.
%% Note that e.g. use(b) occurs in the list before before def(b),
%% The symbols a,b,c,... are mapped to an integer number (e.g. representing
%% their absolute address in the symbol table).
%% The order of the def/1 terms in the input list determines the number for the
%% symbols, (e.g. their address in the symboltable).
%% In the translated list we replace def(sa) by asgn(sa,nb) and use(sa) by
%% use(nb) with nb the number associated with sa.
%%
%% For the input list [def(a), use(a), use(b), use(c), def(c), def(b)] ,
%% we will use number 1 for a, number 2 for c and number 3 for b.
%%
%% This illustrates the power of unification and free variables:
%% translation can be done with one pass (instead of the classical 2 passes).


translate(InL, OutL) :-
    map(InL, OutL, _, 1).


%% map/4
%% 1st arg. : input list
%% 2nd arg. : output list
%% 3rd arg. : open ended list: symboltable,
%%     a listelement associates a symbol with an absolute address.
%%     The absolute address will be instantiated when a def occurs in the
%%     input list.
%% 4th arg. : counter indicating the next available address


map([],[],_,_).
map([def(A) | InL], [asgn(A,N) | OutL], Table, N) :-
    member(asgn(A,N), Table),!,
    N1 is N + 1,
    map(InL,OutL,Table,N1).
map([use(A)|InL], [use(NB)|OutL], Table, N) :-
    member(asgn(A,NB), Table),
    map(InL,OutL,Table,N).
