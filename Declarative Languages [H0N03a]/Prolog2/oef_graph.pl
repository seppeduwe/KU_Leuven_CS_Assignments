% (a) Facts representing a graph.

node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(b,c).
edge(c,d).
edge(b,d).

% (b) The implementation of neighbor/2.

neighbor(X,Y) :- edge(X,Y).
neighbor(X,Y) :- edge(Y,X).

% (c) The implementation of path/2.

path(X,Y) :-
    neighbor(X,Y).
path(X,Y) :-
	neighbor(X,Z),
	path(Z,Y),
	X \== Y.

% Because of the cycle in the graph, the recursive call to path/2 yields an
% infinite branch in the execution tree.

% (d) Solution: store the path in a list and iteratively search for a neighbor
% of the most recently added node.
path2(X,Y) :-
	path2(X,Y,[X]).
path2(X,Y,L) :-
	neighbor(X,Y),
	\+ member(Y,L).
path2(X,Y,H) :-
	neighbor(X,Z),
	\+ member(Z,H),	
	path2(Z,Y,[Z|H]).

