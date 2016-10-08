/*********************************************************************************************
  SOLUTION for exercise 'Holiday lights'

  tour/1 computes a Hamiltonian path beginning and ending in node 1 of a given graph, 
  making sure that all edges are used and that no two adjacent edges in the Hamiltonian 
  path have the same label.

  PREREQUISITE:
    - edges of graphs are given with highway/3 facts

**********************************************************************************************/

node(X) :- highway(X,_,_).
node(X) :- highway(_,X,_).

% allNodes/1: returns all the nodes in the graph sorted from low to high
allNodes(Nodes) :- 
  findall(Node,node(Node),UnsortedNodes),
  sort(UnsortedNodes,Nodes). % also removes doubles

% allRoads/1: returns all the roads (edges) in the graph
allRoads(Roads) :- 
  findall(highway(X,Y,T),highway(X,Y,T),UnsortedRoads),
  sort(UnsortedRoads,Roads).

% check/0: checks the first constraint and the second constraint
check :-
  allNodes(Nodes),
  check_even(Nodes).

% check_even/1: checks whether every node has an even degree and goes on to check second constraint
check_even([]).
check_even([Node|Rest]) :-
  findall(Neighbor,(highway(Neighbor,Node,_);highway(Node,Neighbor,_)),UNeighbors),
  sort(UNeighbors,Neighbors),
  length(Neighbors,Length),
  even(Length),
  ( Node == 1 -> % we only have to check the color constraint for nodes != 1
	true
  ;
    check_color_constraint(Node,Neighbors)
  ),
  check_even(Rest).

% even/1: checks whether a given number is even
even(I) :- 
  X is I mod 2,
  X == 0.

% check_color_constraint/2: given a node and its neighbors, checks whether, if there are X connections of a particular color, that there are at least X connections of other colors
check_color_constraint(Node,Neighbors) :-
  findall(Type,(highway(Node,_,Type);highway(_,Node,Type)),Ts1), % find colors of all adjacent edges to the node
  sort(Ts1,Types),
  checkTypes(Types,Node,Neighbors).

checkTypes([],_Node,_Neighbors).
checkTypes([Type|OtherTypes],Node,Neighbors):-
  numberOfConnections(Type,Node,Number),
  numberOfOtherConnections(Type,Node,NumberNot),
  Number =< NumberNot,
  checkTypes(OtherTypes,Node,Neighbors).

% numberOfConnections/3: counts the Number of edges of a particular Type adjacent to a Node
numberOfConnections(Type,Node,Number) :-
  findall(B,(highway(Node,B,Type); highway(B,Node,Type)),Bs1),
  sort(Bs1,Bs), % remove doubles
  length(Bs,Number).

% numberOfConnections/3: counts the Number of edges other than a particular Type adjacent to a Node
numberOfOtherConnections(Type,Node,Number) :-
  findall(B,(highway(Node,B,T1), T1 \== Type; highway(B,Node,T2),T2 \== Type),Bs1),
  sort(Bs1,Bs), % remove doubles
  length(Bs,Number).

% tour/1: returns the smallest tour
tour(T) :-
  check,
  findall(Tour,tourcandidate(Tour),L),
  sort(L,[T|_OtherTours]).

% tourcandidate/1: computes a possible tour
tourcandidate(Tour) :-
  allNodes([FirstNode|_]),
  allRoads(Roads),
  tour(FirstNode,FirstNode,Roads,[],TourRev),
  reverse(TourRev,Tour).

% tour(Begin,End,RoadsLeft,VisitedRoads,Solution)
tour(End,End,[],Visited,Visited). % stop when there are no edges left
tour(Node,End,RoadsLeft,Visited,Solution) :-
  ( 
    select(highway(Node,Neighbor,Type),RoadsLeft,RoadsLeft1)
  ; 
    select(highway(Neighbor,Node,Type),RoadsLeft,RoadsLeft1)
  ),
  compatible(Type,Visited),
  tour(Neighbor,End,RoadsLeft1,[Neighbor-Type|Visited],Solution).
	
% compatible/2: checks whether a road can be added to the tour by checking its type
compatible(_,[]).
compatible(Type,[ _-Type2|_]) :- Type \== Type2.
