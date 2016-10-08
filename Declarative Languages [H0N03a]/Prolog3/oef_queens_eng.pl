/* ?- queens(8,L). */

%queens(N,Ys)
queens(N,Ys) :-
  generate(1,N,Xs),
  permutation(Xs,Ys),
  safe(Ys).

%generate(M,N,List)
generate(N,N,[N]).
generate(M,N,[M|List]) :-
  M < N, M1 is M + 1,
  generate(M1,N,List).

%permutate(List,Perm)
permutation([],[]).
permutation(List,[Elem|Perm]) :-
  select(Elem,List,Rest),
  permutation(Rest,Perm).

%safe(Ys)
safe([]).
safe([First|Queens]) :-
  noattack(First,Queens,1),
  safe(Queens).

%noattack(Y,Ys)
noattack(_,[],_).
noattack(Queen,[First|Queens],N) :-
  % permutation ensures that no two queens are on same row
  abs(Queen - First) =\= N, % ensure no two queens are on same diagonal
  N1 is N + 1,
  noattack(Queen,Queens,N1).


/******************************************************************/
/* A version with freeze. This version chooses a row position for a queen in a certain column,
and immediately checks whether this queen can attack a queen to her left.
*/

%frqueens(N,Ys)
frqueens(N,Ys) :-
  generate(1,N,Xs,Ys),
  frsafe(Ys,[]), % freeze first one (no queens on left yet) 
  permutation(Xs,Ys).

%generate(M,N,List,Vars)
generate(N,N,[N],[_]).
generate(M,N,[M|List],[_|Vars]) :-
  M < N, M1 is M + 1,
  generate(M1,N,List,Vars).

%frsafe(Queens,QueensOnLeft)
frsafe([],_).
frsafe([Queen|RightQueens],LeftQueens) :-
  freeze(Queen,frs(RightQueens,Queen,LeftQueens)).

frs(RightQueens,Queen,LeftQueens):-
  frnoattacktoleft(Queen,LeftQueens,1),
  frsafe(RightQueens,[Queen|LeftQueens]). % freeze next one

frnoattacktoleft(_,[],_).
frnoattacktoleft(Queen,[First|LeftQueens],N):-
  % Queen =\= First, % ensured by using permutations!
  abs(Queen-First) =\= N,
  N1 is N + 1,
  frnoattacktoleft(Queen,LeftQueens,N1).

/*
?- time(queens(8,L)).
% 61,145 inferences, 0.010 CPU in 0.012 seconds (82% CPU, 6114500 Lips)
L = [1, 5, 8, 6, 3, 7, 2, 4].

?- time(frqueens(8,L)).
% 5,273 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
L = [1, 5, 8, 6, 3, 7, 2, 4].

?- time(queens(12,L)).
% 66,537,724 inferences, 9.730 CPU in 9.740 seconds (100% CPU, 6838409 Lips)
L = [1, 3, 5, 8, 10, 12, 6, 11, 2|...].

?- time(frqueens(12,L)).
% 20,091 inferences, 0.010 CPU in 0.004 seconds (226% CPU, 2009100 Lips)
L = [1, 3, 5, 8, 10, 12, 6, 11, 2|...].
*/

/******************************************************************/
/* It is possible to write a solution that does not use freeze, but tests as soon as possible.
This performs even better.*/

coqueens(N,Ys):-
	generate(1,N,Xs,_),
	refine(Xs,[],Ys).

refine([],_,[]).
refine(Kands,Links,[Y|Ys]) :-
	select(Y,Kands,Kands1),
	noattack(Y,Links,1),
	refine(Kands1,[Y|Links],Ys).

/*
?- time(coqueens(8,L)).
% 2,689 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
L = [1, 5, 8, 6, 3, 7, 2, 4].

?- time(coqueens(12,L)).
% 11,981 inferences, 0.000 CPU in 0.003 seconds (0% CPU, Infinite Lips)
L = [1, 3, 5, 8, 10, 12, 6, 11, 2|...].
*/
