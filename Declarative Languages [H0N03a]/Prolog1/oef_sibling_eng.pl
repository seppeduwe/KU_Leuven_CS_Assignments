% sibling/2: Sibling without filter
sibling(X,Y) :-
  father(V,X),
  father(V,Y),
  mother(M,X),
  mother(M,Y).

% sibling2/2: Sibling with filter included
sibling2(X,Y) :-
  father(V,X),
  father(V,Y),
  mother(M,X),
  mother(M,Y),
  X \== Y.

father(anton,bart).
father(anton,daan).
father(anton,elisa).
father(fabian,anton).

mother(celine,bart).
mother(celine,daan).
mother(celine,gerda).
mother(gerda,hendrik).
