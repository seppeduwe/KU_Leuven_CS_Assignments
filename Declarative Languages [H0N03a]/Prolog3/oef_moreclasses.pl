teaches(berbers,bs).
teaches(berbers,iw).
teaches(demoen,ab).
teaches(demoen,cc).
teaches(holvoet,bvp).
teaches(moens,bvp).
teaches(danny,ai).
teaches(maurice,ai).
teaches(dedecker,socs).

takes(tom,bs).
takes(tom,bvp).
takes(tom,socs).
takes(maarten,socs).
takes(maarten,bs).
takes(pieter,bvp).

takes_same_course(X,Y) :-
	findall(paar(X,Z),(takes(X,Y), takes(Z,Y), X \= Z),L),
	list_to_set(L,S),
	member(paar(X,Y),S).

teach_same_course(X,Y) :-
	findall(paar(X,Z),(teaches(X,Y), teaches(Z,Y), X \= Z),L),
	list_to_set(L,S),
	member(paar(X,Y),S).

teaches_multiple_courses(X) :-
	findall(X,(teaches(X,Y),teaches(X,Z), Y \= Z), L),
	list_to_set(L,S),
	member(X,S).
