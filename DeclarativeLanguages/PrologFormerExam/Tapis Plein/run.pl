:- use_module(library(lists)).

writeall([],_,_).
writeall([X|R],Prefix,Postfix) :-
    write(Prefix),
    write(X),
    writeln(Postfix),
    writeall(R,Prefix,Postfix).

loadfiles :- 
    [prolog],
    [tapispleinefacts].


test_opdracht_1 :-
    findall(P,wall_in_part(part(0,0,5,4),P),L),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 1.1:'),
    writeall(L, '   ',''),
    findall(P,wall_in_part(part(2,0,5,3),P),L2),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 1.2:'),
    writeall(L2, '   ',''),
    findall(P,wall_in_part(part(3,0,5,2),P),L3),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 1.3:'),
    writeall(L3, '   ','').

% ALLE OPLOSSINGEN VOOR OPDRACHT 1.1:
%    wall(v,2,3,4)
%    wall(v,3,0,1)
%    wall(v,4,0,1)
%    wall(h,2,3,5)
% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 1.2:
%    wall(v,3,0,1)
%    wall(v,4,0,1)
%    wall(h,2,3,5)
% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 1.3:
%    wall(v,4,0,1)


test_opdracht_2 :-
    findall(P,color_in_part(part(0,0,5,4),P),L),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 2.1:'),
    writeall(L, '   ',''),
    findall(P,color_in_part(part(1,0,4,2),P),L2),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 2.2:'),
    writeall(L2, '   ',''),
    findall(P,color_in_part(part(3,0,5,2),P),L3),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 2.3:'),
    writeall(L3, '   ','').

% ALLE OPLOSSINGEN VOOR OPDRACHT 2.1:
%    green
%    red
%    red
%    blue
% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 2.2:
%    green
%    red
% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 2.3:
%    red
%    blue


test_opdracht_3 :-
    findall(yes,single_color_present(part(0,0,5,4)),L),
    writeln('\nOPLOSSING VOOR OPDRACHT 3.1:'),
    writeall(L, '   ',''),
    findall(yes,single_color_present(part(0,0,2,2)),L2),
    writeln('\nOPLOSSING VOOR OPDRACHT 3.2:'),
    writeall(L2, '   ',''),
    findall(yes,single_color_present(part(2,2,5,4)),L3),
    writeln('\nOPLOSSING VOOR OPDRACHT 3.3:'),
    writeall(L3, '   ',''),
    findall(yes,part_has_wall(part(0,0,5,4)),L4),
    writeln('\nOPLOSSING VOOR OPDRACHT 3.4:'),
    writeall(L4, '   ',''),
    findall(yes,part_has_wall(part(2,2,5,4)),L5),
    writeln('\nOPLOSSING VOOR OPDRACHT 3.5:'),
    writeall(L5, '   ','').

% OPLOSSING VOOR OPDRACHT 3.1:
% 
% OPLOSSING VOOR OPDRACHT 3.2:
%    yes
% 
% OPLOSSING VOOR OPDRACHT 3.3:
%    yes
% 
% OPLOSSING VOOR OPDRACHT 3.4:
%    yes
% 
% OPLOSSING VOOR OPDRACHT 3.5:
% 



test_opdracht_4 :-
    findall((C-P),cut_plan(C,P),L),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 4:'),
    writeall(L, '   ','').

% Desired output:
% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 3:
%    [cut(v,4,0,4),cut(h,2,0,4),cut(v,3,0,2),cut(v,2,2,4),cut(h,2,4,5)]-[part(0,0,3,2),part(0,2,2,4),part(2,2,4,4),part(3,0,4,2),part(4,0,5,2),part(4,2,5,4)]
%    [cut(h,2,0,5),cut(v,3,0,2),cut(v,4,0,2),cut(v,2,2,4)]-[part(0,0,3,2),part(0,2,2,4),part(2,2,5,4),part(3,0,4,2),part(4,0,5,2)]
%    [cut(h,2,0,5),cut(v,4,0,2),cut(v,3,0,2),cut(v,2,2,4)]-[part(0,0,3,2),part(0,2,2,4),part(2,2,5,4),part(3,0,4,2),part(4,0,5,2)]

:- loadfiles.
:- test_opdracht_1.
:- test_opdracht_2.
:- test_opdracht_3.
:- test_opdracht_4.
:- halt.