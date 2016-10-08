:- use_module(library(lists)).

writeall([],_,_).
writeall([X|R],Prefix,Postfix) :-
    write(Prefix),
    write(X),
    writeln(Postfix),
    writeall(R,Prefix,Postfix).

loadfiles :-
    [prolog], % If needed, edit your name file here
    [cocktailsfacts].

test_assignment_1 :-
    findall(DO,drink_order(DO),LDO),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 1:'),
    writeall(LDO, '   ','').


% Desired output:
% ALLE OPLOSSINGEN VOOR OPDRACHT 1:
%    [order(bruno,[margarita,margarita]),order(sandra,[bloody_mary]),order(stephen,[bloody_mary])]
%    [order(bruno,[margarita,margarita]),order(sandra,[bloody_mary,bloody_mary]),order(stephen,[bloody_mary])]


test_assignment_2 :-
    findall(SL,shopping_lists([order(bruno,[margarita,margarita]),order(sandra,[bloody_mary]),order(stephen,[bloody_mary])],SL),SLL),
    writeln('\nALLE OPLOSSINGEN VOOR OPDRACHT 2 VOOR DE EERSTE DRINK ORDER UIT OPDRACHT 1:'),
    writeall(SLL, '   ',''),
    writeln('\n').

% 
% ALLE OPLOSSINGEN VOOR OPDRACHT 2 VOOR DE EERSTE DRINK ORDER UIT OPDRACHT 1:
%    [shopat(bruno,carrefour,[limes,tequila]),shopat(sandra,aldi,[tomatoes]),shopat(stephen,delhaize,[vodka])]
%    [shopat(bruno,carrefour,[limes,tequila]),shopat(sandra,delhaize,[vodka]),shopat(stephen,aldi,[tomatoes])]



:- loadfiles.
:- test_assignment_1.
:- test_assignment_2.


:- halt.