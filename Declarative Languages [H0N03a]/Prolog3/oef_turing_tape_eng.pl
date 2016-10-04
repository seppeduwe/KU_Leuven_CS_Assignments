% The internal representation of a tape is tape(Left,Current,Right), where
% Left is the reversed list of symbols to the left of the current symbol,
% Right is the list with symbols to the right of the current symbol, and
% Current is the symbol currently located under the read head.
% It is possible to perform every operation in constant time.

% move/3: moves the read head in a certain direction.
move(left,tape([],Symbol,Rs),tape([],#,[Symbol|Rs])).
move(left,tape([L|Ls],Symbol,Rs),tape(Ls,L,[Symbol|Rs])).
move(right,tape(Ls,Symbol,[]),tape([Symbol|Ls],#,[])).
move(right,tape(Ls,Symbol,[R|Rs]),tape([Symbol|Ls],R,Rs)).
move(wait,Tape,Tape).

% read/2: Reads the symbol under the read head.
read(tape(_,Symbol,_),Symbol).

% write/3: Writes a symbol under the read head.
write(Symbol,tape(L,_,R),tape(L,Symbol,R)).
