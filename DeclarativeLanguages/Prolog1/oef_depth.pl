
% depth/2
%  depth(B,D) : D is the depth of tree B
%  First argument has to be completely instantiated!
depth(nil,0).
depth(node(LB,_,RB),D) :-
	depth(LB,LD),
	depth(RB,RD),
	D1 is max(LD,RD), % built-in
	D is D1 + 1.