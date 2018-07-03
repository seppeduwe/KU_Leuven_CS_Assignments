function [A, Q] = willorth(L)

% function [A, Q] = willorth(L)
%
% pas een willekeurige gelijkvormigheidstransformatie toe op 
% een matrix met als diagonaalelementen L(:)

m = length(L);
V = rand(m);
Q = orth(V);
A = Q*diag(L)*Q';
