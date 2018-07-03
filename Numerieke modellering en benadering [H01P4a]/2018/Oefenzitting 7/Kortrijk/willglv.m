function [A,V] = willglv(L)

% function [A,V] = willglv(L)
%
% pas een willekeurige gelijkvormigheidstransformatie toe op 
% een matrix met als diagonaalelementen L(:)

m = length(L);
V = rand(m);
A = (V*diag(L))/V;
