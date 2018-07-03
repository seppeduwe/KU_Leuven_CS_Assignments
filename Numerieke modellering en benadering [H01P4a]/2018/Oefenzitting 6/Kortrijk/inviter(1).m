function [lambda, v] = inviter(A, mu, v, n)
%Inverse iteratie.
% [lambda, v] = inviter(A, mu, v, n) berekent de rayleigh quotient waarden
% lambda, i.e., een schatting voor de eigenwaarden, en de bijbehorende
% eigenvector v voor een matrix A met behulp van inverse iteratie. De waarde
% mu geeft de initiële shift, v de initiële startvector en n het aantal uit
% te voeren iteraties.

dim = size(A,1);
lambda = ones(n,1);
v = v / norm(v);
for j=1:n,
  v = (A - mu * speye(dim)) \ v;
  v = v / norm(v);
  lambda(j) = v'*A*v ;
end
