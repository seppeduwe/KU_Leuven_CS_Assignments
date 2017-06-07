function [lambda, v] = inviter(A, mu, v, n)

% function [lambda, v] = inviter(A, mu, v, n);

dim = size(A,1);
lambda = ones(n,1);
v = v / norm(v);
for j=1:n,
  v = (A - mu * speye(dim)) \ v;
  v = v / norm(v);
  lambda(j) = v'*A*v ;
end
