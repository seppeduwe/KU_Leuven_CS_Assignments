% challenge7.m

n = 20000;
P = 224737;
p = primes(P)';
m = floor(log2(n));
e = ones(n, m+1);
i = 2.^(0:m);
B = [ e p e ];
d = [ -i 0 i ];
A = spdiags(B, d, n, n);
b = eye(n, 1);

D = spdiags(p, 0, n, n);
L = triu(A);
x = rand(n, 1);
maxit = 400;
tol = 1e-15;

method = 2;

if (method ~= 2) && (method ~= 3)
  x1 = x(1);
  R = [];
  X = [];
  for it = 1:maxit
    r = b - A*x;
    if method == 1
      % gauss-seidel
      x = x + L \ r;
    else
      % jacobi
      x = x  + r ./ p;
    end
    R(it) = norm(r);
    X(it) = abs(x1 - x(1));
    %if (R(it) < tol * R(1)) || (X(it) < tol), break; end
    if (X(it) < tol), break; end
    x1 = x(1);
  end
  semilogy(1:length(R), R, 1:length(X), X)
else
  if method == 3
    % cg symmetric gauss-seidel
    M1 = L + D;
    M2 = D \ (D + L');
  else
    % cg diag
    M1 = D;
    M2 = [];
  end
  [x, flag, relres, iter, resvec] = pcg(A, b, tol, maxit, M1, M2);
  x1 = x(1);
  semilogy(resvec);
end

x1
