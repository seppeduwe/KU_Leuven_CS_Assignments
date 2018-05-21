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
maxit = 1000;
tol = 1e-15;
M = speye(size(A));
[x, error, iter, flag, errvec, xs] = cg(A, x, b, M, maxit, tol);
antw1 = x(1)
[x, error, iter, flag, errvec, xs] = cg(A, x, b, D, maxit, tol);
antw2 = x(1)
