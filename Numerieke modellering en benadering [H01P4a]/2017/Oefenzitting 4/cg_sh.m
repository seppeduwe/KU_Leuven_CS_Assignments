kappa = 10;
n = 100;
A = willorth(eigint(1, kappa, n));
xx = rand(n, 1);
b = A*xx;

max_it = n;
M = eye(size(A));
tol = 1e-10;
x = rand(n, 1);

[x1, error, iter, flag, errvec, xs] = cg(A, x, b, M, max_it, tol);

[x2, relres, f] = steilstehelling(A, b, x, max_it, tol);

semilogy(1:iter, errvec, 'b', 1:length(relres), relres, 'r')
legend('cg','steilste helling')

