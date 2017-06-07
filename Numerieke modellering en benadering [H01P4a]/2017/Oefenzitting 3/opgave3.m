n = 10;
A = full(spdiags(ones(n,2), [0, 1], n, n));

epsilon = 1e-5
A(n,1) = epsilon;
ew = eig(A);
plot(ew,'+')

epspown = epsilon^(1/n)
errew = abs(1 - ew)
