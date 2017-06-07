L1 = diag(1:4);
P1 = orth(rand(4));
A1 = P1*L1*P1';

A = A1;
mu = 2 + 1e-5;

As = A-mu*eye(size(A));
condAs = cond(As)

b = rand(4, 1);
x = b/norm(b);
y = As\x;
x = y/norm(y)

eigvec = P1(:,2);
errx = min(norm(x-eigvec), norm(x+eigvec))

Ap = As + 1e-8 * (2*rand(4) - 1);

xp = b/norm(b);
yp = Ap\xp;
xp = yp/norm(yp);
erryp = norm(y-yp)
errxp = norm(x-xp)
