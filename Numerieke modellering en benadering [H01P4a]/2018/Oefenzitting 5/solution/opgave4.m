% Opgave 4
%% b)

n = [10 100 1000];
K = [0 4 8]; % conditie 10^K(i)
relatieve_fout = zeros(length(n),length(K));
for i=1:length(n)
  for j=1:length(K)
    D = diag(logspace(0,K(j),n(i)));
    A = orth(randn(size(D))) * D * orth(randn(size(D)));
    x_orig = randn(n(i),1);
    b = A*x_orig;
    [L,R] = householder(A);
    y = applyQ(L,b);
    x = R\y;
    relatieve_fout(i,j) = norm(x - x_orig) / norm(x);
  end
end

figure
loglog(n,relatieve_fout,'*-','LineWidth',2)
ylabel('relatieve fout')
xlabel('n')

figure
semilogy(K,relatieve_fout','*-','LineWidth',2)
ylabel('relatieve fout')
xlabel('log(K)')
%% c)

K = 10;
m = 100;
n = 80;

D = diag(logspace(0,K,n));
A = orth(randn(m,n)) * D * orth(randn(n,n));
[L,R] = householder(A);
[Qhh] = formQ(L);
[Qmgs,~] = mgs(A);

norm(eye(n)-Qhh'*Qhh)
norm(eye(n)-Qmgs'*Qmgs)