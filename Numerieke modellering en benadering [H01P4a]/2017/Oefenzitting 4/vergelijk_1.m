m = 50;

%% eigenwaarden
L = eigint(9,11,m);
%L = eigint(4,6,m);
%L = eigint(1,400,m);
%L = [eigint(1,1.5,floor(m/2));eigint(399,400,ceil(m/2))];
%L = eigint(10,400,m);
%L = [eigint(10,15,floor(m/2));eigint(399,400,ceil(m/2))];

%% matrix
[A,Q] = willorth(L);
%[A,V] = willglv(L);

%% exacte oplossing
xexact = rand(m,1);

%% rechterlid
b = A*xexact;

%% startwaarde
x0 = rand(m,1);

%% maximum aantal iteraties
max_it = 100;

%% stopcriterium (norm residu reduceren met factor tol)
tol = 1e-12;

%% geen preconditionering, M is identiteitsmatrix
M = eye(size(A));
[x, error, iter, flag, errvec, xs] = cg(A, x0, b, M, max_it, tol);

fout = [];
foutA = [];
r = [];
for i = 1:length(errvec)
  xi = xs(:,i);
  e = xi-xexact;
  fout(i) = norm(e);
  foutA(i) = sqrt(e'*A*e); % heeft geen zin als A niet spd 
  r(i) = norm(b-A*xi) / norm(b);
end

iters=1:length(errvec);
semilogy(iters,fout,'x-',iters,foutA,'o-',iters, r,'s-')
legend({'fout','foutA','r'});
p = polyfit(iters, log10(foutA), 1);
rho = 10^p(1)


