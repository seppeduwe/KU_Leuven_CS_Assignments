function [x,mu] = invmachten(A,x0,sigma,n)
% Methode van de inverse machten met verschuiving sigma toegepast op de
% matrix A met startwaarde x0, n iteratiestappen
%
% OUTPUT: de benaderde eigenvector x en de opeenvolgende benaderingen mu
% van de bijhorende eigenwaarde.

x0 = x0/norm(x0);
for i=1:n,
   x1 = (sigma*eye(size(A,1))-A)\x0;
   mu(i) = 1/norm(x1);
   if x0'*x1<0,
      mu(i) = -mu(i);
   end
   x0 = x1*mu(i);
end
% mu bevat opeenvolgende benaderingen voor (sigma-lambda_1(A))
x = x0;
mu = (sigma-mu)';
