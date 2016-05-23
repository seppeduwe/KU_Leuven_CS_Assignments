function [x,sigma] = invmachten_adaptief(A,x0,sigma,n)
% Methode van de inverse machten met adaptieve verschuiving toegepast op de
% matrix A met startwaarde x0, n iteratiestappen en sigma de waarde van de
% eerste verschuiving.
%
% OUTPUT: de benaderde eigenvector x en de opeenvolgende benaderingen mu
% van de bijhorende eigenwaarde.

x0 = x0/norm(x0);
for i=1:n,
   x1 = (sigma(i)*eye(size(A,1))-A)\x0;
   mu = 1/norm(x1);
   if x0'*x1<0,
      mu = -mu;
   end
   x0 = x1*mu;
   sigma(i+1) = sigma(i) - mu;
end
% mu bevat opeenvolgende benaderingen voor lambda_1(A)
x = x0;
sigma = sigma';
