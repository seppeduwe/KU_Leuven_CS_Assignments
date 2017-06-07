function [e,res] = nlaqr(a)

%function [e,res] = nlaqr(A)
%berekent met de QR-methode een eigenwaarde van de matrix A
%
%IN  : A : matrix
%UIT : e : de berekende eigenwaarde
%      res : de normen van de residu's voor iedere iteratiestap
%
%De gebruikte methode is de QR-methode met shift. Als shift wordt het
%(n,n)-element van A gebruikt.
%
%Oefeningen Numerieke Lineaire Algebra
%auteur : Serge Goossens 

[n,m] = size(a);
if n~=m,
  disp('A is geen vierkante matrix')
  return
end
if n<2
  disp('A moet minstens dimensie 2 hebben')
  return
end

a = hess(a);
res = [];

while abs(a(n,n-1))>1.e-13
   a
   res = [res abs(a(n,n-1))];
   disp(sprintf('residu = %.0e', abs(a(n,n-1))))
   disp('druk op een toets om verder te gaan')
   pause
   [q,r]=qr(a-a(n,n)*eye(n));
   a = r*q + a(n,n)*eye(n);
end
a
res = [res abs(a(n,n-1))];
disp(sprintf('residu = %.1e', abs(a(n,n-1))))
e = a(n,n);
