function [lambda,v] = rayleigh(A,v,maxit)

% function [lambda,v] = rayleigh(A,v,maxit)
%
% Rayleigh quotient iteration
%
% invoer: 
% A   - matrix
% v   - startvector
% maxit - maximum aantal iteratiestappen
%
% uitvoer:
% lambda - berekende eigenwaarde
% v      - berekende eigenvector

[n,m] = size(A);
if n~=m,
  disp('A is geen vierkante matrix')
  return
end
if n<2
  disp('A moet minstens dimensie 2 hebben')
  return
end

if length(v)~=n
    disp('v moet evenveel rijen als A hebben')
    return
end

v = v / norm(v);
lambda = v'*A*v;
warning off all % deze regel wegdoen als je de waarschuwingen van Matlab wilt zien
for it = 1:maxit
    w = (A-lambda*eye(n))\v;
    v = w/norm(w);
    lambda = v'*A*v;
end
warning on all
    