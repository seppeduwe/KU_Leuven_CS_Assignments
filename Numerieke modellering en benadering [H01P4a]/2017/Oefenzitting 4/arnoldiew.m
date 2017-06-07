function [H, Q, maxd, mind] = arnoldiew(A, b, N)

% function [H, Q, maxd, mind] = arnoldiew(A, b, N)
%
% maximum en minimum eigenwaarden met Arnoldi iteratie
% maxd(:) absolute waarde van verschil tussen 
%         grootste eigenwaarde en grootste Ritz waarde
% mind(:) absolute waarde van verschil tussen 
%         kleinste eigenwaarde en kleinste Ritz waarde

ewA = eig(full(A));
maxewA = max(ewA);
minewA = min(ewA);

Q(:,1) = b/norm(b);
for n=1:N
  v = A*Q(:,n);
  for j = 1:n
    H(j,n) = Q(:,j)'*v;
    v = v - H(j,n)*Q(:,j);
  end
  H(n+1,n) = norm(v);
  if H(n+1,n) <= 0, break; end
  ewH = eig(full(H(1:n,1:n)));
  maxewH = max(ewH);
  minewH = min(ewH);
  maxd(n) = abs(maxewA - maxewH);
  mind(n) = abs(minewA - minewH);
  Q(:,n+1) = v/H(n+1,n);
end

