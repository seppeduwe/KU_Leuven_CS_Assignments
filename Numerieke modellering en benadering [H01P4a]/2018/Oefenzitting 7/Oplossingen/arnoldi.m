function [H, Q] = arnoldi(A, b, N)

% function [H, Q] = arnoldi(A, b, N)
%

Q(:,1) = b/norm(b);
for n=1:N
  v = A*Q(:,n);
  for j = 1:n
    H(j,n) = Q(:,j)'*v;
    v = v - H(j,n)*Q(:,j);
  end
  H(n+1,n) = norm(v);
  if H(n+1,n) <= 0, break; end
  Q(:,n+1) = v/H(n+1,n);
end;

