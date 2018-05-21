function [Q, R] = mgs(A)
% QR factorization with MGS

[m, n] = size(A);
Q = zeros(m,n);
R = zeros(n,n);

for i=1:n
    R(i,i) = norm(A(:,i));
    Q(:,i) = A(:,i)/R(i,i);
    R(i,i+1:n) = Q(:,i)'*A(:,i+1:n);
    A(:,i+1:n) = A(:,i+1:n) - Q(:,i)*R(i,i+1:n);
end
