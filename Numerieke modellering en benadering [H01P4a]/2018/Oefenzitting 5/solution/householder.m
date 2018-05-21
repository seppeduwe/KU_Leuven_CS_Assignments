function [L, R] = householder(A)
% input:
% A  - m x n matrix, m >= n
%
% output:
% L  - m x n benedendriehoeksmatrix waarvan de kolommen de vectoren v van
% opeenvolgende Householder transformaties zijn
% R  - n x n bovendriehoeksmatrix

[m,n] = size(A);
R = A;
L = eye(m,n);
for i = 1:n
    x = R(i:m,i);
    v = sign(x(1))*norm(x)*eye(m-i+1,1) + x;
    v = v / norm(v);
    R(i:m,i:n) = R(i:m,i:n) - 2*v*(transpose(v) * R(i:m,i:n));
    R(i+1:m,i) = 0;
    L(i:m, i) = v;
end
R(n+1:m,:) = [];