function y = applyQ(L,b)
% input:
% L  - m x n benedendriehoeksmatrix, m >= n
% b  - m x 1 vector
%
% output:
% y  - m x 1 vector Q'*b
[m,n] = size(L);
y = b;
for i = 1:n
    v = L(i:m,i);
    y(i:m) = y(i:m) - 2 * v * (transpose(v) * y(i:m));
end