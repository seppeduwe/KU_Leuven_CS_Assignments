function Q = formQ(L)
% input:
% L  - m x n benedendriehoeksmatrix, m >= n
%
% output:
% Q  - m x n orthogonale matrix

[m,n] = size(L);
Q = eye(m,n);
for j=1:n
  for i = 1:n
      v = L(i:m,i);
      Q(i:m,j) = Q(i:m,j) - 2 * v * (transpose(v) * Q(i:m,j));
  end
end