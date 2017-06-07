function C = matmul(A, B)

% function C = matmul(A, B)
%
% Vermenigvuldig matrices A en B
% gebruikt lussen ipv de ingebouwde operaties

[m,k1] = size(A);
[k2,n] = size(B);
if k1 ~= k2
  error('size(arg1,2) ~= size(arg2,1)');
end

C = zeros(m,n);
for i = 1:m
  for j = 1:n
    for k = 1:k1
      C(i,j) = C(i,j) + A(i,k)*B(k,j);
    end
  end
end
