function [A, b, x, kapA, kapbx, eta, costheta] = opgave4(sigma, c, U, V)

m = size(U,1);
n = size(V,1);
S = [diag(sigma); zeros(m-n,n)]; 
A = U * S * V';
b = U * c;
z = c(1:n) ./ sigma;
x = V * z;

kapA = cond(A);
eta = norm(A) * norm(x) / norm(A*x);
costheta = norm(A*x) / norm(b);
kapbx = kapA / (eta*costheta);
