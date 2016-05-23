function m=genmatrix1(dmat)
echo off
% genereert een generieke random matrix
a = rand(dmat);
x = randi(100,dmat,1)-1;
m = [a a*x];