function [ y ] = naturalspline( x,f,t )

% Returns the natural interpolating cubic spline through the points [x,f]
% evaluated in the points t. 

n = length(x) - 1;

% Coefficientenmatrix
deltax = diff(x);
C = diag(2*(deltax(1:n-1)+deltax(2:n))) + diag(deltax(2:n-1),1) + diag(deltax(2:n-1),-1);

% Rechterlid
deltaf = diff(f);
b = 6*(deltaf(2:n)./deltax(2:n) - deltaf(1:n-1)./deltax(1:n-1));
dds = C \ b;
dds = [0;dds;0];

% Evalueren
N = length(t); 
y = zeros(N,1);
j = 1;
for i = 1:N 
    if t(i) > x(end) 
        j = n;
    elseif t(i) > x(j+1) 
        j = j+1;
    end
    s = t(i);
    T1 = (f(j+1)*(s-x(j)) + f(j)*(x(j+1)-s))/deltax(j);
    T2 = 1/6*( (s-x(j))^3/deltax(j) - deltax(j)*(s-x(j)))*dds(j+1);
    T3 = -1/6*( (s-x(j+1))^3/deltax(j) + deltax(j)*(x(j+1)-s))*dds(j);
    y(i) = T1+T2+T3;
end
end

