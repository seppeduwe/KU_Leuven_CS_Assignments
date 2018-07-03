% BGSIN(X)
clc; clear all; close all;
N = 100; % aantal punten
x = linspace(0,1,N)';
t = linspace(0,1,10*N);
f = asin(x);
%% enkele keuzes gewichtsfuncties
w1 = ones(size(x));
%w2 = x;
%w3 = abs(randn(size(x)));
%w4 = randn(size(x)); w4(w4 < 0) = 0; w4(w4 > 0 ) = 1;
%w5 = 1/sqrt(1-x.^2); w5 = w5';
w = w1;
%% kkb optie 1
n = 1:15
;
figure;
for k=n
    c = kkb(x,f,w,k);
    r = polyval(c(end:-1:1),t);
    norm(f-polyval(c(end:-1:1),x))
    plot(x,f,'b.','markersize',15); hold on;
    plot(t,r,'r','linewidth',2);
    title(k);
    pause;
    clf;
end
close all;
%% kkb optie 2
f2 = (pi/2 - f)./sqrt(1-x);
f2 = f2(1:N-1); x2 =  x(1:end-1); t2 = t(1:end-1); w2 = w(1:N-1);
for k=n
    c = kkb(x2,f2,w2,k);
    r = polyval(c(end:-1:1),t2);
    r = pi/2 - (sqrt(1-t2).*r);
    norm(f(1:N-1)-(pi/2 - sqrt(1-x2).*polyval(c(end:-1:1),x2)))
    plot(x,f,'b.','markersize',15); hold on;
    plot(t2,r,'r','linewidth',2)
    title(k);
    pause;
    clf;
end