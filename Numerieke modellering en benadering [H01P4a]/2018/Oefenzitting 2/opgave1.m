% KKB - DEEL 2 - OEFENING 1
clc; clear all; close all;
% f(x) = int_{1}^{x} exp(-t)/t dt
% x > 0
x = 0.05;
%% (a)
fa = expint(1)-expint(x)
%% (b)
% N abcissen --> kkb van graad N-1 is de interpolerende veelterm en residu
% is nul in alle abcissen, een hogere graad is zinloos.
%% (c)
% functie kkb is gegeven
x = linspace(1/2,3/2,1000)';
f = expint(1) - expint(x);
w = ones(size(x));
res = []; r = [];
n = 1:20;
for k=n,
    c = kkb(x,f,w,k);
    r(:,k) = polyval(c(end:-1:1),x);
    res(k) = norm(f-r(:,k));
end
figure;
semilogy(n,res);
%axis([0,max(n),1e-20,1e-3])
figure;
plot(x,f); hold on;
plot(x,r);
%%
format long
I = integraal(x,f)

%% (d)
x = linspace(1/2,3/2,1000)';
f = expint(1) - expint(x);
df = exp(-x)./x;
[d, err] = afgeleide(x,f,df);
figure;
subplot(121);
plot(x,err);
subplot(122);
semilogy(d);
