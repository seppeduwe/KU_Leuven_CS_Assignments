x = linspace(1/2,3/2,1000)';
f = expint(1) - expint(x);
w = ones(size(x));
res = [];
n = 1:20;
for k=n,
    c = kkb(x,f,w,k);
    r = polyval(c(end:-1:1),x);
    res(k) = norm(f-r);
end
figure;
plot(n,res);
%axis([0,max(n),1e-20,1e-3])