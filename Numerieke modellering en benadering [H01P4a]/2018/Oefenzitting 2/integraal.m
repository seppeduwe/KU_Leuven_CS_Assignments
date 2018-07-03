function I = integraal(x,f)

w = ones(size(x));
I=zeros(15,1);
for n = 1:15,
    c = kkb(x,f,w,n);
    c = [0;c./(1:n+1)']; % dit maakt het de integraal
    c = polyval(c(end:-1:1),[x(end),x(1)]);
    I(n) = c(1)-c(2);
end;

