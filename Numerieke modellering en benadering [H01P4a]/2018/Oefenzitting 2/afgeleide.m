function [d, err] = afgeleide(x,f,df)

w = ones(size(x));
d=zeros(15,1); err = [];
for n = 1:15,
    c = kkb(x,f,w,n);
    c = [(1:n)'.*c(2:n+1)]; % dit maakt het de afgeleide
    err(:,n) = abs(df-polyval(c(end:-1:1),x))./abs(df);
    d(n) = max(err(:,n));
end;

