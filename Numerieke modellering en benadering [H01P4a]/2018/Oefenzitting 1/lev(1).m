function [ L ] = lev( a,b,i,j )

if min(i,j) == 0
    L = max(i,j);
else
    s = lev(a,b,i-1,j) + 1;
    t = lev(a,b,i,j-1) + 1;
    u = lev(a,b,i-1,j-1) + (1 - (a(i) == b(j)));
    L = min([s,t,u]);
end

end

