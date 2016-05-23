function gg=gauss2(a)
echo off
% gausseliminatie met optimale pivotering
% op de matrix [a b 1:n]
g1=a;
[n,m]=size(g1);
g=[g1';1:n]';
for i=1:n,
    j=i;
    s=abs(g(i,i));
    for l=i+1:n,
        if abs(g(l,i))>s,
           s=abs(g(l,i));
           j=l;
        end;
    end;
    if s==0,break,end
    if j~=i,
       hulp=g(i,:);
       g(i,:)=g(j,:);
       g(j,:)=hulp;
    end;
    for j=i+1:n,
        g(j,i)=g(j,i)/g(i,i);
        for l=i+1:m,
            g(j,l)=g(j,l)-g(j,i)*g(i,l);
        end;
    end;
end;
gg=g;
