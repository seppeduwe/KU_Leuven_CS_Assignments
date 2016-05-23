function gg=gauss1(a)
echo off
% Gausseliminatie met pivotering op de matrix
% g=[a b 1:n]
g1=a;
[n,m]=size(g1);
g=[g1';1:n]';
for i=1:n,
    j=i;
    while (g(j,i)==0)&(j<n),
          j=j+1;
    end;
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
