function y=asubst(gg)
echo off
% voert achterwaartse substitutie uit op matrix g, 1 rechterlid
[n,m]=size(gg);

g=gg(:,1:n+1);

[p,q]=size(g);
t=p;
while (t>0),
      x(t)=g(t,q);
      for i=t+1:p,
          x(t)=x(t)-g(t,i)*x(i);
      end;
      x(t)=x(t)/g(t,t);
      t=t-1;
end;
y=x';
