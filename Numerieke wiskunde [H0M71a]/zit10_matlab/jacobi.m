function fout=jacobi(a,b,x0,xe,n)
[ra,ca]=size(a);
x=x0;
r=a*x-b;
fout(1)=norm(x-xe)/norm(xe);
for k=2:n+1;
  for i=1:ra;
    x(i)=x(i)-r(i)/a(i,i);
  end;   
  fout(k)=norm(x-xe)/norm(xe);
  r=a*x-b;
end
fout = fout';