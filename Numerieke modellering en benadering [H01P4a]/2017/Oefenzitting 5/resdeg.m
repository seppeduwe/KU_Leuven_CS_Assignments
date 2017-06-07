function [q1,q2] = resdeg(x,f,w,n)
% q = resdeg(x,f,w,n)

q1 = []; q2 = [];
for i=1:n,
    c1 = kkb1(x,f,w,i);
    c2 = kkb2(x,f,w,i);
    y1 = polyval(c1(end:-1:1),x);
    y2 = polyval(c2(end:-1:1),x);
    q1(i) = max(abs(y1-f));
    q2(i) = max(abs(y2-f));
end

figure
hold on
semilogy(q1,'b');
semilogy(q2,'r');
legend('Overgedetermineerd stelsel','Normaalstelsel');
hold off

