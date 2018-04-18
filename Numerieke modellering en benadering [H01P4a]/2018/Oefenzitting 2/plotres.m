function plotres(x,r,w)
% plotres(x,r,w)

clf
hold on
plot(x,r,'r+-','linewidth',2);
plot([x(1) x(end)],[0 0],'k:','linewidth',2,'markersize',10);
a = axis;
a(3) =-max(abs(a(3:4)));
a(4) = -a(3);
axis(a);
plot(x,w/max(w)*a(4),'g--','linewidth',2)
hold off;