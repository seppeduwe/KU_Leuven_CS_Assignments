[x,y] = click;
N = size(x,2);

% parametrisatie
t = linspace(0,1,N)';

% gewichten
w = ones(size(x'));

% graad benadering
n = 6;

% kleinste-kwadratenbenaderingen
c1 = kkb1(t,x',w,n);
c2 = kkb1(t,y',w,n);

% controle
figure;
t = linspace(0,1,10*N);
plot(polyval(c1(end:-1:1),t),polyval(c2(end:-1:1),t));
axis([0 1 0 1]);
