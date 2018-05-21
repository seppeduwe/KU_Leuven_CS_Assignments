%% Benaderen van een functie 
x = [-2:0.4:2]';
V = vander(x');
f = 1./(1+7*x.^2);
p = V\f;
t = linspace(x(1), x(end), 1000);
y = naturalspline(x,f,t); 
ypoly = polyval(p,t);

figure; 
plot(t,y, 'b', 'linewidth',2); 
hold on; 
plot(t,ypoly,'g','linewidth',2);
hold on;
plot(x,f,'r.', 'markersize',15);
legend('spline','veelterm','datapunten')


%% Krommen benaderen met interpolerende splines/veeltermen
[f,g] = click 
f = f'; g = g';
n = length(f) - 1; deg = n;
w = ones(length(f),1);
a = (0:n)'; 
t = linspace(a(1), a(end), 1000);

x = naturalspline(a,f,t); 
y = naturalspline(a,g,t); 

c1 = kkb1(a,f,w,deg);
c2 = kkb1(a,g,w,deg);

figure;
subplot(1,2,1)
plot(x,y, 'b', 'linewidth', 2);
hold on; 
plot(f,g,'r.', 'markersize',15);
axis([0 1 0 1]);
subplot(1,2,2)
plot(f,g,'r.', 'markersize',15);
hold on
t = linspace(0,n,10*n);
plot(polyval(c1(end:-1:1),t),polyval(c2(end:-1:1),t),'b','linewidth',2);
axis([0 1 0 1]);

%% Naam schrijven 
[f,g] = click 
f = f'; g = g';
n = length(f) - 1; 
a = (0:n)'; 
t = linspace(a(1), a(end), 1000);

x = naturalspline(a,f,t); 
y = naturalspline(a,g,t); 
figure;
plot(x,y, 'b', 'linewidth', 2);
hold on; 
plot(f,g,'r.', 'markersize',15);
axis([0 1 0 1]);
