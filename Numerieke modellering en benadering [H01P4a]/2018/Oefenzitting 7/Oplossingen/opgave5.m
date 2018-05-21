m = 100; 
n = 10;
L = eigint(4,5,m); 
L(1) = 8;
L(2) = 2;
[A,V] = willglv(L);
b = rand(m,1);

[H, Q, maxd, mind] = arnoldiew(A, b, n);

t = 1:n;
semilogy(t, maxd, '+-', t, mind,'x-');
legend('max eigw','min eigw')