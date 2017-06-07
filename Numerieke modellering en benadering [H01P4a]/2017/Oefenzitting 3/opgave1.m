L1 = diag(1:4);
P1 = orth(rand(4));
A1 = P1*L1*P1';

L2 = L1;
P2 = orth(rand(4))*diag([100 10 10 1])*orth(rand(4));
A2 = (P2*L2)/P2;

L3 = L1; L3(4,4) = 3; L3(3,4) = 1;
P3 = P2;
A3 = (P3*L3)/P3;

disp('Eigenwaarden van A1, A2, A3 berekend met Matlab eig');
ew1 = eig(A1)'
ew2 = eig(A2)'
ew3 = eig(A3)'

disp('druk op een toets om verder te gaan'); pause

disp('Bereken eigenwaarden voor A1, A2 en A3 via QR');
[e1, res1] = nlaqr(A1);
[e2, res2] = nlaqr(A2);
[e3, res3] = nlaqr(A3);
disp('Gevonden eigenwaarden voor A1, A2 en A3');
e1
e2
e3

figure(1)
n1 = length(res1); n2 = length(res2); n3 = length(res3);
semilogy(1:n1, res1, '+-', 1:n2, res2, 'x-', 1:n3, res3, '.-')
xlabel('iteraties');
ylabel('Residus');
legend('A1','A2','A3');

figure(2)
loglog(res1(1:n1-1), res1(2:n1), '+-', ...
      res2(1:n2-1), res2(2:n2), 'x-', ...
      res3(1:n3-1), res3(2:n3), '.-');
hold on
t = [1e-5, 1];
plot(t, t.^3, '--', t, t.^2, '--', t, t, '--');
hold off
legend('A1','A2','A3');
