D = diag([0:6]); 
V = orth(randn(7)) * diag(7:-1:1) * orth(randn(7)); 
Vinv = V\eye(7);
A = V*D*Vinv; 
d = [];
norm(V)

for k = 1: 10
    deltaA = orth(randn(7)) * diag(fliplr(eps/2*linspace(1, 10^k, 7))) * orth(randn(7)); 
    Atilde = A +deltaA; 
    norm(deltaA)
    e = eig(Atilde); 
    d = [d abs(max(e) - 6)+eps]; 
end

figure
semilogy(d, 'linewidth', 2) 
hold on; 
semilogy(1:10, 7*10.^[1:10]*eps/2, 'linewidth', 2)
legend('|\Delta \lambda|','Bauer-Fike')
xlabel('k')

%% Tweede deelvraag

D = diag(10.^[0:6]); 
V = orth(randn(7)) * diag(7:-1:1) * orth(randn(7)); 
Vinv = V\eye(7);
A = V*D*Vinv; 
d = [];
norm(V)

for k = 1: 10
    deltaA = orth(randn(7)) * diag(fliplr(eps/2*linspace(1, 10^k, 7))) * orth(randn(7)); 
    Atilde = A +deltaA; 
    norm(deltaA)
    e = eig(Atilde); 
    d = [d abs(max(e) - 10^6)+eps]; 
end

figure
semilogy(d, 'linewidth', 2) 
hold on; 
semilogy(1:10, 7*10.^[1:10]*eps/2, 'linewidth', 2)
legend('|\Delta \lambda|','Bauer-Fike')
xlabel('k')

%% Symmetrische matrix

D = diag([0:6]); 
V = orth(randn(7));
A = V*D*V'; 
d = [];
norm(V)

for k = 1: 10
    deltaA = orth(randn(7)) * diag(fliplr(eps/2*linspace(1, 10^k, 7))) * orth(randn(7)); 
    Atilde = A +deltaA; 
    norm(deltaA)
    e = eig(Atilde); 
    d = [d abs(max(e) - 6)+eps]; 
end

figure
semilogy(d, 'linewidth', 2) 
hold on; 
semilogy(1:10, 10.^[1:10]*eps/2, 'linewidth', 2)
legend('|\Delta \lambda|','Bauer-Fike')
xlabel('k')