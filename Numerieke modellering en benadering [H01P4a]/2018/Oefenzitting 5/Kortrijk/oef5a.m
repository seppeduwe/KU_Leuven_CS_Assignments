%% Opgave 5a
figure(1)
A = randn(100,50);

[d,~,ds] = fastgivensQR1d(A);

subplot(1,2,1)
plot(ds(end,:)')
