% b)

A = [-3 0.5 0.1; 
     0.2 -4   0; 
     1 1   1];
   
lambda = eig(A);

figure;
plotGerschgorinDisks(A,lambda)

figure;
plotGerschgorinDisks(transpose(A),lambda)

function plotGerschgorinDisks(A,lambda)
  c = diag(A);
  r = sum(abs(A-diag(diag(A))),2);
  points = 50;
  clf;
  plot(real(lambda),imag(lambda),'r.','MarkerSize',15);
  hold on
  for i = 1:length(c)
    Di = c(i) + r(i)*exp(1i*linspace(0,2*pi,points));
    plot(real(Di),imag(Di),'b--','LineWidth',2);
  end
end
