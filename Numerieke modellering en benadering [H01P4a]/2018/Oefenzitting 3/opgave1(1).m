f = @(x) (mod(x-pi,2*pi)-pi)/pi; 
%f = @(x) sin((mod(x-pi,2*pi) -pi).^2);
t = -10:0.01:10;
plot(t,f(t),'b','linewidth',2)
N = 0;
for M = 1:4
    [a,b] = fourier_reeks(f,M,N);
    g = evalueer_FR(a,b,t);
    hold on;
    plot(t,g,'linewidth',1.5)
end
