function [ a,b ] = fourier_reeks( f,M,N )
% Bereken de coefficienten van de fourierreeks die een periodische
% herhaling van de functie f op het interval -pi...pi benadert. De
% basisfuncties die gebruikt worden hebben periode 2pi tot 2pi/M voor de
% oneven basisfuncties en 2pi tot 2pi/N voor de even basisfuncties.

a = zeros(1,M+1); b = zeros(1,M);

a(1) = 1/(2*pi)*integral(f,-pi,pi);

for i = 1:M
    s = @(x) f(x).*sin(i*x);
    a(i+1) = 1/pi*integral(s,-pi,pi);
end
for i = 1:N
    c = @(x) f(x).*cos(i*x);
    b(i) = 1/pi*integral(c,-pi,pi);
end

end

