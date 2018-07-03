function L = eigcirk(c, R, N)

% function L = eigcirk(c, R, N)
%
% genereert N complex getallen in cirkel met straal R en centrum c

t = rand(N,1)*2*pi;
a = sqrt(rand(N,1))*R;
L = c + a.*exp(i*t);

