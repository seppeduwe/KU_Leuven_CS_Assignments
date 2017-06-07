function L = eigint(rmin, rmax, N)

% function L = eigint(rmin, rmax, N)
%
% genereert N reele getallen in (rmin, rmax)

L = rmin + rand(N,1)*(rmax-rmin);
