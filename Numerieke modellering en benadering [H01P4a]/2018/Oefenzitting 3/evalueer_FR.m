function [ f] = evalueer_FR( a,b,t )
% Evalueer de fourierreeks gegeven door de coefficienten a,b in de punten
% t.

f = zeros(size(t));
f = f + a(1);
for i = 2:length(a)
    f = f + a(i)*sin((i-1)*t);
end
for i = 1:length(b)
    f = f + b(i)*cos(i*t);
end

end

