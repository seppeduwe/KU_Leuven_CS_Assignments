function [c] = mult_int(a,b)

% a en b moeten als volgt worden voorgesteld, als a het getal 512 voorstelt
% dan is a = [1 2 5];
base = 10;

% Geef a en b lengte 2*max(lengte(a),lengte(b))
l1 = length(a);
l2 = length(b);
lm = 2 * max(l1,l2); 
a(l1+1:lm) = zeros([1 lm-l1]);
b(l2+1:lm) = zeros([1 lm-l2]);

% DFT-vermenigvuldiging
x = fft(a);
y = fft(b);
z = x .* y;
c = round(real(ifft(z)));

% Voer overdracht uit
for i=1:lm-1
  m = rem(c(i), base);
  c(i+1) = c(i+1) + (c(i)-m) / base;
  c(i) = m; 
end

% Verwijder overtollige nullen:
for i=lm:-1:1
  if (c(i)~=0), break; end
end
c = c(1:i);


