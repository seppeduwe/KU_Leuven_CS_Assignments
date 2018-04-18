function [tx,ty] = zonnevlek(dayssn,yearssn)

figure; 
plot(dayssn(:,1),dayssn(:,2)); hold on; 
plot(yearssn(:,1),yearssn(:,2),'LineWidth',2);

% dayssn
x = fft(dayssn(:,2));
x = abs(x);             % amplitude nemen
nx = floor(length(x)/2);

figure;
semilogy(x(2:nx));      % Enkel waarden uitzetten die horen bij frequenties 
                        % lager dan de Nyquist frequentie.
                        % Het spectrum van een reele rij is immers
                        % symmetrisch rond deze frequentie.

[maxval,indexx] = max(x(2:nx));
indexx = indexx + 1;
                        % index frequentie met hoogste amplitude zoeken
tx = length(x) / (indexx*365); % bijhorende periode

% yearssn
y = fft(yearssn(:,2));
y = abs(y);
ny = floor(length(y)/2);
figure;
plot(y(2:ny),'b', 'linewidth', 2);

indexy = find(y(1:ny)==max(y(2:ny)));
ty = length(y)/indexy;
