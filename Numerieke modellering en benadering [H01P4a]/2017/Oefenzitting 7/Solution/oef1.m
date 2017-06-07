clear all;

load('yearssn.mat');
load('dayssn.mat');

plot(yearssn(:,1),yearssn(:,2))
plot(dayssn(:,1),dayssn(:,2))

fft_yearssn = abs(fft(yearssn(:,2)));
fft_dayssn = abs(fft(dayssn(:,2)));

fft_yearssn = real(sqrt(real(fft_yearssn)+abs(imag(fft_yearssn).^2))); 
fft_dayssn = real(sqrt(real(fft_dayssn)+abs(imag(fft_dayssn).^2))); 

fft_yearssn_x = linspace(0,1,size(fft_yearssn,1));
fft_dayssn_x = linspace(0,1,size(fft_dayssn,1));

[max_fft_yearssn,id_fft_yearssn] = max(fft_yearssn(2:(end+1)/2));
f_yearssn = id_fft_yearssn*(1/length(fft_yearssn));
T_yearssn = 1/f_yearssn

[max_fft_dayssn,id_fft_dayssn] = max(fft_dayssn(2:end/2));
f_dayssn = id_fft_dayssn*(1/length(fft_dayssn));
T_dayssn = 365.25/f_dayssn % Convert to years

figure(1);
plot(fft_yearssn_x(2:(end+1)/2),fft_yearssn(2:(end+1)/2));
figure(2);
plot(fft_dayssn_x(2:(end)/2),fft_dayssn(2:(end)/2));