clear all;

%% Lees uw foto
a = imread('lena_gray_512.tif');
% load('lena.mat');

%% Converteer naar grijswaardebeeld
% a = rgb2gray(a);

%% Verklein
%a = a(1:2:size(a,1),1:2:size(a,2),:);

%% Bekijk
% image(a);
imshow(a);
axis image;

%% Bereken 2D fft
f = fft2(a);

%% Verwijder de DC-component
fm = f(1,1,:);
f(1,1,:) = 0;

%% Bekijk via (onder andere)
% figure;
% mesh(abs(f(:,:,1)));
% view(2);
% surf(abs(f(:,:,1)));
% plot(abs(f(:,1,1)));
% plot(abs(f(1,:,1)));
% 
% f = fftshift(f);
% mesh(abs(f(:,:,1)));
% f = fftshift(f);

%% Herstel DC-component
f(1,1,:) = fm;

%% Zet een deel van f op nul
N = 512;

figure;
imagesc(log(abs(f)))

f(1:end,N:end-N)=0;
f(N:end-N,1:end)=0;

l = figure;
imagesc(log(abs(f)))

%% Bereken inverse 2D fft
b = uint8(round(ifft2(f)));

%% Bekijk het resultaat
h = figure;
imshow(real(b));

%% Sla het resultaat op
filename = sprintf('lena_%d', N);
saveas(h,filename,'bmp')

filename = sprintf('lenaFreq_%d', N);
saveas(l,filename,'bmp')