function opgave2(i,j)
% opgave2(i,j)
%
% Input 
% (i,j) : aantal frequenties die niet weggefilterd worden in beide richtingen


% fotootje inladen en tonen
X = imread('lena_gray_512.tif');
figure(1); imagesc(X);
colormap gray;
%axis image;


% fourier-transformatie
F = fft2(X);
figure(2); imagesc(log(abs(F)));


% deel van de matrix nul stellen
% (hoge frequenties wegfilteren)
F(i+1:end+1-i,:) = 0;
F(:,j+1:end+1-j) = 0;
figure(3); imagesc(log(abs(F)));


% inverse fourier-transformatie
X1 = real(ifft2(F));
figure(4); imagesc(X1);
colormap gray;
axis image;

factor = nnz(F)/(size(F,1)*size(F,2))

