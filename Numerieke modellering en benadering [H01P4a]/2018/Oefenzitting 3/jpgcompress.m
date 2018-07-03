function q = jpgcompress(jpg, percent)

i0 = size(jpg,1);
j0 = size(jpg,2);

i1 = round(i0/2*percent/100);
j1 = round(j0/2*percent/100);

% fourier-transformatie
f = fft2(jpg);

% deel van de matrix nul stellen
% (hoge frequenties wegfilteren)
f(i1:i0-i1,:,:) = 0;
f(:,j1:j0-j1,:) = 0;

% inverse fourier-transformatie
q = uint8(abs(ifft2(f)));

