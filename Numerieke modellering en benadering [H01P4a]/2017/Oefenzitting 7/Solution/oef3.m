clear all;
%input1 = [3 5];
%input2 = [8 2];
%input1getal = 53
%input2getal = 28

r_number = 04;
f = [1 0];
for i=2:50+ r_number
    n = fliplr(str2double(regexp(int2str(i),'\d','match')));
    f = mult_fft(f,n);
end

% Convert vec to number
A = [1, 10*ones(1,length(f)-1)];
A = cumprod(A);
output = A.*f;
sum(output)

% Convert vec to char
char(real('0')+f(end:-1:1))

% Show last 20 numbers in vector
f(end-19:end)