function output = mult_fft(input1,input2)
%input1 = fliplr(str2double(regexp(int2str(input1),'\d','match')))
%input2 = fliplr(str2double(regexp(int2str(input2),'\d','match')))

maxLength = max(find(input1 ~= 0,1,'last'),find(input2 ~= 0,1,'last'));
maxLength = 2*maxLength;

%input1 = input1(1:maxLength);
%input2 = input2(1:maxLength);
input1(end+1:maxLength) = 0;
input2(end+1:maxLength) = 0;

input1_fft = fft(input1);
input2_fft = fft(input2);
input_fft = input1_fft .* input2_fft;
output = round(real(ifft(input_fft)));

while(any(output > 9))
    output = [0 floor(output/10)] + [rem(output,10) 0];
end

output = output(1:find(output ~= 0,1,'last'));

% Convert vec to number
% A = [1, 10*ones(1,maxLength-1)];
% A = cumprod(A);
% output = A.*input;
% output = sum(output);

% Convert vec to char
% char(real('0')+output(end:-1:1))