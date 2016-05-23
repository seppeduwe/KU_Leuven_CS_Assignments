function b = bepaalb
% Bepaal het grondtal waarmee de computer werkt
a = 1;
while ((a+1) -a) == 1,
   a = a*2;
%    fprintf('a: %16i\n',a)
end
i = 1;
while (a == (a+i)),
   i = i + 1;
%    fprintf('i: %16i\n',i)
end
b = (a + i) -a;
