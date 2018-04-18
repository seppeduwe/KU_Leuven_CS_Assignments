%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Inleidingsles Matlab %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basiscommando's
2+2
2+3; % Nuttige commentaar voor later
ans
a = 4;
disp('I <3 Matlab');
disp(a);
clear a;



%% Vectoren en matrices

vec1 = [1,5,42,15,6,8] % Vierkante haakjes!
vec1 = [1 5 42 15 6 8]
mat1 = [1,2,3;4,5,6]
vec2 = 1:10
vec2 = 1:3:10
transpose = mat1.'

% Elementen selecteren
element23 = mat1(2,3) % Ronde haakjes!
rij2 = mat1(2,:)
kolom3 = mat1(:,3)
selectie = vec1([1:3,5:6])
vec1(1:2) = [3000,200]

% Bewerkingen met matrices en vectoren

A = randi(10,4,3)
B = randi(10,4,3)
t = rand(4,1)
A+B
A-1
A*B
A/2
A*B.'

x = A\t % Los Ax=t op
format long % Toon meer cijfers
x

% Elementsgewijs
A*A
A.*A
A.^2
A./A 

% Ingebouwde functies

svd(A) % Hoe werkt dit?
help svd % Of Matlab website, Google, Mathworks forum, stackoverflow, vriendjes, Michiel
[U,S,V] = svd(A)
[U,S,V] = svd(A,0) % Gereduceerde SVD
[~,S,V] = svd(A,0) % Tilde voor ongewenste uitvoer

vec1
mean(vec1)
mat1
mean(mat1) % Werkt in op elke kolom
mean(mat1,2) % Gemiddelde volgens de tweede (kolom) dimensie
mean(mean(mat1))
mean(mat1(:))

C = ones(3)
C = zeros(3,2)
C = eye(3,5)
size(C)

%% Plotten

x = 1:0.05:10;
y = sin(x);
plot(x,y);

figure();
plot(x,x.^2);
hold on;
plot(x,x.^3,'m--');
hold off;
title('Mijn mooiste plotje');
xlabel('Tijd');
ylabel('Temperatuur');
legend('Kwadraat', 'Derdemacht');

figure();
semilogy(x,x.^2);
loglog(x,x.^2);

%% Functies

help doeietsnuttigs
[som, prod] = doeietsnuttigs(2,3,4)

% For-lus
s = 0;
for i = 1:10
    s = s+i;
end
s
s = sum(1:10)

% While-lus
t = 0;
while t ~= 10 % Niet /=, niet |=, niet !=
    t = t+1;
end
t

s = 20;
% If-voorwaarde
if s >= 10 % Groter of gelijk aan
    disp('Amai, zo groot!');
elseif s == 0 % Gelijk aan
    disp('NUL!')
elseif s == 1 || s == -1 % || is logische of, && is logische en
    disp('Absolute waarde is 1')
else
    disp('Niets speciaals');
end

%% Debuggen

[som, prod] = doeietsmindernuttigs(2,3,4)

% Timings: time.m


%% Workspace opslaan
save('MijneerstescriptjevoorNMB')
clear all
load('MijneerstescriptjevoorNMB')
% Willekeurige data inladen?
A = importdata('voorbeeld.jpg');
image(A)

