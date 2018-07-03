function [c] = kkb2(x,f,w,n)

% c = kkb2(x,f,w,n)
%
% Stelt discrete kleinste-kwadratenveeltermbenadering op
% van graad n voor de punten (x_i,f_i) met gewichten w_i,
% i = 1:N, gebruikmakende van het normaalstelsel
%
% size(x) = size(f) = size(w) = [N 1]
%

warning off;

% gewichten
D = diag(sqrt(w));

% rechterlid
b = D*f;

% stelsel
A = [ones(size(x)) x*ones(1,n)];
A = cumprod(A,2);
A = D*A;

% Constructie normaalstelsel
b = A'*b;
A = A'*A; 

% oplossing
c = A\b;
