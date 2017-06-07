function c = kkb(x, f, w, n)
%function c = kkb(x, f, w, n)
%
%   Stelt discrete kleinste-kwadratenveeltermbenadering op
%   van graad n voor de punten (x_i,f_i) met gewichten w_i,
%   i = 1:N .
%
% size(x) = size(f) = size(w) = [N 1]


N = length(x);
A = zeros(n+1,n+1);
B = zeros(n+1,1);

% 1) de trage manier met geneste for-lussen
% for i=0:n
%    for j=0:n
%        for k=1:N
%            A(i+1,j+1) = A(i+1,j+1)+w(k)*x(k)^(i+j);
%        end
%    end
%    for k=1:N
%        B(i+1) = B(i+1) + w(k)*x(k)^i*f(k);
%    end
% end


% 2) een snellere manier: gebruik vector-operaties waar mogelijk

% maak machten van x, vermenigvuldigd met gewichten
X = [ones(size(x)) x*ones(1,2*n)];
X = cumprod(X,2);
X = X .* (w*ones(1,2*n+1));

% matrix A heeft hankel structuur: constant op de anti-diagonalen
A = sum(X,1);
A = hankel(A(1:n+1),A(n+1:end)');

% rechterlid
B = X(:,1:n+1) .* (f*ones(1,n+1));
B = sum(B,1)';

% Conditiegetal van de coefficientenmatrix
% condA = cond(A)

% Stelsel oplossen
c = A\B;


