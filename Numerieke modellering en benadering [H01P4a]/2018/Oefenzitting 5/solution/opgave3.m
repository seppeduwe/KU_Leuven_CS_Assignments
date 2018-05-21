format short e;

X = rand(5,2); X(2:5,1) = X(2:5,1)*1.e-8;

U1 = X(:,1) + norm(X(:,1))*eye(size(X,1),1)
U2 = X(:,1) - norm(X(:,1))*eye(size(X,1),1)

H1X = X  - 2/ (U1'*U1) * U1 * U1' * X 
H2X = X  - 2 / (U2'*U2) * U2 * U2'* X 

%% better u2
UU2 = -sum(X(2:5,1).^2)/U1(1);
UU2(2:5,1) = X(2:5,1);

HH2X = X  - 2 / (UU2'*UU2) * UU2 * UU2'* X 

% error u2 vs uu2
E = (U2-UU2)./UU2