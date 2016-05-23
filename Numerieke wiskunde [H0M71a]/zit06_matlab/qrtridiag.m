function [Q,R]=qrtridiag(M)

[n1,n2]=size(M);

% initialisatie transformatiematrix
T=eye(n1);
% initialisatie R
R=M;


for k=1:(n1-1);
    
    % givens rotatie
    
    G=planerot(...);
        
    % 'elementaire' transformatie-matrix Te

    Te= eye(n1);
    Te(...,...)=G;
    
    % update R

    R=Te*R;    
    R(k+1,k)=0;
    
    % update transformatie
    
    T=Te*T;

end


Q=...;
