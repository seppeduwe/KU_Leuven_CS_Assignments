function [alfa,beta,type,newd]=fastgivens1(x,d)

% function [alfa,beta,type,newd]=fastgivens1(x,d)
%
% bereken de parameters van een snelle Givens rotatie
% in : x(1:2), d(1:2)
% uit : alfa, beta, type (1 of 2), newd(1:2)

% initialisatie
    newd=zeros(1,2);

    % berekening alfa, beta en nieuwe d waarden

    if x(2) ~= 0
        alfa=-x(1)/x(2);
        beta=-alfa*d(2)/d(1);
        gamma=-alfa*beta;
        
        
        type=1;
        tau=d(1);
        newd(1)=(1+gamma)*d(2);
        newd(2)=(1+gamma)*tau;

    else
        type=2;
        alfa=0;
        beta=0;
    end 
