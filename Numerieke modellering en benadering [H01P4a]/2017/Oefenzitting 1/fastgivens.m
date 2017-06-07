function [alfa,beta,type,newd]=fastgivens(x,d)

% function [alfa,beta,type,newd]=fastgivens(x,d)
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
		
		if gamma <= 1
			type=1;
			tau=d(1);
			newd(1)=(1+gamma)*d(2);
			newd(2)=(1+gamma)*tau;

		else
			type=2;
			alfa=1/alfa;
			beta=1/beta;
			gamma=1/gamma;
			newd(1)=(1+gamma)*d(1);
			newd(2)=(1+gamma)*d(2);
		end 

	else
		type=2;
		alfa=0;
		beta=0;
	end 
