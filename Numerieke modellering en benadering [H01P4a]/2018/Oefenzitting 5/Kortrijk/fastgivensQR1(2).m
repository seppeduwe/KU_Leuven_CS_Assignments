function [d,T]=fastgivensQR1(A)

% function [d,T]=fastgivensQR1(A)
%
% bereken bovendriehoeksmatrix T met snelle Givens transformaties
% M'*A = T, M'*M = diag(d)
% M wordt niet opgebouwd
% Q = M*diag(d.^(-0.5)), R = diag(d.^(-0.5))*T is een QR factorisatie
% in : A(1:m,1:n), m >= n
% uit : d(1:n), T(1:m,1:n) bovendriehoeks

% intialisatie
[m,n]=size(A);
T=A;
d=ones(1,m);

% T zal de bovendriehoeksvorm van A bevatten

for j=1:n
	for i=m:-1:j+1
		
		%nieuwe alfa,beta en d bepalen
		[alfa,beta,type,newd]=fastgivens1(T(i-1:i,j),d(i-1:i));
		d(i-1:i)=newd;
		
		%fast givens rotatie toepassen op T
		if type==1 % als tweede element niet reeds nul is
			T(i-1:i,j:n)=[beta 1 ; 1 alfa]*T(i-1:i,j:n);
		end 
    end
end 
