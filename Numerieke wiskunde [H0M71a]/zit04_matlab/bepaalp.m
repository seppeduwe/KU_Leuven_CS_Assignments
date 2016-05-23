function p=bepaalp(b)
% Bereken het aantal beduidende cijfers in de mantisse, 
% als b gekend is ( bv. uit bepaalb.m )
p = 1;
z = b;
while ((z+1) - z) == 1,
   p = p+1;
   z = z*b;
%    fprintf('p =%3i  z =%17i\n',p,z)
end 

