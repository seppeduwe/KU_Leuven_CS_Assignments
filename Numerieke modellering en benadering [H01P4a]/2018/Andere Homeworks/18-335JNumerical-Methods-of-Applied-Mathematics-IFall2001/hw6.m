for i=1:200 for j=1:200 A(i,j)=-1/i-1/j; end; end
s=2.^[2:2:400];                                  
1/norm(InverseMM(s,A)) 
