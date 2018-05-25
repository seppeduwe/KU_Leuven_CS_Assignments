% Computes the Inverse of a weakly row diagonally dominant M-matrix
% given the row sums s and offdiagonal elements A
% function B=InverseMM(s,A);

function [B,L,U]=InverseMM(s,A);
n=length(s);
for k=1:n-1
   for i=k:n
      A(i,i)=s(i)-sum(A(i,[k:i-1,i+1:n]));
   end
   
   for i=k+1:n
      if A(k,k)~=0
         A(i,k)=A(i,k)/A(k,k);
         s(i)=s(i)-A(i,k)*s(k);
         for j=k+1:n
            if i~=j
               A(i,j)=A(i,j)-A(i,k)*A(k,j);
            end
         end
      end
   end
end
A(n,n)=s(n);

L=tril(A,-1)+eye(n);
U=triu(A);

B=U\(L\eye(n));
