function m=genmatrixc(dmat)
echo off
% genereert een random matrix met een bepaald permutatie-gedrag
% dmat=4
%seed=input('Geef je kontrole-nummer:')
%rand('seed',seed);
%for i=1:seed+13
%	rand;
%end
%colp=2+fix(rand*(dmat-2));
colp=2+fix(rand*(dmat-3)); %mvb 04/01/96
rowp=colp+1+fix(rand*(dmat-colp));
m=triu(rand(dmat))+eye(dmat);
for j=dmat-1:-1:colp+1
	for i=j+1:dmat
		m(i,:)=m(i,:)+rand*m(j,:);
	end
end
for i=colp+1:dmat
	m(i,colp)=(1+rand)*1.0e-10;
end
m(colp,colp)=(1+rand)*1.0e-10;
for j=colp-1:-1:1
	for i=j+1:dmat
		m(i,:)=m(i,:)+rand*m(j,:);
	end
end
for i=1:dmat
	x(i)=fix(rand*100);
end
b=m*x';
m=[m b];
