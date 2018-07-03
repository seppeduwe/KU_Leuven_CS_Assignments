A = rand(1000,1000);
tic
n = norm(A);
toc

tic
A = rand(1000,1000);
[U,S,V] = svd(A);
[a,b] = doeietsnuttigs(1,2,3);
toc
