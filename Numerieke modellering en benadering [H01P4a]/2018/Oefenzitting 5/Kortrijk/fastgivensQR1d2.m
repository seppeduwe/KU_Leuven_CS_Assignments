function [d,T,ds]=fastgivensQR1d(A)
...
ds=zeros(m,n);
...
for j=1:n
    for i=m:-1:j+1
        %nieuwe alfa,beta en d bepalen
        [alfa,beta,type,newd]=fastgivens1(T(i-1:i,j),d(i-1:i));
        % we willen dit weten 
        d(i-1:i)=newd;
        ...
    end
    % opslaan
    ds(:,j) = d';
end 
