% AB INBEV
clc; clear all; close all;
%%
load abinbev.mat
x = data(:,1); f = data(:,2);
figure;
plot(x,f);
%%


x_eval = [x; linspace(x(end) + 1, x(end) + 10, 10)']; 
w = ones(size(x));
n = 1:15;
for k = n
    c = kkb(x,f,w,k);
    r(:,k) = polyval(c(end:-1:1),x_eval);
end

for k = n
    figure;
    plot(x,f,'b.','markersize',15); hold on;
    plot(x_eval,r(:,k),'r','linewidth',2);
    title(k);
    pause;
end
close all;