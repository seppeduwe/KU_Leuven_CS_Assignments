function plotres(x,r,w)
%PLOTRES plots residuals in combination with weights
% Inputs
% x     Vector holding the points in which to plot residuals and weights
% r     Residuals that are to be plotted
% w     Vector with weights that are to be plotted

    % Check input
    if any([length(r),length(w)]~= length(x))
        error('plotres:input', ...
                'r and w should have the same number of elements as x');  
    end

    hold on     
    plot(x,r,'r+-') % Plot residuals
    
    plot([x(1) x(end)], [0 0], 'k:') % x-axis
    scalefactor = max(abs(r)); % Compute maximum distance from x-axis
    
    plot(x,w/max(w)*scalefactor,'g--') % Plot scaled weights
    ylim([-scalefactor scalefactor]) % Center x-axis
    
    hold off
end
