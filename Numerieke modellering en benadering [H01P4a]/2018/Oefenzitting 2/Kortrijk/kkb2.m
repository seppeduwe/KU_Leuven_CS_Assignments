function c = kkb2(x, f, w, n)
%KKB2 Discrete weighted least-squares approximation of degree n for the
%function that takes the values f in the points x for the given weigths w.
%This method uses an efficient implementation of the normal equations to 
%solve the system of equations for large number of points.
% Inputs
% x     Vector holding the points in which function values are given
% f     Function values in the given points
% w     Vector with weights in given points
% Outputs
% c     Coefficients of the approximating degree n polynomial
   
    % Make sure that all vectors are column vectors
    x = x(:); f = f(:); w = w(:);
    
    % Check input
    N = length(x);
    if N < n
        error('plotres:input', ...
         'length(x) should be larger than n for an overdetermined system');  
    end   
    if any([length(f),length(w)]~= length(x))
        error('plotres:input', ...
                'r and w should have the same number of elements as x');  
    end
    
    % Compute powers of x_i form zero up to 2n to admit all powers x_i^(j+k)
    X = [ones(N, 1),cumprod(x(:,ones(1,2*n)), 2)];

    % Form weighted sums over i for all terms of the form w_i*x_i^(j+k)
    % Only the 2n distinct powers of x_i stored in X are needed as j,k < n
    wX = w'*X;
    
    % Fill up the nxn-matrix AtWA with the weighted sums
    % The (j,k)th element of AtWA holds the value sum_i(w_i*x_i^(j+k))
    AtWA = hankel(wX(1:n+1), wX(n+1:end)');
    
    % Form nx1 vector b for the right side of the equation
    % Only n weighted sums are needed here
    b = (w.*f)'*X(:,1:n+1);
    
    % Solve system to obtain coefficient vector c
    c = AtWA \ b';
end
