function c = kkb1(x, f, w, n)
%KKB1 Discrete weighted least-squares approximation of degree n for the
%function that takes the values f in the points x for the given weigths w.
%This method solves the overdtermined system of equations directly.
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
    
    % Compute square roots of weight vector and form matrix A
    d = sqrt(w);
    A = [ones(N, 1),cumprod(x(:,ones(1,n)), 2)];

    % Compute D*A and D*f to obtain a system Qx = b.
    Q = d.*A;
    b = d.*f;
    % Solve overdetermined system to obtain coefficient vector c
    c = Q \ b;
end
