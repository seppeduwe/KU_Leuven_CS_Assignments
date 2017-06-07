function [x, relres, f] = steilstehelling(A, b, x, max_it, tol)

for it = 1:max_it
  r = b - A*x;
  alpha = (r'*r) / (r'*A*r);
  relres(it) = norm(r) / norm(b);
  f(it) = 0.5 * x'*A*x - x'*b;
  x = x + alpha * r;
  if relres(it) < tol, break; end
end
