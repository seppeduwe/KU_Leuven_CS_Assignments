function opgave5(sigma, U, V, type)

% veronderstel dat de sigma geordend zijn van groot naar klein
I = eye(10);
e1 = I(:,1);
e7 = I(:,7);
if type == 1 
  c = rand(10,1);
  dc = rand(10,1);
elseif type == 2
  c = rand * e1;
  dc = rand * e7;
elseif type == 3  
  c = rand * e7;
  dc = rand * e1;
elseif type == 4  
  c = rand(10,1);
  dc = [zeros(7,1); rand(3,1)];
elseif type == 5  
  c = rand(10,1);
  dc = [rand(7,1); zeros(3,1)];
end

% oorspronkelijk stelsel
tol = 1e-8;
[A, b1, x1, kapA, kapbx, eta] = opgave4(sigma, c, U, V);

% geperturbeerd stelsel via opgave4
[A, b2, x2] = opgave4(sigma, c + tol * dc, U, V);

% geperturbeerd stelsel via \ operator
% b2 = b1 + tol * U * dc;
% x2 = A \ b2;

rx = norm(x2-x1) / norm(x1);
rb = norm(b2-b1) / norm(b1);
kexp = rx / rb;

fprintf(1, '%.1e  %3d  %.1e %.1e %.1e %.1e %.1e \n', ...
	kapA, type, eta, kapbx, rx, rb, kexp);


