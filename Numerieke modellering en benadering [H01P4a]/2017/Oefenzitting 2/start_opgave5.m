U = orth(rand(10));
V = orth(rand(7));

fprintf(1, '%7s  %4s %7s %7s %7s %7s %7s \n', ...
	'kap_A', 'type', 'eta', 'kap_bx', 'rx', 'rb', 'k_exp');
for kappa = [1 1e3 1e6]
  sigma = linspace(kappa, 1, 7)';
  for type = [1 2 3 4 5]
    opgave5(sigma, U, V, type);
  end
end

