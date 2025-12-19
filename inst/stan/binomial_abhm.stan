data {
    int<lower = 0> n;
  	int<lower = 1> p;          // number of predictors
  	int<lower = 1> l; 		// number of groups  excl current
  	matrix[l, p] theta_hat;
  	matrix[p, p] cov_hat[l];
  	int<lower = 0, upper = 1> Y[n];
  	matrix[n, p] X;
  	real var_beta;
  	real var_tau;
}

parameters {
	matrix[l, p] z;
	vector[p] xi;
 	vector[p] beta;
 	real<lower = 0> tau;
}

transformed parameters{
  matrix[l, p] theta;
  theta = rep_matrix(beta', l) + tau * z;   // non-centered
  vector[p] phi;
  phi = beta + tau * xi;
}

model {
	beta ~ normal(0, var_beta);
	tau ~ student_t(3, 0, var_tau);
	  #normal(0, var_tau) T[0, ];
 	// hierarchical prior: per-group coefficients
  to_vector(z) ~ normal(0, 1);
  xi ~ normal(0, 1);
  	// likelihood
	for (j in 1:l)
		target += multi_normal_cholesky_lpdf(theta[j] | theta_hat[j], cov_hat[j]);;
	Y ~ bernoulli_logit_glm(X,  rep_vector(0, n), phi);
}
