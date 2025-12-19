data {
    int<lower = 0> n;
  	int<lower = 1> p;          // number of predictors
  	int<lower = 1> l; 		// number of groups  excl current
  	matrix[l, p] theta_hat;
  	matrix[p, p] cov_hat[l];
  	int<lower = 0, upper = 1> Y[n];
  	matrix[n, p] X;
  	real omega[l];
  	real delta[l];
}

parameters {
	vector[p] theta;
	real<lower = 0, upper = 1> alpha[l];
	}

model {
	//prior
	for (j in 1:l) 
    alpha[j] ~ beta(omega[j], delta[j]);
  	// likelihood
	for (j in 1:l)
		target += multi_normal_lpdf(theta| theta_hat[j], cov_hat[j] / alpha[j]);
	Y ~ bernoulli_logit_glm(X,  rep_vector(0, n), theta);
}
