data {
  	int<lower=0> n;          // number of observations
  	int<lower=1> p;          // number of predictors
  	int<lower = 2> l; 		// number of groups
  	matrix[n, p] X;          // design matrix
  	int<lower=0> Y[n];            // response
  	int<lower =1, upper = l>  group[n]; // group label
  	real<lower=0> var_beta;
  	real<lower=0> var_tau;
}

parameters {
  matrix[l, p] z;            // standardized group effects
 	vector[p] beta;
 	real<lower = 0> tau;
}

transformed parameters{
  matrix[l, p] theta;
  theta = rep_matrix(beta', l) + tau * z;   // non-centered
}

model {
	  beta ~ normal(0, var_beta);
	  tau ~ student_t(3, 0, var_tau);

    // hierarchical prior: per-group coefficients
    to_vector(z) ~ normal(0, 1);

  	// likelihood
  	vector[n] eta;
    for (i in 1:n)
      eta[i] = X[i] * theta[group[i]]';
    Y ~ poisson_log(eta);
}
