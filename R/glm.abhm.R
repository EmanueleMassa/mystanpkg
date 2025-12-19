glm_abhm <- function(
    Y,
    X,
    family = "binomial",
    historical_data,
    chains = 4,
    warm_up = 1000,
    iter = 2000,
    var_meta_mean = 10,
    var_meta_var = 5,
    seed = NULL
){
  if(family == "binomial"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/binomial_abhm_compiled.rds")
  }
  if(family == "poisson"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/poisson_abhm_compiled.rds")
  }
  n <- length(Y)
  p <- length(X[1, ])
  l <- length(historical_data$coef)
  cov_hat <- array(NA, dim = c(l, p, p))
  theta_hat <- array(NA, dim = c(l, p))
  for (j in 1:l) {
    cov_hat[j,,] <- chol(historical_data$cov[[j]])
    theta_hat[j,] <- historical_data$coef[[j]]
  }
  stan_data <- list(
    n = n,
    p = p,
    l = l,
    theta_hat = theta_hat,
    cov_hat = cov_hat,
    Y = Y,
    X = X,
    var_beta = var_meta_mean,
    var_tau = var_meta_var
  )
  fit <- sampling(sm,
           data = stan_data,
           chains = chains,
           warmup = warm_up,
           iter = iter,
           cores = 2,
           seed = seed,
           refresh = 0)
  return(fit)
}
