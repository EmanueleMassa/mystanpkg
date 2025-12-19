glm_napp <- function(
    Y,
    X,
    family = "binomial",
    historical_data,
    chains = 4,
    warm_up = 1000,
    iter = 2000,
    shape_omega = 1,
    shape_delta = 1,
    seed = NULL
){
  if(family == "binomial"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/binomial_napp_compiled.rds")
  }
  if(family == "poisson"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/poisson_napp_compiled.rds")
  }
  n <- length(Y)
  p <- length(X[1, ])
  l <- length(historical_data$coef)
  cov_hat <- array(NA, dim = c(l, p, p))
  theta_hat <- array(NA, dim = c(l, p))
  for (j in 1:l) {
    cov_hat[j,,] <- historical_data$cov[[j]]
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
    omega = rep(shape_omega, l),
    delta = rep(shape_delta, l)
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
