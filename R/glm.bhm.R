glm_bhm <- function(
    Y,
    X,
    group,
    family = "binomial",
    chains = 4,
    warm_up = 1000,
    iter = 2000,
    var_meta_mean = 10,
    var_meta_var = 5,
    seed = NULL
){
  l <- max(group)
  if(family == "binomial"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/binomial_bhm_compiled.rds")
  }
  if(family == "poisson"){
    Y <- as.integer(Y)
    sm <- readRDS(file = "./inst/stan/poisson_bhm_compiled.rds")
  }
  stan_data <-  list(
    n = length(Y),
    p = length(X[1,]),
    l = l,
    X = X,
    Y = Y,
    group = group,
    var_beta = var_meta_mean,
    var_tau = var_meta_var
    )
  fit <- sampling(sm,
                  data = stan_data,
                  chains = 4,
                  warmup = warm_up,
                  iter = iter,
                  cores = 2,
                  seed = seed,
                  refresh= 0
  )
  return(fit)
}
