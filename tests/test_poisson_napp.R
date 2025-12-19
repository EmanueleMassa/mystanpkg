library(rstan)
library(tictoc)

beta <- c(-1, 0, 1)
tau <- 0.01
l <- 5
p <- 3
betas <- MASS::mvrnorm(n = l, mu = beta, Sigma = (tau**2) * diag(rep(1, 3)))

# generate data
n <- 500
group <- sample(1:l, size = n, replace = TRUE, prob= rep(1/l, l))

X <- MASS::mvrnorm(n = n, mu = rep(1,3), Sigma = diag(rep(1, 3)) + matrix(0.0, nrow = 3, ncol = 3))
lp <- diag(X%*% t(betas[group, ]))
Y <- rpois(n = n, lambda = exp(lp))



#compute sufficient statistics
data_fits <- list(
  coef <- list(),
  cov <- list()
)

for(k in 1:(l-1)){
  y <- Y[group == k]
  x <- X[group ==k, ]
  fit.glm <- glm(y ~ x+ 0, family = poisson)
  data_fits$coef[[k]] <- coef(fit.glm)
  data_fits$cov[[k]] <- vcov(fit.glm)
}

data_fits
source("glm.napp.R")

y <- Y[group == l]
x <- X[group == l, ]

tic()
fit <- glm_napp(y, x, family = "poisson", historical_data = data_fits, seed = 123)
toc()
fit



