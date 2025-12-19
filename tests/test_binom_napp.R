library(rstan)
library(tictoc)
library(hdbayes)

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
Y <- rbinom(n = n, size = 1, prob = plogis(lp))


#compute sufficient statistics
data_fits <- list(
  coef <- list(),
  cov <- list()
)

for(k in 1:(l-1)){
  y <- Y[group == k]
  x <- X[group ==k, ]
  fit.glm <- glm(y ~ x+ 0, family = binomial)
  data_fits$coef[[k]] <- coef(fit.glm)
  data_fits$cov[[k]] <- vcov(fit.glm)
}

data_fits
source("glm.napp.R")

y <- Y[group == l]
x <- X[group == l, ]

tic()
fit_napp <- glm_napp(y, x, historical_data = data_fits, seed = 123)
toc()
fit_napp


currdata <- list(
  y = Y[group ==l],
  x = X[group ==l, ]
)

histdata <- list(
  y = Y[group != l],
  x = X[group != l, ]
)

data_list <- list(currdata = currdata, histdata = histdata)


data_list[[1]]
# Fit logistic regression with normalized power prior
fit_npp <- glm.npp(
  formula = y ~ x1 + x2,
  family = binomial(link = "logit"),
  data.list = data_list,
  a0.shape1 = 1,
  a0.shape2 = 1,
  chains = 2,
  iter_warmup = 500,
  iter_sampling = 500
)



