library(rstan)
library(tictoc)
library(brms)


devtools::load_all()

beta <- c(-1, 0, 1)
tau <- 0.3
l <- 5
p <- 3
betas <- MASS::mvrnorm(n = l, mu = beta, Sigma = (tau**2) * diag(rep(1, 3)))

# generate data
n <- 500
group <- sample(1:l, size = n, replace = TRUE, prob= rep(1/l, l))

X <- MASS::mvrnorm(n = n, mu = rep(1,3), Sigma = diag(rep(1, 3)) + matrix(0.0, nrow = 3, ncol = 3))
lp <- diag(X%*% t(betas[group, ]))
Y <- rbinom(n = n, size = 1, prob = plogis(lp))

df <- data.frame(Y = Y, group = factor(group))
df <- cbind(df, X)
colnames(df)[3:(2+p)] <- paste0("X", 1:p)

# ---- Formula for brms ----
# Random intercepts + group-specific slopes (hierarchical)
formula <- bf(Y ~ 0 + (1|group) + (0 + X1|group) + (0 + X2|group) + (0 + X3|group),
              family = bernoulli())

# ---- Fit hierarchical logistic regression with brms ----
fit_brms <- brm(formula, data = df, chains = 2, iter = 1000, cores = 2,
                seed = 123, refresh = 0)

fit_brms

tic()
fit <- glm_bhm(Y, X, group, seed = 123, var_meta_mean = 10, var_meta_var = 2.5)
toc()

fit

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
source("glm.abhm.R")

y <- Y[group == l]
x <- X[group == l, ]

tic()
fit2 <- glm_abhm(y, x, historical_data = data_fits, seed = 123, var_meta_mean = 10, var_meta_var = 2.5)
toc()
fit2
fit

print(fit, pars = c("beta", "tau"))
print(fit2, pars = c("beta", "tau"))

stan_trace(fit2, pars=c("beta","tau"))
stan_trace(fit, pars=c("beta","tau"))

