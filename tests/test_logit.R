library(rstan)

beta <- c(-1, 0, 1)
tau <- 1
l <- 5
p <- 3
betas <- MASS::mvrnorm(n = l, mu = beta, Sigma = (tau**2) * diag(rep(1, 3)))

# generate data 
n <- 500
group <- sample(1:l, size = n, replace = TRUE, prob= rep(1/l, l)) 

X <- MASS::mvrnorm(n = n, mu = rep(1,3), Sigma = diag(rep(1, 3)) + matrix(0.0, nrow = 3, ncol = 3))

X
betas
lp <- diag(X%*% t(betas[group, ]))
lp
Y <- rbinom(n = n, size = 1, prob = plogis(lp))

length(Y[group == 10])
y <- Y[group == 5]
x <- X[group == 5, ]
y
x

Y_H <- Y[group != 5]
X_H <- X[group != 5, ]

Y_H
X_H



stan_data1 <- list(n = n, p = 3, l = l, X = X, Y = Y, group = group, var_beta = 10, 
                   var_tau = 5)

sm1 <- readRDS(file = "./stan/binomial_bhm_compiled.rds")

fit1 <- sampling(sm1,
                data = stan_data1,
                chains = 4,
                warmup = 3000,
                iter = 5000, 
                refresh = 0)


fit1


# Compile the model
sm <- stan_model("binomial_napp.stan", verbose = FALSE)
# 
# # Save compiled model to an RDS file
saveRDS(sm, file = "binomial_napp_compiled.rds")

#compute sufficient statistics
data_fits <- list(
  coef <- list(),
  cov <- list()
)

for(k in 1:l){
  fit <- glm(Y[group == k] ~ X[group ==k, ] + 0, family = binomial)
  data_fits$coef[[k]] <- coef(fit)
  data_fits$cov[[k]] <- vcov(fit)
}


cov_hat <- array(NA, dim = c(l, p, p))
for (j in 1:l) {
  cov_hat[j,,] <- data_fits$cov[[j]]
}

cov_hat

data_fits

theta_hat <- array(NA, dim = c(l, p))
for (j in 1:l) {
  theta_hat[j,] <- data_fits$coef[[j]]
}

data_fits$coef[[1]]
theta_hat

stan_data2 <- list(
  p = 3, 
  l = l, 
  theta_hat = theta_hat,
  cov_hat = cov_hat
  )

sm2 <- readRDS(file = "approx_hier_logit_compiled.rds")

theta_hat
data_fits$coef

betas
fit2 <- sampling(sm2,
                 data = stan_data2,
                 chains = 4,
                 warmup = 3000,
                 iter = 5000,
                 seed = 123
)
fit2

fit1
