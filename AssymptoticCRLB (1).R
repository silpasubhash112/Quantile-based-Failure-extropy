# Load required library
library(stats)

# Parameters for Govindarajulu distribution
sigma <- 1/8
beta <- 1/16
n <- 10000

# Define the quantile function of Govindarajulu distribution
Q_gov <- function(u, sigma, beta) {
  sigma * ((beta + 1) * u^beta - beta * u^(beta + 1))
}

# Derivative of Q(u) for use in quantile density
q_gov <- function(u, sigma, beta) {
  sigma * ((beta + 1) * beta * u^(beta - 1) - beta * (beta + 1) * u^beta)
}

#C_i coefficients (eq. 4.5)
c_vector <- numeric(n)
for (i in 1:(n - 1)) {
  c_vector[i] <- (i - 1) / (n * (n - 1))
}
c_vector[n] <- (2 - n) / (2 * n)

# Generate Monte Carlo samples to approximate CRLB
set.seed(123)
m <- 1000  # number of simulations
U <- matrix(runif(n * m), nrow = m, ncol = n)
X <- t(apply(U, 1, function(u) sort(Q_gov(u, sigma, beta))))

# Compute estimator over all simulations
xi_hat <- apply(X, 1, function(x) sum(c_vector * x))

# Compute empirical variance = CRLB estimate
crlb_estimate <- var(xi_hat)
mean_estimate <- mean(xi_hat)

# Output results
cat("CRLB (estimated variance of estimator):", crlb_estimate, "\n")
cat("Mean of estimator (should match ξ_Q):", mean_estimate, "\n")





# Parameters for Generalized Tukey Lambda
lambda1 <- 1
lambda2 <- 2
lambda3 <- 3
lambda4 <- 4
n <- 1000
m <- 10000  # number of Monte Carlo samples

# Define the quantile function for the generalized Tukey Lambda distribution
Q_tukey <- function(u, lambda1, lambda2, lambda3, lambda4) {
  lambda1 + (u^lambda3 + (1 - u)^lambda4) / lambda2
}

# Compute weights c_i for estimator
c_vector <- numeric(n)
for (i in 1:(n - 1)) {
  c_vector[i] <- (i - 1) / (n * (n - 1))
}
c_vector[n] <- (2 - n) / (2 * n)

# Simulate samples
set.seed(123)
U <- matrix(runif(n * m), nrow = m, ncol = n)
X <- t(apply(U, 1, function(u) sort(Q_tukey(u, lambda1, lambda2, lambda3, lambda4))))

# Estimate the QFE for each sample
xi_hat <- apply(X, 1, function(x) sum(c_vector * x))

# Compute CRLB (empirical variance) and mean
crlb_estimate <- var(xi_hat)
mean_estimate <- mean(xi_hat)

# Output
cat("Tukey Lambda CRLB (estimated variance):", crlb_estimate, "\n")
cat("Mean of estimator (should match ξ_Q):", mean_estimate, "\n")






# Parameters for Generalized Weibull
sigma <- 1
lambda <- 4
alpha <- 3
n <- 1000
m <- 10000

# Quantile function of Generalized Weibull
Q_weibull <- function(u, sigma, lambda, alpha) {
  sigma * (1 - ((1 - u)^lambda) / lambda)^alpha
}

# Weights
c_vector <- numeric(n)
for (i in 1:(n - 1)) {
  c_vector[i] <- (i - 1) / (n * (n - 1))
}
c_vector[n] <- (2 - n) / (2 * n)

# Simulate
set.seed(123)
U <- matrix(runif(n * m), nrow = m, ncol = n)
X <- t(apply(U, 1, function(u) sort(Q_weibull(u, sigma, lambda, alpha))))

# Estimate
xi_hat <- apply(X, 1, function(x) sum(c_vector * x))

# CRLB
crlb_estimate <- var(xi_hat)
mean_estimate <- mean(xi_hat)

# Output
cat("Generalized Weibull CRLB (estimated variance):", crlb_estimate, "\n")
cat("Mean of estimator (should match ξ_Q):", mean_estimate, "\n")
