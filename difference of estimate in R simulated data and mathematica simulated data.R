# Load required library
library(stats)
# Parameters for Generalized Weibull
sigma <- 1
lambda <- 5
alpha <- 6
n <- 50
m <- 1000

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
cat("Mean of estimator (should match ??_Q):", mean_estimate, "\n")

# Parameters for Generalized Weibull
sigma <- 1
lambda <- 5
alpha <- 6
n <- 50
m <- 1000

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
# U <- matrix(runif(n * m), nrow = m, ncol = n)
# X <- t(apply(U, 1, function(u) sort(Q_weibull(u, sigma, lambda, alpha))))
X=read.csv(file.choose())
# Estimate
xi_hat <- apply(X, 1, function(x) sum(c_vector * x))

# CRLB
crlb_estimate <- var(xi_hat)
mean_estimate <- mean(xi_hat)

# Output
cat("Generalized Weibull CRLB (estimated variance):", crlb_estimate, "\n")
cat("Mean of estimator (should match ??_Q):", mean_estimate, "\n")
