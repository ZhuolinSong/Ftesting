###############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Generate functional data with covariance from a quadratic polynomial + deviation
# Updated: Aug 4, 2018

# Inputs
# seed: starting seed
# deviation: 'quadratic' or 'trigonometric' deviation from the null covariance
# nsubj: number of subjects
# r: magnitude of deviation from the null (r=0 is the null model)
# M: # observations per subject, max = 80
# error.var: magnitude of error variance

# Outputs: data.frame of functional data observations
# y: functional predictor, X_i(t)
# argvals: timepoint, t
# subj: subject id


gen.data <- function(deviation = "quadratic",
                     nsubj = 100,
                     r = 0,
                     M = 80,
                     error.var = 1, mixed_m = T) {
  argvals <- seq(-1, 1, length.out = 80) # possible time pts
  data <- NULL
  error.sd <- sqrt(error.var)
  rho <- -0.5 # cov b/t r.intercept and r.slope
  LMEmat <- matrix(c(1, rho, rho, 1), nrow = 2) # cov matrix for Linear portion
  b.mat <- matrix(rnorm(2 * nsubj), ncol = 2) %*% chol(LMEmat)
  t.mat <- matrix(cbind(rep(1, length(argvals)), argvals), ncol = 2)
  mu <- 0

  if (deviation == "quadratic") {
    y <- mu + c(t.mat %*% t(b.mat) + matrix(argvals^2, ncol = 1) %*% matrix(rnorm(nsubj, sd = sqrt(r)), nrow = 1)) + rnorm(nsubj * length(argvals), sd = error.sd)
  }
  if (deviation == "trigonometric") {
    lambda.mat <- diag(sqrt(c(1, 1)))
    psi1 <- sin(2 * pi * argvals)
    psi2 <- sin(4 * pi * argvals)
    psi <- cbind(psi1, psi2)
    y <- mu + c(t.mat %*% t(b.mat) + sqrt(r) * psi %*% lambda.mat %*% matrix(rnorm(2 * nsubj), nrow = 2)) + rnorm(nsubj * length(argvals), sd = error.sd)
  }
  full.data <- data.frame(y = y, argvals = rep(argvals, nsubj), subj = rep(c(1:nsubj), each = length(argvals)))
  # vector of M_i
  if (mixed_m && M <= 78) {
    v_mi <- sample(rep((M - 2):(M + 2), ceiling(nsubj / 5)))
  } else {
    v_mi <- rep(M, nsubj)
  }
  obs.ind <- lapply(c(1:nsubj), function(x) sort(sample(c(1:length(argvals)), v_mi[x], replace = F)) + (x - 1) * length(argvals))

  return(full.data[unlist(obs.ind), ])
}
