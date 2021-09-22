#######################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Example code for implementation of bootstrap, direct, and multivariate tests

# This code gives an example of generating data using the 'gen.data' function, based on
# the simulation study in the main paper. See the function for a description
# of inputs. The function outputs a data.frame of functional data that can be directly
# input into any of the testing function. The second example using the DTI dataset
# from the refund package.
#
# The functions 'bootstrap.test', 'direct.test', and 'multivariate.test' can be called
# to perform the corresponding test for the inputted dataset. Note that the multivariate'
# test can only be applied to dense (balanced) data where all subjects have the same
# time points. See the relevant function for information about changing default settings.
#
# Each testing function takes the two inputs inputs:
# 1) data: data.frame with 3 columns: .value, .index, .id; do not included missing observations
# .value: Functional predictor, does not need to be demeaned
# .index: Observed timepoints, from -1 to 1
# .id: Subject ids, must be in sequentially order beginning from 1, with no missing subjects
# 2) times: vector of all possible timepoints from -1 to 1, in sequential order
#
# Each testing function returns the null and alternative model fits, test statistic,
# and p-value. See the individual function pages for details.
#######################
library(devtools)
devtools::load_all()

# libraries
library(nlme)
library(MASS)
library(refund)
library(matrixcalc)
library(mgcv)
library(Matrix)
library(Bolstad)
library(splines)

type_1(seed = 2021087, k = 1, n = 100, m = 7, L = 1, mixed = T,
knots.option = "quantile", fast.tn = T, trunc.eig = F, #"equally-spaced"
center.bs = F, off_diag = F, no.pen = T)


# Example 1. Simulated data from null model, with 100 subjects and 80 obs/subj
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- gen.data(deviation = "trigonometric", nsubj = 100, r = 0, M = 7, mixed_m = T)
times <- seq(-1, 1, length.out = 80) # all possible time points
m_cov_truth <- 1 + tcrossprod(times) - 0.5 * times - 0.5 * matrix(rep(times, 80), 80, byrow = T)
set.seed(2021085)
# Implement the tests
system.time(face.b <- bootstrap.face(data, nbs = 10, argvals.new = times,
knots.option = "equally_spaced", fast.tn = T, center.bs = F, no.pen = T))

face.b$p
face.b$bs.approx
face.b$Tn
norm(face.b$C.null - m_cov_truth, type = "F")
norm(face.b$C.alt - m_cov_truth, type = "F")
mean((face.b$sigma2 - 1)^2)
# v_rand <- c()
# for(i in 1:10) {
#     v_rand <- c(v_rand, rnorm(1))
# }
# v_rand
# Comparison methods
system.time(fit.b <- bootstrap.test(data, times, nbs = 10)) # pilot bootstrap test with 10 resamples
# fit.b<-bootstrap.test(data,times,nbs=1000) # full bootstrap test with 1000 resamples
fit.d <- direct.test(data, times) # direct test
fit.m <- multivariate.test(data, times) # multivariate test (note: data must be dense, ie M=80)


## deviation from null
f_devnull <- function (r, deviation = "trigonometric", n=1e5) {
    grid <- seq(-1, 1, length.out = n)
    var_z <- switch(
        deviation,
        "quadratic" = r^2 * grid^4,
        "trigonometric" = r^2 * (sin(2 * pi * grid) ^2 + sin(4 * pi * grid) ^2)
    )
    mean(var_z / (1 + grid^2 + var_z))
}

dev_grid <- seq(0, 0.99, length.out = 5)
r_grid_quad <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x, "quadratic") - t,
                c(0, 1e5))$root})
r_grid_trig <- sapply(dev_grid, function(t) {
                uniroot(function(x) f_devnull(x) - t,
                c(0, 1e5))$root})

# Example 2. DTI data from "refund" package
nsub.full <- 382
times <- round(seq(-1, 1, length.out = 93), digits = 5)

data.full <- data.frame(.value = c(t(DTI$cca)), .index = rep(times, nsub.full), .id = rep(DTI$ID, each = 93), case = rep(DTI$case, each = 93), visit = rep(DTI$visit.time, each = 93))
data.full <- subset(data.full, case == 1 & visit == 0) # limit to patients w/ MS, baseline only
data.full <- data.full[complete.cases(data.full$.value), ] # no missing data

# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
new.subj <- 1
for (i in unique(data.full$.id)) {
    subj <- subset(data.full, .id == i)
    if (nrow(subj) < 93) {
        delete.subj <- c(delete.subj, i)
    }
    else {
        subj$.id <- new.subj
        data <- rbind(data, subj)
        new.subj <- new.subj + 1
    }
}

# Implement the tests
fit.b <- bootstrap.test(data, times, nbs = 10) # pilot bootstrap test with 10 resamples
# fit.b<-bootstrap.test(data,times,nbs=1000) # full bootstrap test with 1000 resamples
fit.d <- direct.test(data, times) # direct test
fit.m <- multivariate.test(data, times) # multivariate test (note: data must be dense)




# CD4 count
times <- round(seq(-1, 1, length.out = ncol(cd4)), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 1) {
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
mean(count)
min(count)
max(count)

# No log-transform stepface
data <- data.frame(y = value, argvals = index, subj = id)
system.time(face.b1 <- bootstrap.face(data, nbs = 1000, argvals.new = times,
fast.tn = T, trunc.eig = T, center.bs = T, off_diag = T, lambda = 0))

face.b1$p
# log-transform stepface
data <- data.frame(y = log(value), argvals = index, subj = id)
system.time(face.b2 <- bootstrap.face(data, nbs = 1000, argvals.new = times,
fast.tn = F, trunc.eig = T, center.bs = F, off_diag = F, lambda = NULL))
face.b2$p