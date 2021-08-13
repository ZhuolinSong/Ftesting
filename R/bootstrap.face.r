#' bootstrap.face
#'
#' Apply the bootstrap test for testing a quadratic polynomial covariance
#'
#' @param data  "data frame with three arguments:
#'                  (1) "argvals": observation times;
#'                  (2) "subj": subject indices;
#'                  (3) "y": values of observations;
#'  Note that: we only handle complete data, so missing values are not allowed at this moment
#' @param nbs number of bootstrap samples, default = 1000
#' @param argvals.new  "argvals.new" if we want the estimated covariance function at "argvals.new"; if NULL,
#' then 100 equidistant points in the range of "argvals" in "data"
#' @param fast.tn indicator whether to use fast tn calculation
#' @param semi.iter indicator whether to use semi_iterative for bootstrap
#' @param tune.bs indicator whether to use tunning for bootstrap
#' @param center   "center" means if we want to compute population mean
#' @param knots   number of interior knots for B-spline basis functions to be used;
#' @param p the degrees of B-splines; defaults to 3.
#' @param m the order of differencing penalty; defaults to 2.
#' @param lambda  the value of the smoothing parameter for covariance smoothing; defaults to NULL.
#' @param lambda_mean the value of the smoothing parameter for mean smoothing; defaults to NULL.
#' @param search.length the number of equidistant (log scale) smoothing parameters to search; defaults to 14.
#' @param upper,lower bounds for log smoothing parameter for first step of estimation; defaults are -3 and 10, respectively.
#' @param pve Defaults 0.99. To select the number of eigenvalues by percentage of variance.
#'
#'
#' @import nlme
#' @return an object "bootstrap.face" contain:
#'  fit.alt: Alternative model fit (functional principal components analysis)
#'  fit.null: Null model fit (linear random effects)
#'  C.alt: Covariance matrix under alternative model
#'  C.null: Covariance matrix under null model
#'  tn: Test statistic
#'  p: p-value for test statistic based on the bs.approx
#'  bs.approx: List of values from the null distribution of Tn
#'
#' @references modified from face.sparse.inner from face package
#' and bootstrap.test.R written by Stephanie
bootstrap.face <- function(data, nbs = 1000, argvals.new = NULL,
                          fast.tn = T, semi.iter = T, center.bs = F,
                          tune.bs=F, center = TRUE,
                           knots = 7, knots.option = "equally-spaced",
                           p = 3, m = 2, lambda = NULL, lambda_mean = NULL,
                           search.length = 14,
                           lower = -3, upper = 10,
                           pve = 0.99) {
  #########################
  #### step 0: read in data
  #########################
  if (!fast.tn && semi.iter) {
    warning("semi.iter won't truncate the estimated covariance matrix!")
  }
  check.data(data)
  nb <- knots + p

  y <- data$y
  t <- data$argvals
  subj <- data$subj
  tnew <- argvals.new
  if (is.null(tnew)) tnew <- seq(min(t), max(t), length = 100)

  fit_mean <- NULL

  knots.initial <- knots
  #########################
  #### step 1: Test statistics
  #########################

  ###### a. Initialize C, X, Q
  r <- y
  if (center) {
    #fit_mean <- pspline(data, argvals.new = tnew, knots = knots.initial, lambda = lambda_mean)
    fit_mean <- gam(as.vector(y) ~ s(t, k = nb))
    r <- y - fit_mean$fitted.values
  }

  raw <- raw.construct(data.frame("argvals" = t, "subj" = subj, "y" = as.vector(r)))
  C <- raw$C  # C
  st <- raw$st
  N <- raw$N
  N2 <- raw$N2
  W <- raw$W
  n0 <- raw$n0
  # indicator where ti = tj
  delta <- Matrix((st[, 1] == st[, 2]) * 1) # sparse
  delta <- c(as.matrix(delta))

  knots <- construct.knots(t, knots, knots.option, p)
  ## construct design & penalty on column dimension
  List <- pspline.setting(st[, 1], knots = knots, p, m, type = "simple", knots.option = knots.option)
  B1 <- List$B
  B1 <- Matrix(B1)
  DtD <- List$P

  B2 <- spline.des(knots = knots, x = st[, 2], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  c <- dim(B1)[2]
  c2 <- c * (c + 1) / 2
  ## combine tensor product design
  B <- Matrix(t(KhatriRao(Matrix(t(B2)), Matrix(t(B1)))))
  G <- Matrix(duplication.matrix(c))
  ### double the weight for off-diagonal
  # v_idx <- which(delta == 0)
  # B[v_idx, ] <- sqrt(2) * B[v_idx, ]
  B <-  B * (sqrt(2) * (1 - delta) + delta)

  # BtWB <- matrix(0, nrow = c^2, ncol = c^2)
  # Wdelta <- c()
  # WC <- c()
  # for (i in 1:n0) {
  #   # select combos for object i
  #   seq <- (sum(N2[1:i]) - N2[i] + 1):(sum(N2[1:i]))
  #   B3 <- Matrix(matrix(B[seq, ], nrow = length(seq)))
  #   BtWB <- BtWB + crossprod(B3, B3)
  #   Wdelta <- c(Wdelta, as.matrix(delta[seq]))
  #   WC <- c(WC, as.matrix(C[seq]))
  # }
  BtB <- crossprod(B)
  BG <- B %*% G # sparse

  GtBtBG <- crossprod(G, BtB %*% G)
  detde <- crossprod(delta) # detWde = sum(delta)
  GtBtdelta <- crossprod(BG, delta)
  XtX <- rbind(cbind(GtBtBG, GtBtdelta), cbind(t(GtBtdelta), detde))
  ## design X
  X <- cbind(BG, delta)
  #XtX <- crossprod(X)
  ## penalty
  P <- crossprod(G, Matrix(suppressMessages(kronecker(diag(c), DtD)))) %*% G
  Q <- bdiag(P, 0)

  ###### b. Pre-calculate s, F, B^*, Li, g, f, G (2 eigens)
  eSig <- eigen(XtX, symmetric = TRUE)
  V <- eSig$vectors
  E <- eSig$values
  E <- E + 0.000001 * max(E)
  Sigi_sqrt <- matrix.multiply(V, 1 / sqrt(E)) %*% t(V)

  tUQU <- crossprod(Sigi_sqrt, (Q %*% Sigi_sqrt))
  Esig <- eigen(tUQU, symmetric = TRUE)
  # s, f, F(m_F here)
  U <- Esig$vectors
  s <- Esig$values
  A0 <- Sigi_sqrt %*% U
  m_F <- as.matrix(X %*% A0)

  m_FtF <- crossprod(m_F) # FTF
  f <- crossprod(m_F, C) # f=FTC

  c2 <- c2 + 1
  g <- rep(0, c2)
  G1 <- matrix(0, c2, c2)
  mat_list <- list()
  # G(G1 here), g, Li
  for (i in 1:n0) {
    seq <- (sum(N2[1:i]) - N2[i] + 1):(sum(N2[1:i]))
    Fi <- matrix(m_F[seq, ], nrow = length(seq))
    Li <- crossprod(Fi)#Li <- FitFi

    fi <- crossprod(Fi, C[seq]) # t(Fi)Ci

    g <- g + fi * fi
    G1 <- G1 + Li * (fi %*% t(f))

    LList <- list()
    LList[[1]] <- Li
    #LList[[2]] <- Li
    mat_list[[i]] <- LList
  }
  # B^*
  st.construct <- function(tnew) {
    m1 <- length(tnew)
    if (m1 > 1) {
      st <- cbind(vech(kronecker(tnew, t(rep(1, m1)))),
                  vech(kronecker(rep(1, m1), t(tnew))))
    } else if (m1 == 1) {
      st <- rbind(st, c(tnew, tnew))
    }
    st
  }
  stnew <- st.construct(tnew)
  Bnew1 <- spline.des(knots = knots, x = stnew[, 1], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  Bnew2 <- spline.des(knots = knots, x = stnew[, 2], ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design
  Bstar <- Matrix(t(KhatriRao(Matrix(t(Bnew2)), Matrix(t(Bnew1)))))

  delta_star <- which(stnew[, 1] != stnew[, 2])
  Xstar <- Bstar %*% G
  Xstar[delta_star, ] <- sqrt(2) * Xstar[delta_star, ]
  Bnew <- spline.des(knots = knots, x = tnew, ord = p + 1, outer.ok = TRUE, sparse = TRUE)$design

  ###### c. Null estimate Ctilde0, f0 (2 eigens)
  fitNull <- function(data) {
     try(nlme::lme(y ~ 1, random = list(subj = pdSymm(~ 1 + argvals)),
          data = data), silent = T)
  }
  fit.null <- fitNull(data.frame("argvals" = t, "subj" = subj, "y" = as.vector(r)))
  if ("try-error" %in% class(fit.null)) {# issue with null fit
    stop(fit.null)
  }
  calc.R0 <- function(fit.null, times) {
    var.mat <- VarCorr(fit.null)
    sigsq0 <- as.numeric(var.mat[1, 1])
    sigsq1 <- as.numeric(var.mat[2, 1])
    cov01 <- as.numeric(var.mat[2, 3]) * sqrt(sigsq0 * sigsq1) # corr->cov
    Rbar0 <- sigsq0 + cov01 * (times[, 1] + times[, 2]) + sigsq1 * (times[, 1] * times[, 2])
    list(Rbar0 = Rbar0, coef.null = c(sigsq0, cov01, sigsq1))
  }

  Rbar0.fit <- calc.R0(fit.null, st)
  C0 <- Rbar0.fit$Rbar0

  ###### d. tunning(s, F, F^*, Li, g, f, G) to get lambda^*
  Lambda <- seq(lower, upper, length = search.length)
  Gcv <- 0 * Lambda
  gcv <- function(x) {
    lambda <- exp(x)
    d <- 1 / (1 + lambda * s)
    f_d <- f * d
    cv0 <- -2 * sum(f_d * f)
    cv1 <- sum(f_d * (m_FtF %*% f_d))
    cv2 <- 2 * sum(d * g)
    cv3 <- -4 * sum(d * (G1 %*% d))
    cv4 <- sum(unlist(sapply(mat_list, function(x) {
      a <- x[[1]] %*% f_d
      2 * sum(a * a * d)
    })))
    cv <- cv0 + cv1 + cv2 + cv3 + cv4
    return(cv)
  }
  if (is.null(lambda)) {
    Lambda <- seq(lower, upper, length = search.length) # construct lambda grid
    Length <- length(Lambda)
    Gcv <- rep(0, Length)
    for (i in 1:Length) {
      Gcv[i] <- gcv(Lambda[i])
    }
    i0 <- which.min(Gcv)
    lambda <- exp(Lambda[i0]) # lambda^*
  }

  ###### e. calculate estimated covariance function and test statistics
  m_est <- matrix.multiply(A0, 1 / (1 + lambda * s)) %*% t(m_F)

  trun_mat <- function(C) {
    alpha <- m_est %*% C
    Theta <- G %*% alpha[1:(c2 - 1)]
    Theta <- matrix(Theta, c, c)
    sigma2 <- alpha[c2]
    if (sigma2 <= 0.000001) {
      warning("error variance cannot be non-positive, reset to 1e-6!")
      sigma2 <- 0.000001
    }
    # make sure Theta positive definite(2 eigens)
    Eigen <- eigen(Theta, symmetric = TRUE)
    Eigen$values[Eigen$values < 0] <- 0
    npc <- sum(Eigen$values > 0) # which.max(cumsum(Eigen$values)/sum(Eigen$values)>pve)[1]
    if (npc > 1) {
      Theta <- matrix.multiply(Eigen$vectors[, 1:npc], Eigen$values[1:npc]) %*% t(Eigen$vectors[, 1:npc])
    }
    if (npc == 1) {
      Theta <- Eigen$values[1] * suppressMessages(kronecker(Eigen$vectors[, 1], t(Eigen$vectors[, 1])))
    }
    #Eigen <- eigen(Theta, symmetric = TRUE)
    list(C = as.matrix(tcrossprod(Bnew %*% Matrix(Theta), Bnew)),
         sigma2 = sigma2)
  }

  l_mat <- trun_mat(C)
  sigsq <- l_mat$sigma2
  C.alt <- l_mat$C
  C.null <- trun_mat(C0)$C

  if (fast.tn) {
    Tn <- (m_est[-c2, ] %*% (C0 - C))
    Tn <- norm(Xstar %*% Tn, type = "F")
  } else {
    Tn <- norm(C.alt - C.null, type = "F")
  }


  #########################
  #### step 2: Bootstrap
  #########################
  raw.C <- function(data) {
    y <- data$y
    subj <- data$subj
    subj_unique <- unique(subj)
    n <- length(subj_unique)
    C <- c()
    for (i in 1:n) {
      r1 <- y[subj == subj_unique[i]]
      m1 <- length(r1)
      if (m1 > 1) {
        C <- c(C, vech(tcrossprod(r1)))
      } ## for if(m1>1)
      if (m1 == 1) {
        C <- c(C, r1^2)
      }
    } ## for i
    C
  }

  bs.stats <- c()
  bs.success <- 0
  if (center.bs) {
      mean.bs <- fit_mean$fitted.values
  } else {
      mean.bs <- 0
  }

  if (semi.iter) {
    C0.bs <- matrix(NA, nrow = length(C), ncol = 0)
    C.bs  <- C0.bs
    y.bs <- resample(data, mean.bs, Rbar0.fit$coef.null, sigsq, L = nbs)

    while (bs.success < nbs) { #(0.15s)

      ###### a. generate Y_ij^(l) (0.01s)## fit_mean$fitted.values
      s_iterator <- bs.success + 1
      if (ncol(y.bs) < s_iterator) {
        y.bs <- cbind(y.bs, resample(data, mean.bs, Rbar0.fit$coef.null, sigsq, L = 5))
      }
      y <- y.bs[, s_iterator]

      ###### b. center Y_ij^(l) (0.01s)
      r <- y
      if (center && center.bs) {
        #fit_mean.bs <- pspline(this.bs, argvals.new = tnew, knots = knots.initial, lambda = lambda_mean)
        fit_mean.bs <- mgcv::gam(as.vector(y) ~ s(t, k = nb))
        r <- y - fit_mean.bs$fitted.values
      }
      data.demean.bs <- data.frame(
        "argvals" = t,
        "subj" = subj, "y" = as.vector(r)
      )

      ###### c. Initialize C0^(l) (0.08s)
      fit.null.bs <- fitNull(data.demean.bs) # null fit
      if ("try-error" %in% class(fit.null.bs)) { # issue with null fit
        y.bs <- y.bs[, -s_iterator]
        next # if problem
      }
      Rbar0.fit.bs <- calc.R0(fit.null.bs, st)
      C0.bs <- cbind(C0.bs, Rbar0.fit.bs$Rbar0)

      ###### d. Initialize C^(l) (0.05s)
      C.bs <- cbind(C.bs, raw.C(data.demean.bs))
      bs.success <- bs.success + 1

    }
# ptm <- proc.time()
    # Compute delta.bs (0.17 s)
    delta.bs <- m_est[-c2, ] %*% (C0.bs - C.bs)
# print(proc.time() - ptm)
    # compute bs statistics (0 s)
    bs.stats <- (Xstar %*% delta.bs) ^ 2
    bs.stats <- sqrt(colSums(bs.stats))

  } else {
    while (bs.success < nbs) { #(0.25s)

      ###### a. generate Y_ij^(l) (0.05s)
      y <- c(resample(data, mean.bs, Rbar0.fit$coef.null, sigsq))

      ###### b. center Y_ij^(l) (0.05s)
      r <- y
      if (center && center.bs) {
        #fit_mean.bs <- pspline(this.bs, argvals.new = tnew, knots = knots.initial, lambda = lambda_mean)
        fit_mean.bs <- mgcv::gam(as.vector(y) ~ s(t, k = nb))
        r <- y - fit_mean.bs$fitted.values
      }
      data.demean.bs <- data.frame(
        "argvals" = t,
        "subj" = subj, "y" = as.vector(r)
      )

      ###### c. Initialize C0^(l) (0.05s)
      fit.null.bs <- fitNull(data.demean.bs) # null fit
      if ("try-error" %in% class(fit.null.bs)) { # issue with null fit
        next # if problem
      }
      Rbar0.fit.bs <- calc.R0(fit.null.bs, st)
      C0.bs <- Rbar0.fit.bs$Rbar0

      ###### d. Initialize C^(l) (0.05s)
      C.bs <- raw.C(data.demean.bs) #(0.05s)

#ptm <- proc.time()
      ###### e. tunning(f, g, G) -> lambda_a^l (most inefficient 1s)
      if (tune.bs) {
        ## f, g, G(G1 here), Li (0.1s)
        f <- crossprod(m_F, C.bs) # f=FTC
        for (i in 1:n0) {
          seq <- (sum(N2[1:i]) - N2[i] + 1):(sum(N2[1:i]))
          Fi <- matrix(m_F[seq, ], nrow = length(seq))
          Li <- mat_list[[i]][[1]]

          fi <- crossprod(Fi, C.bs[seq]) # t(Fi)Ci
          g <- g + fi * fi
          G1 <- G1 + Li * (fi %*% t(f))
        }

        # tunning step (0.02 s)
        Gcv.bs <- rep(0, search.length)
        for (i in 1:search.length) {
          Gcv.bs[i] <- gcv(Lambda[i])
        }
        i0 <- which.min(Gcv.bs)
        lambda.bs <- exp(Lambda[i0]) # lambda^*

        # recalculate estimation matrix (0.8s)
        m_est <- matrix.multiply(A0, 1 / (1 + lambda.bs * s)) %*% t(m_F)
      }
#print(proc.time() - ptm)
      ###### f. calculate estimated covariance function and test statistics
      # (0.05 s)

      if (fast.tn) {
        Tn.bs <- m_est[-c2, ] %*% (C0.bs - C.bs)
        Tn.bs <- norm(Xstar %*% Tn.bs, type = "F")
      } else {
        C.alt.bs <- trun_mat(C.bs)$C
        C.null.bs <- trun_mat(C0.bs)$C
        Tn.bs <- norm(C.alt.bs - C.null.bs, type = "F")
      }

      bs.stats <- c(bs.stats, Tn.bs) # save bs stats
      bs.success <- bs.success + 1
    }
  }


  #########################
  #### step 3: P-values
  #########################
  p.bs <- function(stat, bs.stat) {
    p <- mean(stat <= bs.stat)
    list(p = p, mean = mean(bs.stat), var = var(bs.stat))
  }
  Tn.stats <- p.bs(Tn, unlist(bs.stats))


  list(
    mu = fit_mean$fitted.values,
    C.alt = C.alt,
    C.null = C.null,
    sigma2 = sigsq,
    Tn = Tn, p = Tn.stats$p, p.var = Tn.stats$var,
    bs.approx = bs.stats
  )
}




############################
# Written by: Stephanie Chen (stchen3@ncsu.edu)
# Purpose: Generate bootstrap resamples
# Updated: Aug 4, 2018
# Modified on July 30, 2021

resample <- function(data, mu, coef.null, sigsq, L = 1) {
  nsubj <- length(unique(data$subj))
  cov.mat <- matrix(c(coef.null[1], coef.null[2], coef.null[2], coef.null[3]), nrow = 2)
  b.mat <- matrix(rnorm(2 * nsubj * L), ncol = 2) %*% chol(cov.mat) # random effects

  # r.slope & r.int by subject
  par.random <- sapply(seq_len(L), function(i) {
        s_offset <- (i-1) * nsubj
        sapply(seq_len(nsubj), function(x) {
            b.mat[s_offset + x, 1] + b.mat[s_offset + x, 2] * subset(data, subj == x)$argvals
        })
    })

  if (is.list(par.random)) { # if a list
    par.random <- unlist(par.random)
  }
  # mean.null + null.par r.int & r.slope*t + alt.par residual
  matrix(mu + par.random + rnorm(nrow(data) * L, sd = sqrt(sigsq)), ncol = L)
}

# resample(data, 0, c(1,0,1), 2, L = 2)
# sapply(1:2, function(i) {
#         sapply(6:8, function(x) {
#             i*x
#         })
#     })