library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
s_seed <- 999983
s_k <- 5000
s_n <- 100
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

center_1004 <- mclapply(v_seed, seed_loop <- function(seed) {
    type_1(seed, s_k / 2, s_n, s_m, L = 1000,
                trunc.eig = 1, semi.iter = F, center.bs = T)
}, mc.cores = 2)

save(center_1004, file = "center_1004.RData")
