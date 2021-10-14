library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

sim_type1_5007_2 <- mclapply(v_seed, seed_loop <- function(seed) {
    type_1(seed, s_k / 2, s_n, s_m, L = 1000,
                trunc.eig = 1, semi.iter = F, center.bs = F)
}, mc.cores = 2)

save(sim_type1_5007_2, file = "sim_type1_5007_2.RData")