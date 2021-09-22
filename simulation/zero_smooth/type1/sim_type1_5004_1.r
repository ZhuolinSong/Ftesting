library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983
s_k <- 1000
s_n <- 500
s_m <- 4
v_seed <- c(s_seed, s_seed + s_k)

sim_type1_5004_1 <- mclapply(v_seed, seed_loop <- function(seed) {
    type_1(seed, s_k / 2, s_n, s_m, L = 1000,
                fast.tn = T, semi.iter = F, center.bs = F, no.pen = T)
}, mc.cores = 2)

save(sim_type1_5004_1, file = "sim_type1_5004_1.RData")