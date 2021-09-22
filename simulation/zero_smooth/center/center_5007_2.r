library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

s_seed <- 999983 + 2000
s_k <- 1000
s_n <- 500
s_m <- 7
v_seed <- c(s_seed, s_seed + s_k)

center_5007_2 <- mclapply(v_seed, seed_loop <- function(seed) {
    type_1(seed, s_k / 2, s_n, s_m, L = 1000,
                fast.tn = T, semi.iter = F, center.bs = T, no.pen = T)
}, mc.cores = 2)

save(center_5007_2, file = "center_5007_2.RData")