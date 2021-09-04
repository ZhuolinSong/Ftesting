library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 500
s_m <- 4

sim_type2_trig_5004 <- mclapply(r_grid_trig[-1], r_loop <- function(s_r) {
    type_2(seed, s_k, s_n, s_m, "trigonometric", s_r, L = 1000,
            fast.tn = F, semi.iter = F, center.bs = T)
}, mc.cores = 4)

save(sim_type2_trig_5004, file = "sim_type2_trig_5004.RData")