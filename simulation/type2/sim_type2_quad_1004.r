library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
data(grid)

seed <- 999983
s_k <- 1000
s_n <- 100
s_m <- 4

sim_type2_quad_1004 <-  mclapply(r_grid_quad[-1], r_loop <- function(s_r) {
    type_2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000,
            fast.tn = F, semi.iter = F, center.bs = T)
}, mc.cores = 2)

save(sim_type2_quad_1004, file = "sim_type2_quad_1004.RData")