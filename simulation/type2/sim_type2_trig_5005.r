library(devtools)
library(parallel)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
seed <- 999983
set.seed(seed)

data(grid)

s_k <- 1000
s_n <- 500
s_m <- 5

sim_type2_trig_5005 <- mclapply(r_grid_trig[-1], r_loop <- function(s_r) {
    type_2(seed, s_k, s_n, s_m, "trigonometric", s_r)
}, mc.cores = 4)

save(sim_type2_trig_5005, file = "sim_type2_trig_5005.RData")