library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 500
s_m <- 6


sim_type1_5006 <- type_1(seed, s_k, s_n, s_m, L = 1000,
                fast.tn = T, semi.iter = F, center.bs = F)
save(sim_type1_5006, file = "sim_type1_5006.RData")
