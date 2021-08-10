library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")
seed <- 999983
set.seed(seed)


s_k <- 5000
s_n <- 500
s_m <- 5


sim_type1_5005 <- type_1(s_k, s_n, s_m, fast.tn=T, semi.iter = F)
save(sim_type1_5005, file = "sim_type1_5005.RData")
