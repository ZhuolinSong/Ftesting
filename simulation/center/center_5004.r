library(devtools)
devtools::load_all()
RNGkind("L'Ecuyer-CMRG")

seed <- 999983
s_k <- 5000
s_n <- 500
s_m <- 4


center_5004 <- type_1(seed, s_k, s_n, s_m, L = 1000,
                fast.tn = T, semi.iter = F, center.bs = T)
save(center_5004, file = "center_5004.RData")
