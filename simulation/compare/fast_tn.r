#######################
library(devtools)
devtools::load_all()
library(parallel)
RNGkind("L'Ecuyer-CMRG")
seed <- 999983
set.seed(seed)

data(grid)

s_k <- 100
s_n <- 100
s_m <- 4
s_r <- r_grid_quad[2]


l_tn.t <- type_2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000,
            fast.tn = T, semi.iter = F, center.bs = F)
save(l_tn.t, file = "tn_t.RData")

l_tn.f <- type_2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000,
            fast.tn = F, semi.iter = F, center.bs = F)
save(l_tn.f, file = "tn_f.RData")


l_tn.t[[1]]
l_tn.f[[1]]

colMeans(l_tn.t[[2]])
colMeans(l_tn.f[[2]])
