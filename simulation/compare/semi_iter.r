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

l_semi.t <- type_2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000,
            fast.tn = T, semi.iter = T, center.bs = F)
save(l_semi.t, file = "semi_t.RData")


l_semi.f <- type_2(seed, s_k, s_n, s_m, "quadratic", s_r, L = 1000,
            fast.tn = T, semi.iter = F, center.bs = F)
save(l_semi.f, file = "semi_f.RData")


load("semi_t.RData")
load("semi_f.RData")

# power
l_semi.t[[1]]
l_semi.f[[1]]
# mse for sigma2
l_semi.t[[2]]
l_semi.f[[2]]
# runtime
rowMeans(matrix(unlist(l_semi.t[[3]]), nrow = 3))
rowMeans(matrix(unlist(l_semi.f[[3]]), nrow = 3))
