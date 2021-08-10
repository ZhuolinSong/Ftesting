#######################
library(devtools)
devtools::load_all()
data(grid)
set.seed(2021087)

l_tn.t <- fast.tn(fast.tn = T, 100, 100, 4,
        "quadratic", r_grid_quad[2], L = 1000)
save(l_tn.t, file = "tn_t.RData")

set.seed(2021087)
l_tn.f <- fast.tn(fast.tn = F, 100, 100, 4,
        "quadratic", r_grid_quad[2], L = 1000)
save(l_tn.f, file = "tn_f.RData")


l_tn.t[[1]]
l_tn.f[[1]]

colMeans(l_tn.t[[2]])
colMeans(l_tn.f[[2]])
