#######################
library(devtools)
devtools::load_all()
data(grid)
set.seed(2021087)

l_semi.t <- semi_iter(semi.iter = T, 100, 100, 4,
            "quadratic", r_grid_quad[2], L = 1000)
save(l_semi.t, file = "semi_t.RData")

set.seed(2021087)
l_semi.f <- semi_iter(semi.iter = F, 100, 100, 4,
            "quadratic", r_grid_quad[2], L = 1000)
save(l_semi.f, file = "semi_f.RData")


l_semi.t[[1]]
l_semi.f[[1]]

colMeans(l_semi.t[[2]])
colMeans(l_semi.f[[2]])
