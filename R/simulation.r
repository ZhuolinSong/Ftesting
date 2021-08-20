# simulate the size for testing
type_1 <- function(seed = 2021087, k, n, m, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    sim.c0 <- c() 
    sim.calt <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    cov_truth <- 1 + tcrossprod(times) - 0.5 * times - 0.5 * matrix(rep(times, 80), 80, byrow = T)

    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = "trigonometric", nsubj = n, r = 0, M = m)
        l_time[[sim.success + 1]] <- try(system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(l_time[[sim.success + 1]])) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
        sim.sigma2 <- c(sim.sigma2, face.b$sigma2)
        sim.c0 <- c(sim.c0, norm(face.b$C.null - cov_truth, type = "F"))
        sim.calt <- c(sim.calt, norm(face.b$C.alt - cov_truth, type = "F"))
    }

    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        mean((sim.sigma2 - 1)^2), mean(sim.c0), mean(sim.calt),
        l_time)
}

# simulate the size for testing
type_2 <- function(seed = 2021087, k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        l_time[[sim.success + 1]] <- try(system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(l_time[[sim.success + 1]])) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
        sim.sigma2 <- c(sim.sigma2, face.b$sigma2)
    }
    list(mean(sim.stats <= 0.05),
        mean((sim.sigma2 - 1)^2),
        l_time)
}
