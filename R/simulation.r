# simulate the size for testing
type_1 <- function(seed = 2021087, k, n, m, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    sim.sigma2 <- c()
    l_time <- list()
    times <- seq(-1, 1, length.out = 80) # all possible time points
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
    }

    list(c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1)),
        mean((sim.sigma2 - 1)^2),
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

# simulate the size and runtime for different options
semi_iter <- function(seed = 2021087, semi.iter = T, k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    l_time <- matrix(NA, nrow = k, ncol = 3)
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        try(l_time[sim.success + 1, ] <- system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, semi.iter=semi.iter, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    list(mean(sim.stats <= 0.05),
        l_time)
}

# simulate the size and runtime for different options
fast.tn <- function(seed = 2021087, fast.tn = T, k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    l_time <- matrix(NA, nrow = k, ncol = 3)
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        set.seed(seed + sim.success)
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        try(l_time[sim.success + 1, ] <- system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, semi.iter = F, fast.tn = fast.tn, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            seed <- seed + 1
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    list(mean(sim.stats <= 0.05),
        l_time)
}