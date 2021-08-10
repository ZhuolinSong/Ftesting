# simulate the size for testing
type_1 <- function(k, n, m, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        data <- gen.data(deviation = "trigonometric", nsubj = n, r = 0, M = m)
        face.b <- try(bootstrap.face(data, nbs = L, argvals.new = times, ...),
                    silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    c(mean(sim.stats <= 0.05), mean(sim.stats <= 0.1))
}

# simulate the size for testing
type_2 <- function(k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        face.b <- try(bootstrap.face(data, nbs = L, argvals.new = times, ...),
                    silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    c(mean(sim.stats <= 0.05))
}

# simulate the size and runtime for different options
semi_iter <- function(semi.iter = T, k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    l_time <- matrix(NA, nrow = k, ncol = 3)
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        try(l_time[sim.success + 1, ] <- system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, semi.iter=semi.iter, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    list(mean(sim.stats <= 0.05),
        l_time)
}

# simulate the size and runtime for different options
fast.tn <- function(fast.tn = T, k, n, m, dev, r, L = 1000, ...) {
    sim.success <- 0
    sim.stats <- c()
    l_time <- matrix(NA, nrow = k, ncol = 3)
    times <- seq(-1, 1, length.out = 80) # all possible time points
    while (sim.success < k) {
        data <- gen.data(deviation = dev, nsubj = n, r = r, M = m)
        try(l_time[sim.success + 1, ] <- system.time(
            face.b <- bootstrap.face(data, nbs = L, argvals.new = times, semi.iter = F, fast.tn = fast.tn, ...))[1:3],
            silent = T)
        if ("try-error" %in% class(face.b)) {# issue with null fit
            next
        }
        sim.success <- sim.success + 1
        sim.stats <- c(sim.stats, face.b$p)
    }
    list(mean(sim.stats <= 0.05),
        l_time)
}