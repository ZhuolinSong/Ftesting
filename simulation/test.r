library(devtools)
devtools::load_all()

data <- gen.data(deviation = "trigonometric", nsubj = 100, r = 0, M = 10)
times <- seq(-1, 1, length.out = 80) # all possible time points

# Implement the tests
system.time(face.b <- bootstrap.face(data, nbs = 10, argvals.new = times))

face.b$p
face.b$Tn
face.b$bs.approx