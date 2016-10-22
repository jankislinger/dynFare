require(dynFare)


# Simulate Poisson process ------------------------------------------------

model <- 1
beta <- rep(0,4)
price <- 100
from <- 0
to <- 1

simulate_poisson_process(1, rep(0.1,4), 100, 0, 1)


# Simulate Markov process -------------------------------------------------

model <- 1
routes <- getRoutes(4, 60, c(0.3, 0.4, 0.5))
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- rep(250, 6)
from <- 0; to <- 1
nseats <- 10
init_occupancy <- rep(0, 3)


simulate_markov_process(model, routes, beta, price,
                        from, to, nseats, init_occupancy)

.lambda(0:1)
.Lambda(0,1)
devtools::use_testthat()

