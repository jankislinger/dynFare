require(dynFare)


# Simulate Poisson process ------------------------------------------------

model <- 1
beta <- rep(0,4)
price <- 100
from <- 0
to <- 1

simulate_one_route(model, rep(0.1,4), 100, 0, 1)


# Simulate Markov process -------------------------------------------------

model <- 1
routes <- get_routes(4, 60, c(0.3, 0.4, 0.5))
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- rep(250, 6)
from <- 0; to <- 1
nseats <- 10
init_occupancy <- rep(0, 3)


simulate_all_routes(routes, model, beta, price, from, to, nseats, init_occupancy)


# simulate whole train ----------------------------------------------------

nstations <- 4
routes <- get_routes(nstations, 60, c(0.3, 0.4, 0.5))
model <- 1
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- matrix(250, 2, 6)
time_bp <- 0.5
nseats <- 10

set.seed(1)
simulate_train_demand(routes, model, beta, price, time_bp, nseats, F)
simulate_train_demand(routes, model, beta, price, time_bp, nseats, T)


# generate data -----------------------------------------------------------

beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- matrix(250, 2, 6)
time_bp <- 0.5
ntrains <- 3
nstations <- 4
nseats <- 10


.lambda(0:1)
.Lambda(0,1)
devtools::use_testthat()

get_routes(5, 20, seq(0.2, 0.8, length.out = 4))
