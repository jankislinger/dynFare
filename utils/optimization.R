rm(list = ls())

nstations <- 4
routes <- get_routes(nstations, 60, c(0.3, 0.4, 0.5))
model <- 1
beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- matrix(250, 2, 6)
time_bp <- 0.5
nseats <- 10


simulate_train_demand(routes, model, beta, price, time_bp, nseats, keep_all = T)
