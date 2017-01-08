beta <- matrix(rep(c(18,-4,4, 1.2), each = 6), nrow = 6)
price <- matrix(c(250, 350), 2, 6)
time_bp <- 0.8
ntrains <- 500
nstations <- 4
nseats <- 200
routes <- get_routes(nstations, 60, c(0.3, 0.4, 0.5))

data <- generate_data(beta, price, time_bp, ntrains, nstations, nseats)

ticket_price <- expand.grid(train = 1:ntrains, route = 1:nrow(routes), valid_from = c(0, time_bp)) %>% 
  mutate(valid_to = pmin(valid_from + 0.8, 1), price = 250 + 100 * valid_from / 0.8)


estimate_intensity(data, ticket_price, routes, nseats)
